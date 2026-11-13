{- | The server from the outside: every request goes through the WAI
application, against a real PostgreSQL that exists for this run. The
steps follow one another the way a client's first session does: find
out what the server is, register, sign in with PKCE, upload, post, read
the timelines, follow, like, bookmark, comment, read the notifications,
delete, give the token back.
-}
module Test.Flow (
  tests,
) where

import Control.Monad (forM, forM_)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Network.HTTP.Types (Header, Method, hAuthorization, hContentType, methodDelete, methodGet, methodPost, methodPut, parseSimpleQuery, renderSimpleQuery, statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest (..), SResponse (..), defaultRequest, runSession, setPath, srequest)
import Pellucid.Config (Config (..), Env (..))
import Pellucid.Domain.Ids (AccountId (..), parseId)
import Pellucid.Domain.Limits (defaultLimits)
import Pellucid.Domain.Username (mkUsername)
import Pellucid.Http.App (application, routeTable)
import Pellucid.Http.Router (Dispatch (..), dispatch, pathOf)
import Pellucid.Secret (hashPassword)
import Pellucid.Store (closeDb, migrate, openDb, withConnection)
import Pellucid.Store.Accounts (createLocalAccount)
import Pellucid.Store.Notifications (unreadCount)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Scrub (sampleJpeg, secrets)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCaseSteps)
import Test.TempPostgres (withTempPostgres)

tests :: TestTree
tests = testCaseSteps "a first session, end to end" \step ->
  withTempPostgres \url -> withSystemTempDirectory "pellucid-media" \mediaDir -> do
    db <- openDb url
    first <- migrate db
    again <- migrate db
    assertEqual "the schema is applied once" (2, 0) (length first, length again)
    decoy <- hashPassword "a password nobody has"
    let
      config =
        Config
          { baseUrl = "https://pellucid.test"
          , title = "Test Server"
          , databaseUrl = url
          , mediaDir
          , port = 0
          , limits = defaultLimits
          }
      app = application Env {config, db, clock = getCurrentTime, decoyPasswordHash = decoy}
    forM_ ["alice", "bob", "carol"] \name -> do
      hash <- hashPassword (name <> " has a long password")
      username <- either (const (assertFailure "a bad test username")) pure (mkUsername name)
      created <- withConnection db \conn -> createLocalAccount conn username hash False
      assertBool "the account is created" (isJust created)

    step "the server says what it is"
    nodeinfo <- call app methodGet "/api/nodeinfo/2.0.json" [] ""
    assertEqual "nodeinfo answers" 200 nodeinfo.status
    assertEqual "it gives its own name" (Just (String "pellucid")) (at ["software", "name"] nodeinfo)
    assertEqual "it counts its accounts" (Just (Number 3)) (at ["usage", "users", "total"] nodeinfo)
    settings <- call app methodGet "/api/v1.1/compose/settings" [] ""
    assertEqual "it accepts only what it can scrub" (Just (Aeson.toJSON ["image/jpeg" :: Text, "image/png"])) (at ["allowed_media_types"] settings)

    step "the server's own address, opened in a browser"
    front <- call app methodGet "/" [] ""
    assertEqual "a page is shown" (200, Just "text/html; charset=utf-8") (front.status, lookup hContentType front.headers)
    assertBool "that gives the address the app takes" ("<code>https://pellucid.test</code>" `BS.isInfixOf` BL.toStrict front.body)
    assertBool "and nothing that can run" (isJust (lookup "Content-Security-Policy" front.headers) && not ("<script" `BS.isInfixOf` BL.toStrict front.body))

    step "a path that is not there, and a method a path does not answer"
    missing <- call app methodGet "/api/v1/nothing" [] ""
    assertEqual "a missing page" (404, Just (String "not_found")) (missing.status, at ["error_code"] missing)
    wrongMethod <- call app methodDelete "/api/v1/apps" [] ""
    assertEqual "a wrong method" 405 wrongMethod.status
    assertEqual "with the methods that work" (Just "POST") (lookup "Allow" wrongMethod.headers)

    step "registering a client"
    refusedApp <-
      call
        app
        methodPost
        "/api/v1/apps"
        form
        (formBody [("client_name", ""), ("redirect_uris", "http://elsewhere.example/cb javascript:alert(1)"), ("scopes", "read admin:write")])
    assertEqual "a bad registration is refused" 422 refusedApp.status
    assertBool "naming each field that is wrong" (all (`elem` fieldsOf refusedApp) ["scopes", "client_name", "redirect_uris"])
    registered <- call app methodPost "/api/v1/apps" form (formBody [("client_name", "Test <App>"), ("redirect_uris", redirect), ("scopes", "read write follow push")])
    assertEqual "a good registration is accepted" 200 registered.status
    let
      clientId = text ["client_id"] registered
      clientSecret = text ["client_secret"] registered
    assertBool "a confidential client is given a secret" (T.length clientSecret == 43)
    jsonApp <- call app methodPost "/api/v1/apps" jsonType "{\"client_name\":\"Json App\",\"redirect_uris\":\"pellucid://callback\",\"token_endpoint_auth_method\":\"none\"}"
    assertEqual "a JSON body is read like a form" 200 jsonApp.status
    assertEqual "a public client is given no secret" (Just Null) (at ["client_secret"] jsonApp)
    let publicId = text ["client_id"] jsonApp

    step "the consent page"
    let ask extra =
          [ ("response_type", "code")
          , ("client_id", clientId)
          , ("redirect_uri", redirect)
          , ("scope", "read write follow")
          , ("state", "st&te")
          , ("code_challenge", challenge)
          , ("code_challenge_method", "S256")
          ]
            <> extra
    consent <- call app methodGet ("/oauth/authorize?" <> query (ask [])) [] ""
    assertEqual "the page is shown" 200 consent.status
    assertBool "with the client's name escaped" ("Test &lt;App&gt;" `BS.isInfixOf` BL.toStrict consent.body)
    assertBool "and nothing that can run" (isJust (lookup "Content-Security-Policy" consent.headers) && not ("<script" `BS.isInfixOf` BL.toStrict consent.body))
    foreign' <- call app methodGet ("/oauth/authorize?" <> query [("response_type", "code"), ("client_id", clientId), ("redirect_uri", "https://evil.example/cb")]) [] ""
    assertEqual "a redirect that is not the client's is answered with a page" 400 foreign'.status
    assertBool "and nothing is sent there" (isNothing (lookup "Location" foreign'.headers))
    noPkce <- call app methodGet ("/oauth/authorize?" <> query [("response_type", "code"), ("client_id", publicId), ("redirect_uri", "pellucid://callback")]) [] ""
    assertEqual "a public client without a challenge is refused" 400 noPkce.status
    tooMuch <-
      call
        app
        methodGet
        ( "/oauth/authorize?"
            <> query
              [ ("response_type", "code")
              , ("client_id", publicId)
              , ("redirect_uri", "pellucid://callback")
              , ("scope", "read write")
              , ("code_challenge", challenge)
              , ("code_challenge_method", "S256")
              ]
        )
        []
        ""
    assertEqual "a client is not offered a scope it did not register" 400 tooMuch.status

    step "signing in"
    wrong <- call app methodPost "/oauth/authorize" form (formBody (ask [("username", "alice"), ("password", "not her password")]))
    assertEqual "a wrong password shows the page again" 401 wrong.status
    assertBool "and sends nobody anywhere" (isNothing (lookup "Location" wrong.headers))
    nobody <- call app methodPost "/oauth/authorize" form (formBody (ask [("username", "carol"), ("password", "alice has a long password")]))
    assertEqual "an unknown name is answered the same way" wrong.status nobody.status
    forM_ [wrong, nobody] \reply -> assertBool "with the same sentence" ("do not match an account here" `BS.isInfixOf` BL.toStrict reply.body)
    denied <- call app methodPost "/oauth/authorize" form (formBody (ask [("decision", "deny")]))
    assertEqual "declining goes back to the client" (302, Just "access_denied") (denied.status, lookup "error" (locationQuery denied))
    code <- authorizeAs app (ask []) "Alice" "alice has a long password"

    step "the token"
    let tokenForm verifierText secretText codeText =
          formBody
            [ ("grant_type", "authorization_code")
            , ("code", codeText)
            , ("client_id", clientId)
            , ("client_secret", secretText)
            , ("redirect_uri", redirect)
            , ("code_verifier", verifierText)
            ]
    badVerifier <- call app methodPost "/oauth/token" form (tokenForm (T.replicate 43 "a") clientSecret code)
    assertEqual "a wrong verifier is refused" (400, Just (String "invalid_grant")) (badVerifier.status, at ["error"] badVerifier)
    badSecret <- call app methodPost "/oauth/token" form (tokenForm verifier "not the secret" code)
    assertEqual "a wrong secret is refused" (401, Just (String "invalid_client")) (badSecret.status, at ["error"] badSecret)
    issued <- call app methodPost "/oauth/token" form (tokenForm verifier clientSecret code)
    assertEqual "the right verifier and secret get a token" 200 issued.status
    assertEqual "of the granted scopes" (Just (String "read write follow")) (at ["scope"] issued)
    let firstToken = text ["access_token"] issued
    me <- call app methodGet "/api/v1/accounts/verify_credentials" (bearer firstToken) ""
    assertEqual "the token speaks for alice" (200, Just (String "alice")) (me.status, at ["username"] me)
    replay <- call app methodPost "/oauth/token" form (tokenForm verifier clientSecret code)
    assertEqual "the code cannot be used twice" 400 replay.status
    afterReplay <- call app methodGet "/api/v1/accounts/verify_credentials" (bearer firstToken) ""
    assertEqual "and using it twice revokes the token it produced" 401 afterReplay.status
    assertEqual "an open route refuses a dead token too" 401 . (.status) =<< call app methodGet "/api/v1/timelines/public" (bearer firstToken) ""

    step "two sessions"
    alice <- signIn app clientId clientSecret "alice" "read write follow"
    bob <- signIn app clientId clientSecret "bob" "read write follow"
    carol <- signIn app clientId clientSecret "carol" "read write follow"
    readOnly <- signIn app clientId clientSecret "alice" "read"
    aliceAccount <- call app methodGet "/api/v1/accounts/verify_credentials" (bearer alice) ""
    let aliceId = text ["id"] aliceAccount
    assertEqual "a flag is a JSON boolean and a count is a JSON number" (Just (Bool False), Just (Number 0)) (at ["locked"] aliceAccount, at ["followers_count"] aliceAccount)
    assertEqual "a route that needs a token says so" 401 . (.status) =<< call app methodGet "/api/v1/timelines/home" [] ""

    step "uploading"
    notAnImage <- call app methodPost "/api/v2/media" (multipartType <> bearer alice) (multipart "file" "page.jpg" "image/jpeg" "<html><script>alert(1)</script></html>")
    assertEqual "a file is what its content is, whatever it is called" (422, ["file"]) (notAnImage.status, fieldsOf notAnImage)
    assertEqual "an upload needs the write scope" 403 . (.status)
      =<< call app methodPost "/api/v2/media" (multipartType <> bearer readOnly) (multipart "file" "a.jpg" "image/jpeg" sampleJpeg)
    uploads <- forM [1 .. 6 :: Int] \_ -> do
      uploaded <- call app methodPost "/api/v2/media" (multipartType <> bearer alice) (multipart "file" "holiday.jpg" "application/octet-stream" sampleJpeg)
      assertEqual "a JPEG is accepted" 200 uploaded.status
      pure uploaded
    let
      upload = fromMaybe (error "no upload") (safeHead uploads)
      mediaIds = map (text ["id"]) uploads
      mediaUrl = text ["url"] upload
    assertEqual "typed by its content" (Just (String "image/jpeg")) (at ["mime"] upload)
    assertEqual "with the size it is displayed at" (Just (Number 16), Just (Number 32)) (at ["meta", "original", "width"] upload, at ["meta", "original", "height"] upload)
    assertEqual "is_nsfw is a boolean" (Just (Bool False)) (at ["is_nsfw"] upload)
    let stored = mediaDir </> T.unpack (T.take 2 (keyOf mediaUrl)) </> T.unpack (keyOf mediaUrl) <> ".jpg"
    onDisk <- BS.readFile stored
    forM_ secrets \secret -> assertBool ("what is stored does not hold " <> show secret) (not (secret `BS.isInfixOf` onDisk))
    served <- call app methodGet (TE.encodeUtf8 (pathOfUrl mediaUrl)) [] ""
    assertEqual "what is served is what is stored" (200, onDisk) (served.status, BL.toStrict served.body)
    assertEqual "typed, and not to be reinterpreted" (Just "image/jpeg", Just "nosniff") (lookup hContentType served.headers, lookup "X-Content-Type-Options" served.headers)
    described <- call app methodPut ("/api/v1/media/" <> TE.encodeUtf8 (text ["id"] upload)) (form <> bearer alice) (formBody [("description", "A harbour at dusk")])
    assertEqual "alt text can be added" (Just (String "A harbour at dusk")) (at ["description"] described)
    assertEqual "someone else's upload is a missing page" 404 . (.status)
      =<< call app methodPut ("/api/v1/media/" <> TE.encodeUtf8 (text ["id"] upload)) (form <> bearer bob) (formBody [("description", "mine now")])

    step "posting"
    noMedia <- call app methodPost "/api/v1/statuses" (form <> bearer alice) (formBody [("status", "words only")])
    assertEqual "a post needs a photo" (422, ["media_ids"]) (noMedia.status, fieldsOf noMedia)
    notHers <- call app methodPost "/api/v1/statuses" (form <> bearer bob) (formBody [("status", "borrowed"), ("media_ids[]", text ["id"] upload)])
    assertEqual "an upload can only go on its owner's post" 422 notHers.status
    readOnlyPost <- call app methodPost "/api/v1/statuses" (form <> bearer readOnly) (formBody [("status", "x"), ("media_ids[]", text ["id"] upload)])
    assertEqual "a read token cannot post" (403, Just (String "insufficient_scope")) (readOnlyPost.status, at ["error_code"] readOnlyPost)
    posts <- forM (zip [1 :: Int ..] (take 3 mediaIds)) \(n, mediaId) -> do
      posted <-
        call
          app
          methodPost
          "/api/v1/statuses"
          (jsonType <> bearer alice)
          (BL.toStrict (Aeson.encode (Aeson.object ["status" Aeson..= ("Photo " <> T.pack (show n) <> " <b>&</b>"), "media_ids" Aeson..= [mediaId]])))
      assertEqual "a post with a photo is published" 200 posted.status
      pure posted
    let firstPost = fromMaybe (error "no post") (safeHead posts)
    assertEqual "as a photo" (Just (String "photo")) (at ["pf_type"] firstPost)
    assertEqual "its caption is text" (Just (String "Photo 1 <b>&</b>")) (at ["content_text"] firstPost)
    assertEqual "and never markup" (Just (String "<p>Photo 1 &lt;b&gt;&amp;&lt;/b&gt;</p>")) (at ["content"] firstPost)
    assertEqual "with its attachment" (Just (String mediaUrl)) (at ["media_attachments", "0", "url"] firstPost)
    reused <- call app methodPost "/api/v1/statuses" (form <> bearer alice) (formBody [("status", "again"), ("media_ids[]", text ["id"] upload)])
    assertEqual "an upload goes on one post, once" 422 reused.status
    private <-
      call
        app
        methodPost
        "/api/v1/statuses"
        (form <> bearer alice)
        (formBody [("status", "for followers"), ("visibility", "private"), ("media_ids[]", fromMaybe "" (safeHead (drop 3 mediaIds)))])
    assertEqual "a followers-only post is published" (200, Just (String "private")) (private.status, at ["visibility"] private)
    let privateId = text ["id"] private

    step "the timelines"
    homePage <- call app methodGet "/api/v1/timelines/home?limit=3" (bearer alice) ""
    assertEqual "home is newest first, the author's own posts included" ["for followers", "Photo 3 <b>&</b>", "Photo 2 <b>&</b>"] (captions homePage)
    next <- maybe (assertFailure "the first page has no Link header") pure (nextLink homePage)
    assertBool "the link is absolute, on this server, under /api/" ("https://pellucid.test/api/v1/timelines/home?limit=3&cursor=" `T.isPrefixOf` next)
    secondPage <- call app methodGet (TE.encodeUtf8 (pathOfUrl next)) (bearer alice) ""
    assertEqual "the link leads to the rest" ["Photo 1 <b>&</b>"] (captions secondPage)
    assertBool "and the list ends without another link" (isNothing (nextLink secondPage))
    exact <- call app methodGet "/api/v1/timelines/home?limit=4" (bearer alice) ""
    assertBool "a list that ends on a page boundary carries no link" (length (captions exact) == 4 && isNothing (nextLink exact))
    byMaxId <- call app methodGet ("/api/v1/timelines/home?max_id=" <> TE.encodeUtf8 privateId) (bearer alice) ""
    assertEqual "max_id is read as a cursor" 3 (length (captions byMaxId))
    badCursor <- call app methodGet "/api/v1/timelines/home?cursor=nonsense" (bearer alice) ""
    assertEqual "a cursor this server did not issue is refused" (422, ["cursor"]) (badCursor.status, fieldsOf badCursor)
    publicPage <- call app methodGet "/api/v1/timelines/public" [] ""
    assertEqual "the public timeline holds public posts only, for anyone" ["Photo 3 <b>&</b>", "Photo 2 <b>&</b>", "Photo 1 <b>&</b>"] (captions publicPage)
    bobHome <- call app methodGet "/api/v1/timelines/home" (bearer bob) ""
    assertEqual "nothing is inserted into a feed" [] (captions bobHome)

    step "following, and who may read what"
    assertEqual "a stranger cannot read a followers-only post" 404 . (.status) =<< call app methodGet ("/api/v1/statuses/" <> TE.encodeUtf8 privateId) (bearer bob) ""
    assertEqual "nor can nobody" 404 . (.status) =<< call app methodGet ("/api/v1/statuses/" <> TE.encodeUtf8 privateId) [] ""
    profileBefore <- call app methodGet ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/statuses") (bearer bob) ""
    assertEqual "and it is not in the author's list for them" 3 (length (captions profileBefore))
    selfFollow <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/follow") (bearer alice) ""
    assertEqual "nobody follows themselves" 422 selfFollow.status
    followed <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/follow") (bearer bob) ""
    assertEqual "an open account is followed at once" (200, Just (Bool True), Just (Bool False)) (followed.status, at ["following"] followed, at ["requested"] followed)
    followedAgain <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/follow") (bearer bob) ""
    assertEqual "asking again changes nothing" (200, Just (Bool True)) (followedAgain.status, at ["following"] followedAgain)
    aliceNow <- call app methodGet ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId) [] ""
    assertEqual "one follower is counted, once" (Just (Number 1)) (at ["followers_count"] aliceNow)
    relation <- call app methodGet ("/api/v1/accounts/relationships?id[]=" <> TE.encodeUtf8 aliceId) (bearer bob) ""
    assertEqual "the relationship says so" (Just (Bool True)) (at ["0", "following"] relation)
    assertEqual "a follower can read it" 200 . (.status) =<< call app methodGet ("/api/v1/statuses/" <> TE.encodeUtf8 privateId) (bearer bob) ""
    bobHomeNow <- call app methodGet "/api/v1/timelines/home" (bearer bob) ""
    assertEqual "and has it at home, with the rest" 4 (length (captions bobHomeNow))
    unfollowed <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/unfollow") (bearer bob) ""
    assertEqual "unfollowing ends it" (Just (Bool False)) (at ["following"] unfollowed)
    assertEqual "and the post is out of reach again" 404 . (.status) =<< call app methodGet ("/api/v1/statuses/" <> TE.encodeUtf8 privateId) (bearer bob) ""

    let
      status path = "/api/v1/statuses/" <> TE.encodeUtf8 path
      secondId = maybe "" (text ["id"]) (safeHead (drop 1 posts))
      comment who wording parent = call app methodPost "/api/v1/statuses" (form <> bearer who) (formBody [("status", wording), ("in_reply_to_id", parent)])
      alertsOf who = alerts <$> call app methodGet "/api/v1/notifications" (bearer who) ""
      unreadOf accountId = maybe (pure 0) (\n -> withConnection db \conn -> unreadCount conn (AccountId n)) (parseId accountId)
    bobId <- text ["id"] <$> call app methodGet "/api/v1/accounts/verify_credentials" (bearer bob) ""

    step "likes"
    assertEqual "an unfollow took the notice of the follow with it" [] =<< alertsOf alice
    liked <- call app methodPost (status secondId <> "/favourite") (bearer bob) ""
    assertEqual
      "a like is answered with the post as it now is"
      (200, Just (Bool True), Just (Number 1))
      (liked.status, at ["favourited"] liked, at ["favourites_count"] liked)
    assertEqual "which names who likes it" (Just (String "bob"), Just (Bool False)) (at ["liked_by", "username"] liked, at ["liked_by", "others"] liked)
    likedAgain <- call app methodPost (status secondId <> "/favourite") (bearer bob) ""
    assertEqual "liking twice is one like" (200, Just (Number 1)) (likedAgain.status, at ["favourites_count"] likedAgain)
    ownLike <- call app methodPost (status secondId <> "/favourite") (bearer alice) ""
    assertEqual
      "an author can like her own post"
      (Just (Number 2), Just (Bool True), Just (Number 2))
      (at ["favourites_count"] ownLike, at ["liked_by", "others"] ownLike, at ["liked_by", "total_count"] ownLike)
    seenByNobody <- call app methodGet (status secondId) [] ""
    assertEqual "a like belongs to its reader" (Just (Bool False), Just (Number 2)) (at ["favourited"] seenByNobody, at ["favourites_count"] seenByNobody)
    assertEqual "a post that cannot be read cannot be liked" 404 . (.status) =<< call app methodPost (status privateId <> "/favourite") (bearer bob) ""
    assertEqual "nor one that is not there" 404 . (.status) =<< call app methodPost (status "1" <> "/favourite") (bearer bob) ""
    assertEqual "a read token cannot like" 403 . (.status) =<< call app methodPost (status secondId <> "/favourite") (bearer readOnly) ""
    likers <- call app methodGet (status secondId <> "/favourited_by?limit=1") [] ""
    assertEqual "who likes a post, newest like first" ["alice"] (usernames likers)
    moreLikers <- maybe (assertFailure "the list of likers has no Link header") (\link -> call app methodGet (TE.encodeUtf8 (pathOfUrl link)) [] "") (nextLink likers)
    assertEqual "and the link leads to the rest" (["bob"], Nothing) (usernames moreLikers, nextLink moreLikers)
    assertEqual "the posts an account likes" ["Photo 2 <b>&</b>"] . captions =<< call app methodGet "/api/v1/favourites" (bearer bob) ""
    assertEqual "the author has one thing she has not been shown" 1 =<< unreadOf aliceId
    assertEqual "she is told of the like, and not of her own" [("favourite", "bob", secondId)] =<< alertsOf alice
    assertEqual "and reading the page is what marks it seen" 0 =<< unreadOf aliceId
    unliked <- call app methodPost (status secondId <> "/unfavourite") (bearer bob) ""
    assertEqual "a like can be taken back" (200, Just (Bool False), Just (Number 1)) (unliked.status, at ["favourited"] unliked, at ["favourites_count"] unliked)
    assertEqual "taking back what is not there changes nothing" 200 . (.status) =<< call app methodPost (status secondId <> "/unfavourite") (bearer bob) ""
    assertEqual "the notice of a like goes with the like" [] =<< alertsOf alice
    assertEqual "and so does its place in the list" [] . captions =<< call app methodGet "/api/v1/favourites" (bearer bob) ""
    _ <- call app methodPost (status secondId <> "/favourite") (bearer bob) ""

    step "bookmarks"
    marked <- call app methodPost (status secondId <> "/bookmark") (bearer bob) ""
    assertEqual "a bookmark is answered with the post" (200, Just (Bool True)) (marked.status, at ["bookmarked"] marked)
    assertEqual "and is nobody else's" (Just (Bool False)) . at ["bookmarked"] =<< call app methodGet (status secondId) (bearer alice) ""
    assertEqual "the posts an account has bookmarked" ["Photo 2 <b>&</b>"] . captions =<< call app methodGet "/api/v1/bookmarks" (bearer bob) ""
    assertEqual "a bookmark tells nobody" [("favourite", "bob", secondId)] =<< alertsOf alice
    assertEqual "a post that cannot be read cannot be bookmarked" 404 . (.status) =<< call app methodPost (status privateId <> "/bookmark") (bearer bob) ""
    unmarked <- call app methodPost (status secondId <> "/unbookmark") (bearer bob) ""
    assertEqual "a bookmark can be taken back" (200, Just (Bool False)) (unmarked.status, at ["bookmarked"] unmarked)
    assertEqual "and the list is empty again" [] . captions =<< call app methodGet "/api/v1/bookmarks" (bearer bob) ""

    step "comments"
    said <- comment bob "Lovely light" secondId
    assertEqual "a comment needs no photo" (200, Just (String "text")) (said.status, at ["pf_type"] said)
    assertEqual "and names what it answers and whom" (Just (String secondId), Just (String aliceId)) (at ["in_reply_to_id"] said, at ["in_reply_to_account_id"] said)
    let commentId = text ["id"] said
    blank <- comment bob "   " secondId
    assertEqual "a comment needs some text" (422, ["status"]) (blank.status, fieldsOf blank)
    withPhoto <- call app methodPost "/api/v1/statuses" (form <> bearer bob) (formBody [("status", "look"), ("in_reply_to_id", secondId), ("media_ids[]", "5")])
    assertEqual "and carries no attachment" (422, ["media_ids"]) (withPhoto.status, fieldsOf withPhoto)
    outOfReach <- comment bob "hello?" privateId
    notThere <- comment bob "hello?" "1"
    assertEqual "a post that cannot be read cannot be commented on" (422, ["in_reply_to_id"]) (outOfReach.status, fieldsOf outOfReach)
    assertEqual "and is answered like one that is not there" (notThere.status, notThere.body) (outOfReach.status, outOfReach.body)
    assertEqual "the post counts its comments" (Just (Number 1)) . at ["reply_count"] =<< call app methodGet (status secondId) [] ""
    thread <- call app methodGet (status secondId <> "/context") [] ""
    assertEqual "and its thread holds them" ([], ["Lovely light"]) (captionsAt "ancestors" thread, captionsAt "descendants" thread)
    above <- call app methodGet (status commentId <> "/context") (bearer carol) ""
    assertEqual "a comment's thread leads back to the post" (["Photo 2 <b>&</b>"], []) (captionsAt "ancestors" above, captionsAt "descendants" above)
    answer <- comment alice "Thank you" commentId
    assertEqual "a comment can be answered" (200, Just (String bobId)) (answer.status, at ["in_reply_to_account_id"] answer)
    deeper <- call app methodGet (status secondId <> "/context") [] ""
    assertEqual "and the thread is read oldest first, at any depth" ["Lovely light", "Thank you"] (captionsAt "descendants" deeper)
    assertEqual "a comment is in no timeline" ["Photo 3 <b>&</b>", "Photo 2 <b>&</b>", "Photo 1 <b>&</b>"] . captions =<< call app methodGet "/api/v1/timelines/public" [] ""
    assertEqual "its author's own home included" ["for followers", "Photo 3 <b>&</b>", "Photo 2 <b>&</b>", "Photo 1 <b>&</b>"] . captions
      =<< call app methodGet "/api/v1/timelines/home" (bearer alice) ""
    assertEqual "and not among its author's posts" [] . captions =<< call app methodGet ("/api/v1/accounts/" <> TE.encodeUtf8 bobId <> "/statuses") [] ""
    assertEqual "nor counted as one" (Just (Number 0)) . at ["statuses_count"] =<< call app methodGet ("/api/v1/accounts/" <> TE.encodeUtf8 bobId) [] ""
    assertEqual "the author is told of the comment, with the comment" [("comment", "bob", commentId), ("favourite", "bob", secondId)] =<< alertsOf alice
    assertEqual "and the commenter of the answer" [("comment", "alice", text ["id"] answer)] =<< alertsOf bob
    quiet <-
      call
        app
        methodPost
        "/api/v1/statuses"
        (form <> bearer alice)
        (formBody [("status", "No comments, please"), ("comments_disabled", "true"), ("media_ids[]", fromMaybe "" (safeHead (drop 5 mediaIds)))])
    assertEqual "a post can turn comments off" (200, Just (Bool True)) (quiet.status, at ["comments_disabled"] quiet)
    silenced <- comment bob "but" (text ["id"] quiet)
    assertEqual "and then takes none" (422, ["in_reply_to_id"]) (silenced.status, fieldsOf silenced)

    step "a thread under a followers-only post"
    _ <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/follow") (bearer bob) ""
    assertEqual "a follow tells the one followed" (Just ("follow", "bob", "")) . safeHead =<< alertsOf alice
    whispered <- comment bob "Seen it" privateId
    assertEqual "a comment takes its post's visibility" (200, Just (String "private")) (whispered.status, at ["visibility"] whispered)
    let whisperId = text ["id"] whispered
    assertEqual "a stranger cannot read a comment under a post she cannot read" 404 . (.status) =<< call app methodGet (status whisperId) (bearer carol) ""
    assertEqual "nor can nobody" 404 . (.status) =<< call app methodGet (status whisperId) [] ""
    assertEqual "the post's author can" 200 . (.status) =<< call app methodGet (status whisperId) (bearer alice) ""
    assertEqual "a follower can like the post" 200 . (.status) =<< call app methodPost (status privateId <> "/favourite") (bearer bob) ""
    assertEqual "a stranger cannot" 404 . (.status) =<< call app methodPost (status privateId <> "/favourite") (bearer carol) ""
    assertEqual "nor see who does" 404 . (.status) =<< call app methodGet (status privateId <> "/favourited_by") (bearer carol) ""
    assertEqual "the follower's likes hold both" ["for followers", "Photo 2 <b>&</b>"] . captions =<< call app methodGet "/api/v1/favourites" (bearer bob) ""
    _ <- call app methodPost ("/api/v1/accounts/" <> TE.encodeUtf8 aliceId <> "/unfollow") (bearer bob) ""
    assertEqual "after unfollowing, a list holds only what can still be read" ["Photo 2 <b>&</b>"] . captions =<< call app methodGet "/api/v1/favourites" (bearer bob) ""
    assertEqual "the commenter still reads his own comment" 200 . (.status) =<< call app methodGet (status whisperId) (bearer bob) ""
    alone <- call app methodGet (status whisperId <> "/context") (bearer bob) ""
    assertEqual "and nothing of the thread around it" ([], []) (captionsAt "ancestors" alone, captionsAt "descendants" alone)
    assertEqual "the author is told of both, and the follow's notice is gone" [("favourite", "bob", privateId), ("comment", "bob", whisperId)] . take 2 =<< alertsOf alice

    step "deleting what was liked and commented on"
    assertEqual "only its author can delete a comment" 404 . (.status) =<< call app methodDelete (status commentId) (bearer alice) ""
    assertEqual "its author can" 200 . (.status) =<< call app methodDelete (status commentId) (bearer bob) ""
    assertEqual "the post counts one comment fewer" (Just (Number 0)) . at ["reply_count"] =<< call app methodGet (status secondId) [] ""
    assertEqual "what answered the comment is still in the thread, where its author can find it" ["Thank you"] . captionsAt "descendants"
      =<< call app methodGet (status secondId <> "/context") [] ""
    assertEqual "and the notice of the comment went with it" [("favourite", "bob", privateId), ("comment", "bob", whisperId), ("favourite", "bob", secondId)]
      =<< alertsOf alice
    assertEqual "a deleted post takes its likes with it" 200 . (.status) =<< call app methodDelete (status secondId) (bearer alice) ""
    assertEqual "out of the liker's list" [] . captions =<< call app methodGet "/api/v1/favourites" (bearer bob) ""
    assertEqual "and out of the author's notifications" [("favourite", "bob", privateId), ("comment", "bob", whisperId)] =<< alertsOf alice

    step "deleting"
    let firstId = text ["id"] firstPost
    assertEqual "only the author can delete a post" 404 . (.status) =<< call app methodDelete ("/api/v1/statuses/" <> TE.encodeUtf8 firstId) (bearer bob) ""
    assertEqual "an upload on a post cannot be taken back" 409 . (.status)
      =<< call app methodDelete ("/api/v1/media/" <> TE.encodeUtf8 (text ["id"] upload)) (bearer alice) ""
    deleted <- call app methodDelete ("/api/v1/statuses/" <> TE.encodeUtf8 firstId) (bearer alice) ""
    assertEqual "the author can, and is answered with the post as it was" (200, Just (String firstId)) (deleted.status, at ["id"] deleted)
    assertEqual "the post is gone" 404 . (.status) =<< call app methodGet ("/api/v1/statuses/" <> TE.encodeUtf8 firstId) (bearer alice) ""
    assertEqual "its file is gone from disk" False =<< doesFileExist stored
    assertEqual "and is no longer served" 404 . (.status) =<< call app methodGet (TE.encodeUtf8 (pathOfUrl mediaUrl)) [] ""
    let spare = fromMaybe "" (safeHead (drop 4 mediaIds))
    takenBack <- call app methodDelete ("/api/v1/media/" <> TE.encodeUtf8 spare) (bearer alice) ""
    assertEqual "an upload that is on no post can be taken back" 200 takenBack.status
    assertEqual "once" 409 . (.status) =<< call app methodDelete ("/api/v1/media/" <> TE.encodeUtf8 spare) (bearer alice) ""

    step "giving the token back"
    revoked <- call app methodPost "/oauth/revoke" form (formBody [("token", bob)])
    assertEqual "revoking answers 200" 200 revoked.status
    assertEqual "and the token is dead" 401 . (.status) =<< call app methodGet "/api/v1/accounts/verify_credentials" (bearer bob) ""
    assertEqual "revoking what does not exist answers the same" 200 . (.status) =<< call app methodPost "/oauth/revoke" form (formBody [("token", "never issued")])

    step "the route table answers what the client's first session calls"
    let table = routeTable defaultLimits
    forM_ clientRoutes \(method, path) -> case dispatch table method (pathOf path) of
      Found _ _ -> pure ()
      _ -> assertFailure (T.unpack (TE.decodeUtf8 method <> " " <> path <> " has no route"))
    closeDb db
  where
    redirect = "http://127.0.0.1:5555/callback"
    verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

    -- Sign in through the consent form and return the code from the
    -- redirect, checking that the state came back untouched.
    authorizeAs app fields username password = do
      granted <- call app methodPost "/oauth/authorize" form (formBody (fields <> [("username", username), ("password", password)]))
      assertEqual "the right password is sent back to the client" 302 granted.status
      let back = locationQuery granted
      assertEqual "with the state untouched" (lookup "state" fields) (lookup "state" back)
      assertBool "to the registered redirect" (maybe False ((redirect <> "?") `T.isPrefixOf`) (TE.decodeUtf8 <$> lookup "Location" granted.headers))
      maybe (assertFailure "the redirect carries no code") pure (lookup "code" back)

    signIn app clientId clientSecret username scope = do
      let fields = [("response_type", "code"), ("client_id", clientId), ("redirect_uri", redirect), ("scope", scope), ("code_challenge", challenge), ("code_challenge_method", "S256")]
      code <- authorizeAs app fields username (username <> " has a long password")
      issued <-
        call
          app
          methodPost
          "/oauth/token"
          form
          ( formBody
              [("grant_type", "authorization_code"), ("code", code), ("client_id", clientId), ("client_secret", clientSecret), ("redirect_uri", redirect), ("code_verifier", verifier)]
          )
      assertEqual "a token is issued" 200 issued.status
      pure (text ["access_token"] issued)

{- | What the Pellucid client calls to sign in, post a photo, read it back,
like, bookmark and comment on it, and read its notifications, from its
spec's contract PixelfedApi.
-}
clientRoutes :: [(Method, Text)]
clientRoutes =
  [ (methodGet, "/api/nodeinfo/2.0.json")
  , (methodPost, "/api/v1/apps")
  , (methodGet, "/oauth/authorize")
  , (methodPost, "/oauth/token")
  , (methodGet, "/api/v1/accounts/verify_credentials")
  , (methodGet, "/api/v1.1/compose/settings")
  , (methodPost, "/api/v2/media")
  , (methodPut, "/api/v1/media/5")
  , (methodPost, "/api/v1/statuses")
  , (methodGet, "/api/v1/statuses/5")
  , (methodGet, "/api/v1/statuses/5/context")
  , (methodDelete, "/api/v1/statuses/5")
  , (methodGet, "/api/v1/timelines/home")
  , (methodGet, "/api/v1/timelines/public")
  , (methodGet, "/api/v1/accounts/5")
  , (methodGet, "/api/v1/accounts/5/statuses")
  , (methodGet, "/api/v1/accounts/relationships")
  , (methodPost, "/api/v1/accounts/5/follow")
  , (methodPost, "/api/v1/accounts/5/unfollow")
  , (methodPost, "/api/v1/statuses/5/favourite")
  , (methodPost, "/api/v1/statuses/5/unfavourite")
  , (methodGet, "/api/v1/statuses/5/favourited_by")
  , (methodGet, "/api/v1/favourites")
  , (methodPost, "/api/v1/statuses/5/bookmark")
  , (methodPost, "/api/v1/statuses/5/unbookmark")
  , (methodGet, "/api/v1/bookmarks")
  , (methodGet, "/api/v1/notifications")
  ]

-- Requests and answers --------------------------------------------------------

data Reply = Reply
  { status :: Int
  , headers :: [Header]
  , body :: BL.ByteString
  }

call :: Wai.Application -> Method -> ByteString -> [Header] -> ByteString -> IO Reply
call app method path requestHeaders requestBody = do
  let request = (setPath defaultRequest path) {Wai.requestMethod = method, Wai.requestHeaders = requestHeaders}
  response <- runSession (srequest (SRequest request (BL.fromStrict requestBody))) app
  pure Reply {status = statusCode (simpleStatus response), headers = simpleHeaders response, body = simpleBody response}

form, jsonType, multipartType :: [Header]
form = [(hContentType, "application/x-www-form-urlencoded")]
jsonType = [(hContentType, "application/json")]
multipartType = [(hContentType, "multipart/form-data; boundary=" <> boundary)]

bearer :: Text -> [Header]
bearer secret = [(hAuthorization, "Bearer " <> TE.encodeUtf8 secret)]

formBody :: [(Text, Text)] -> ByteString
formBody = query

query :: [(Text, Text)] -> ByteString
query pairs = renderSimpleQuery False [(TE.encodeUtf8 k, TE.encodeUtf8 v) | (k, v) <- pairs]

boundary :: ByteString
boundary = "pellucid-test-boundary"

multipart :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
multipart name fileName contentType content =
  BS.concat
    [ "--" <> boundary <> "\r\n"
    , "Content-Disposition: form-data; name=\"" <> name <> "\"; filename=\"" <> fileName <> "\"\r\n"
    , "Content-Type: " <> contentType <> "\r\n\r\n"
    , content
    , "\r\n--" <> boundary <> "--\r\n"
    ]

-- | A value inside an answer's JSON, by keys and, in an array, by index.
at :: [Text] -> Reply -> Maybe Value
at path reply = Aeson.decode reply.body >>= go path
  where
    go [] value = Just value
    go (key : rest) (Object object) = KeyMap.lookup (Key.fromText key) object >>= go rest
    go (key : rest) (Array items) = case reads (T.unpack key) of
      [(index, "")] -> safeHead (drop index (toList items)) >>= go rest
      _ -> Nothing
    go _ _ = Nothing

text :: [Text] -> Reply -> Text
text path reply = case at path reply of
  Just (String value) -> value
  _ -> ""

-- | The fields a refusal names, in order, without repeats.
fieldsOf :: Reply -> [Text]
fieldsOf reply = case at ["fields"] reply of
  Just (Array items) -> foldr (\name seen -> if name `elem` seen then seen else name : seen) [] [name | Object item <- toList items, Just (String name) <- [KeyMap.lookup "field" item]]
  _ -> []

captions :: Reply -> [Text]
captions reply = case Aeson.decode reply.body of
  Just (Array items) -> [caption | Object item <- toList items, Just (String caption) <- [KeyMap.lookup "content_text" item]]
  _ -> []

-- | The captions of the posts in one of the two lists of a thread.
captionsAt :: Text -> Reply -> [Text]
captionsAt key reply = case at [key] reply of
  Just (Array items) -> [caption | Object item <- toList items, Just (String caption) <- [KeyMap.lookup "content_text" item]]
  _ -> []

-- | The usernames in a list of accounts.
usernames :: Reply -> [Text]
usernames reply = case Aeson.decode reply.body of
  Just (Array items) -> [name | Object item <- toList items, Just (String name) <- [KeyMap.lookup "username" item]]
  _ -> []

{- | A page of notifications as what happened, who did it, and the id of
the post sent with it, which is empty when none is.
-}
alerts :: Reply -> [(Text, Text, Text)]
alerts reply = case Aeson.decode reply.body of
  Just (Array items) -> [(textIn ["type"] item, textIn ["account", "username"] item, textIn ["status", "id"] item) | item <- toList items]
  _ -> []
  where
    textIn path value = case foldl step' (Just value) path of
      Just (String found) -> found
      _ -> ""
    step' (Just (Object object)) key = KeyMap.lookup (Key.fromText key) object
    step' _ _ = Nothing

-- | The URL in an answer's Link header with rel="next".
nextLink :: Reply -> Maybe Text
nextLink reply = do
  raw <- TE.decodeUtf8 <$> lookup "Link" reply.headers
  url <- T.stripPrefix "<" raw
  let (target, rest) = T.breakOn ">" url
  if "rel=\"next\"" `T.isInfixOf` rest then Just target else Nothing

locationQuery :: Reply -> [(Text, Text)]
locationQuery reply = case lookup "Location" reply.headers of
  Just location -> [(TE.decodeUtf8 k, TE.decodeUtf8 v) | (k, v) <- parseSimpleQuery (BS.drop 1 (snd (BS.break (== 63) location)))]
  Nothing -> []

-- | The path and query of an absolute URL on the test server.
pathOfUrl :: Text -> Text
pathOfUrl url = fromMaybe url (T.stripPrefix "https://pellucid.test" url)

-- | The storage key in a media URL.
keyOf :: Text -> Text
keyOf url = T.takeWhile (/= '.') (T.takeWhileEnd (/= '/') url)

safeHead :: [a] -> Maybe a
safeHead = \case
  first : _ -> Just first
  [] -> Nothing
