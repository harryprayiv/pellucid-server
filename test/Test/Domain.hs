-- | The pure decisions, each against the rule of the spec it carries out.
module Test.Domain (
  tests,
) where

import Data.Int (Int64)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Pellucid.Domain.Account (AccountStatus (..), accountNext, canAct, canSignIn, isMember)
import Pellucid.Domain.Cursor (Cursor (..), Page (..), clampLimit, decodeCursor, encodeCursor, paginate)
import Pellucid.Domain.Follow (FollowOutcome (..), FollowRefusal (..), FollowRequest (..), FollowState (..), decideFollow)
import Pellucid.Domain.Ids (AccountId (..), idText, parseId)
import Pellucid.Domain.Interaction
import Pellucid.Domain.Limits (Limits (..), defaultLimits)
import Pellucid.Domain.Notification
import Pellucid.Domain.OAuth
import Pellucid.Domain.Pkce (Challenge, challengeOf, challengeText, mkChallenge, verifierMatches)
import Pellucid.Domain.Post
import Pellucid.Domain.Username (mkUsername, usernameKey, usernameText)
import Pellucid.Http.Routes.OAuth (validRedirect)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), choose, elements, forAll, testProperty, (.&&.), (===), (==>))

tests :: TestTree
tests =
  testGroup
    "domain"
    [ ids
    , usernames
    , pkce
    , grants
    , redemption
    , scopes
    , cursors
    , lifecycles
    , reading
    , publishing
    , following
    , marking
    , commenting
    , notifications
    , redirects
    ]

ids :: TestTree
ids =
  testGroup
    "ids"
    [ testProperty "an id survives being written and read" \(Positive n) -> parseId (idText n) === Just (n :: Int64)
    , testCase "what is not a positive 64-bit number in digits is no id" do
        map parseId ["", "0", "-1", "12a", " 12", "1.5", "9223372036854775808", "99999999999999999999"] @?= replicate 8 Nothing
        parseId "9223372036854775807" @?= Just maxBound
    ]

usernames :: TestTree
usernames =
  testGroup
    "usernames"
    [ testCase "kept as written, compared without case" do
        fmap usernameText (mkUsername " Alice_1 ") @?= Right "Alice_1"
        fmap usernameKey (mkUsername "Alice_1") @?= Right "alice_1"
    , testCase "what cannot be a username" do
        assertBool "empty" (isLeft' (mkUsername "  "))
        assertBool "too long" (isLeft' (mkUsername (T.replicate 31 "a")))
        assertBool "a space" (isLeft' (mkUsername "a b"))
        assertBool "an at sign" (isLeft' (mkUsername "a@b"))
        assertBool "a slash" (isLeft' (mkUsername "a/b"))
        assertBool "a leading dot" (isLeft' (mkUsername ".ab"))
        assertBool "a trailing hyphen" (isLeft' (mkUsername "ab-"))
        assertBool "letters outside ASCII" (isLeft' (mkUsername "al\233"))
    ]
  where
    isLeft' = either (const True) (const False)

-- | The verifier and challenge of RFC 7636, appendix B.
pkce :: TestTree
pkce =
  testGroup
    "pkce"
    [ testCase "the RFC's own example" do
        fmap challengeText (challengeOf rfcVerifier) @?= Just rfcChallenge
        fmap (`verifierMatches` rfcVerifier) (mkChallenge rfcChallenge "S256") @?= Right True
    , testCase "another verifier does not match" do
        fmap (`verifierMatches` T.replicate 43 "a") (mkChallenge rfcChallenge "S256") @?= Right False
    , testCase "only S256, only a well-formed challenge" do
        assertBool "plain is refused" (isLeft' (mkChallenge rfcChallenge "plain"))
        assertBool "a short challenge is refused" (isLeft' (mkChallenge "abc" "S256"))
        assertBool "a challenge with padding is refused" (isLeft' (mkChallenge (T.take 42 rfcChallenge <> "=") "S256"))
    , testCase "a verifier outside 43 to 128 unreserved characters matches nothing" do
        assertBool "too short" (isNothing (challengeOf (T.replicate 42 "a")))
        assertBool "too long" (isNothing (challengeOf (T.replicate 129 "a")))
        assertBool "a space" (isNothing (challengeOf (T.replicate 42 "a" <> " ")))
    ]
  where
    isLeft' = either (const True) (const False)

rfcVerifier, rfcChallenge :: T.Text
rfcVerifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
rfcChallenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

-- | Rule AuthorizationGranted.
grants :: TestTree
grants =
  testGroup
    "authorization"
    [ testCase "a registered redirect, registered scopes and a challenge are granted" do
        authorize (grant Active PublicClient okRedirect [ScopeRead] okChallenge) @?= Right ()
    , testCase "a suspended account can still sign in; a leaving or departed one cannot" do
        authorize (grant Suspended PublicClient okRedirect [ScopeRead] okChallenge) @?= Right ()
        authorize (grant Leaving PublicClient okRedirect [ScopeRead] okChallenge) @?= Left AccountMayNotSignIn
        authorize (grant Departed PublicClient okRedirect [ScopeRead] okChallenge) @?= Left AccountMayNotSignIn
    , testCase "a redirect is compared character for character" do
        authorize (grant Active PublicClient (okRedirect <> "/") [ScopeRead] okChallenge) @?= Left RedirectNotRegistered
        authorize (grant Active PublicClient "https://evil.example/callback" [ScopeRead] okChallenge) @?= Left RedirectNotRegistered
    , testCase "a scope the client did not register is not granted" do
        authorize (grant Active PublicClient okRedirect [ScopeRead, ScopePush] okChallenge) @?= Left ScopeNotRegistered
    , testCase "invariant PublicClientsAlwaysUsePkce" do
        authorize (grant Active PublicClient okRedirect [ScopeRead] Nothing) @?= Left PublicClientWithoutPkce
        authorize (grant Active ConfidentialClient okRedirect [ScopeRead] Nothing) @?= Right ()
    ]

noon :: UTCTime
noon = UTCTime (fromGregorian 2026 11 10) (12 * 3600)

-- | Rules TokenIssued and CodeExpired.
redemption :: TestTree
redemption =
  testGroup
    "token"
    [ testCase "a fresh code with the right verifier is redeemed" (run publicCode good @?= IssueToken)
    , testCase "a redeemed code revokes what it produced" (run (codeOf Redeemed okChallenge PublicClient Nothing) good @?= RefuseAndRevoke)
    , testCase "a stale code is refused" (run (codeOf Stale okChallenge PublicClient Nothing) good @?= Refuse CodeStale)
    , testCase "a code at the end of its lifetime is expired, one second before is not" do
        redeem 600 (addUTCTime 600 noon) publicCode good @?= RefuseAndExpire
        redeem 600 (addUTCTime 599 noon) publicCode good @?= IssueToken
    , testCase "the redirect must be the one the code was issued for" do
        run publicCode (present "http://127.0.0.1:5555/other" (Just rfcVerifier) Nothing) @?= Refuse RedirectMismatch
    , testCase "the verifier must be present and right" do
        run publicCode (present okRedirect Nothing Nothing) @?= Refuse VerifierMissing
        run publicCode (present okRedirect (Just (T.replicate 43 "a")) Nothing) @?= Refuse VerifierWrong
    , testCase "a public client's code without a challenge is refused all the same" do
        run (codeOf Issued Nothing PublicClient Nothing) good @?= Refuse VerifierMissing
    , testCase "a confidential client must bring its secret, with or without PKCE" do
        run confidential good @?= Refuse SecretWrong
        run confidential (present okRedirect (Just rfcVerifier) (Just "other")) @?= Refuse SecretWrong
        run confidential (present okRedirect (Just rfcVerifier) (Just "hash")) @?= IssueToken
        run (codeOf Issued Nothing ConfidentialClient (Just "hash")) (present okRedirect Nothing (Just "hash")) @?= IssueToken
    , testProperty "a token is issued only for an issued code inside its lifetime" $
        forAll (elements [minBound .. maxBound]) \status ->
          forAll (choose (0, 1200 :: Int)) \age ->
            let outcome = redeem 600 (addUTCTime (fromIntegral age) noon) (codeOf status okChallenge PublicClient Nothing) good
             in (outcome == IssueToken) === (status == Issued && age < 600)
    , testCase "a token is usable while valid and inside its lifetime" do
        tokenUsable 100 (addUTCTime 99 noon) Valid noon @?= True
        tokenUsable 100 (addUTCTime 100 noon) Valid noon @?= False
        tokenUsable 100 noon Revoked noon @?= False
        tokenUsable 100 noon Lapsed noon @?= False
    ]
  where
    run = redeem 600 (addUTCTime 60 noon)
    publicCode = codeOf Issued okChallenge PublicClient Nothing
    confidential = codeOf Issued okChallenge ConfidentialClient (Just "hash")
    good = present okRedirect (Just rfcVerifier) Nothing

okRedirect :: T.Text
okRedirect = "http://127.0.0.1:5555/callback"

okChallenge :: Maybe Challenge
okChallenge = either (const Nothing) Just (mkChallenge rfcChallenge "S256")

-- The records these three build share field names, so a variant is built
-- whole instead of by updating one field of another.

grant :: AccountStatus -> ClientKind -> T.Text -> [Scope] -> Maybe Challenge -> GrantRequest
grant accountStatus clientKind redirectUri asked challenge =
  GrantRequest
    { accountStatus
    , clientKind
    , registeredRedirects = [okRedirect]
    , registeredScopes = Set.fromList [ScopeRead, ScopeWrite]
    , redirectUri
    , scopes = Set.fromList asked
    , challenge
    }

codeOf :: CodeStatus -> Maybe Challenge -> ClientKind -> Maybe T.Text -> CodeFacts
codeOf status challenge clientKind clientSecretHash =
  CodeFacts {status, redirectUri = okRedirect, challenge, issuedAt = noon, clientKind, clientSecretHash}

present :: T.Text -> Maybe T.Text -> Maybe T.Text -> Presentation
present redirectUri verifier secretHash = Presentation {redirectUri, verifier, secretHash}

scopes :: TestTree
scopes =
  testGroup
    "scopes"
    [ testCase "the four scopes are read and written back in one order" do
        fmap renderScopes (parseScopes "write  read push follow") @?= Right "read write follow push"
    , testCase "an unknown scope is a refusal, not something dropped or widened" do
        parseScopes "read admin:write" @?= Left "admin:write"
        parseScopes "read:accounts" @?= Left "read:accounts"
    , testCase "follow routes open to follow or to write; write routes do not open to follow" do
        satisfies (Set.fromList [ScopeFollow]) NeedFollow @?= True
        satisfies (Set.fromList [ScopeWrite]) NeedFollow @?= True
        satisfies (Set.fromList [ScopeFollow]) NeedWrite @?= False
        satisfies (Set.fromList [ScopeRead]) NeedWrite @?= False
        satisfies (Set.fromList [ScopeWrite]) NeedRead @?= False
    ]

-- | S12.
cursors :: TestTree
cursors =
  testGroup
    "cursors"
    [ testProperty "a cursor survives being written and read" \(Positive n) -> decodeCursor (encodeCursor (Cursor n)) === Just (Cursor n)
    , testCase "what this server did not issue is no cursor" do
        map decodeCursor ["", "12", "YzE6", "YzI6MTI", "not base64!"] @?= replicate 5 Nothing
    , testProperty "a page shows at most the limit and points on exactly when more exists" \(Positive limit) rows ->
        let page = paginate limit id (rows :: [Int64])
         in page.items === take limit rows .&&. isJust page.next === (length rows > limit)
    , testProperty "the cursor is the last item shown" \(Positive limit) rows ->
        length (rows :: [Int64]) > limit ==> (paginate limit id rows).next === fmap Cursor (listToMaybe (drop (limit - 1) rows))
    , testCase "a list that ends on a page boundary has no cursor" do
        (paginate 2 id [5, 4 :: Int64]).next @?= Nothing
        (paginate 2 id [5, 4, 3 :: Int64]).next @?= Just (Cursor 4)
    , testCase "the page size is kept between one and the largest page" do
        map (clampLimit 20 40) [Nothing, Just 0, Just (-3), Just 7, Just 40, Just 500] @?= [20, 1, 1, 7, 40, 40]
    ]

{- | The transition graphs, written out again from the spec's entities so
that a change to one side without the other fails here.
-}
lifecycles :: TestTree
lifecycles =
  testGroup
    "lifecycles"
    [ testCase "account" do
        map accountNext [Active, Suspended, Leaving, Departed] @?= [[Suspended, Leaving], [Active, Leaving], [Active, Departed], []]
    , testCase "post" do
        map postNext [Published, TakenDown, Withdrawn] @?= [[Withdrawn, TakenDown], [Published, Withdrawn], []]
    , testCase "media" do
        map mediaNext [Received, Processing, Ready, Unusable, Attached, Purged] @?= [[Processing], [Ready, Unusable], [Attached, Purged], [Purged], [Purged], []]
    , testCase "who can sign in, hold a session, and act" do
        map canSignIn [Active, Suspended, Leaving, Departed] @?= [True, True, False, False]
        map isMember [Active, Suspended, Leaving, Departed] @?= [True, True, True, False]
        map canAct [Active, Suspended, Leaving, Departed] @?= [True, False, False, False]
    , testCase "every state has a name the store can read back" do
        map (parsePostStatus . postStatusText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
        map (parseMediaStatus . mediaStatusText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
        map (parseVisibility . visibilityText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
        map (parseClientKind . clientKindText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
    ]

-- | The reading rule can_see.
reading :: TestTree
reading =
  testGroup
    "can_see"
    [ testCase "public and unlisted posts are seen by anyone" do
        canSee Published Public stranger @?= True
        canSee Published Unlisted stranger @?= True
    , testCase "a followers-only post is seen by its author and by accepted followers" do
        canSee Published FollowersOnly stranger @?= False
        canSee Published FollowersOnly stranger {isAcceptedFollower = True} @?= True
        canSee Published FollowersOnly stranger {isAuthor = True} @?= True
        canSee Published FollowersOnly stranger {isModerator = True} @?= False
    , testCase "a taken-down post is seen by its author and by moderators" do
        canSee TakenDown Public stranger @?= False
        canSee TakenDown Public stranger {isAcceptedFollower = True} @?= False
        canSee TakenDown Public stranger {isAuthor = True} @?= True
        canSee TakenDown Public stranger {isModerator = True} @?= True
    , testProperty "a withdrawn post is seen by nobody" \a f m ->
        forAll (elements [minBound .. maxBound]) \visibility ->
          canSee Withdrawn visibility Viewer {isAuthor = a, isAcceptedFollower = f, isModerator = m} === False
    , testCase "a comment is seen by whoever can read the post it is under, and by its author" do
        canSeeComment Published stranger True @?= True
        canSeeComment Published stranger False @?= False
        canSeeComment Published stranger {isAuthor = True} False @?= True
        -- Following the commenter is not following the author of the post.
        canSeeComment Published stranger {isAcceptedFollower = True} False @?= False
    , testCase "a comment that is taken down or withdrawn is read as such a post is" do
        canSeeComment TakenDown stranger True @?= False
        canSeeComment TakenDown stranger {isAuthor = True} False @?= True
        canSeeComment TakenDown stranger {isModerator = True} False @?= True
        canSeeComment Withdrawn stranger {isAuthor = True, isModerator = True} True @?= False
    , testCase "the API calls followers-only private" do
        visibilityFromApi "private" @?= Just FollowersOnly
        visibilityToApi FollowersOnly @?= "private"
        visibilityFromApi "everyone" @?= Nothing
    ]

-- | Rule PostPublished.
publishing :: TestTree
publishing =
  testGroup
    "publishing"
    [ testCase "a photo and a caption are published" (check Active good @?= [])
    , testCase "a post needs media" (check Active good {attachments = []} @?= [NoAttachment])
    , testCase "an album has a largest size" do
        check Active good {attachments = replicate 10 ready} @?= []
        check Active good {attachments = replicate 11 ready} @?= [TooManyAttachments 10]
    , testCase "an attachment must be the author's and not yet on a post" do
        check Active good {attachments = [ready {ownedByAuthor = False}]} @?= [AttachmentNotUsable]
        check Active good {attachments = [ready, Attachment {ownedByAuthor = True, status = Attached}]} @?= [AttachmentNotUsable]
    , testCase "a caption and a content warning have a largest length, counted in characters" do
        check Active good {caption = T.replicate 500 "\233"} @?= []
        check Active good {caption = T.replicate 501 "a"} @?= [CaptionTooLong 500]
        check Active good {spoilerText = T.replicate 141 "a"} @?= [SpoilerTooLong 140]
    , testCase "a suspended or leaving account cannot post" do
        check Suspended good @?= [AuthorNotActive]
        check Leaving good @?= [AuthorNotActive]
    , testCase "a direct post is a direct message" (check Active good {visibility = Direct} @?= [DirectIsAMessage])
    , testCase "every reason is given at once" do
        check Suspended good {attachments = [], caption = T.replicate 501 "a"} @?= [AuthorNotActive, NoAttachment, CaptionTooLong 500]
    , testCase "the author can withdraw a published or a taken-down post, once" do
        map (mayWithdraw True) [Published, TakenDown, Withdrawn] @?= [True, True, False]
        map (mayWithdraw False) [Published, TakenDown, Withdrawn] @?= [False, False, False]
    , testCase "an upload can be taken back by its owner until it is attached" do
        map (mayTakeBack True) [Ready, Unusable, Attached, Purged] @?= [True, True, False, False]
        mayTakeBack False Ready @?= False
    , testCase "the accepted types are the ones that can be scrubbed" do
        defaultLimits.allowedMediaTypes @?= ["image/jpeg", "image/png"]
    ]
  where
    check = checkPublish defaultLimits
    ready = Attachment {ownedByAuthor = True, status = Ready}
    good = Submission {caption = "Golden hour", spoilerText = "", visibility = Public, attachments = [ready]}

-- | Rules FollowedOpenAccount and FollowRequestedOfLockedAccount.
following :: TestTree
following =
  testGroup
    "following"
    [ testCase "an open account is followed at once" (decideFollow base @?= Right StartFollowing)
    , testCase "a locked account is asked" (decideFollow base {targetLocked = True} @?= Right StartRequest)
    , testCase "invariant NobodyFollowsThemselves" (decideFollow base {sameAccount = True} @?= Left CannotFollowSelf)
    , testCase "both accounts must be active" do
        decideFollow base {followerStatus = Suspended} @?= Left FollowerNotActive
        decideFollow base {targetStatus = Leaving} @?= Left TargetNotActive
    , testCase "invariant OneFollowPerPair: asking again changes nothing" do
        decideFollow base {existing = Following} @?= Right (AlreadyStands Following)
        decideFollow base {existing = Requested, targetLocked = True} @?= Right (AlreadyStands Requested)
    ]
  where
    base = FollowRequest {followerStatus = Active, targetStatus = Active, sameAccount = False, targetLocked = False, existing = NoFollow}

-- | Rules PostLiked and PostBookmarked.
marking :: TestTree
marking =
  testGroup
    "likes and bookmarks"
    [ testCase "a post that can be read is liked, once" do
        decideLike base @?= Right Place
        decideLike base {standing = True} @?= Right AlreadyPlaced
    , testCase "a post that cannot be read is out of reach, whoever asks and whatever stands" do
        decideLike base {readable = False} @?= Left PostOutOfReach
        decideLike base {readable = False, actorStatus = Suspended, standing = True} @?= Left PostOutOfReach
        decideBookmark base {readable = False, standing = True} @?= Left PostOutOfReach
    , testCase "a suspended or leaving account cannot like" do
        map (\status -> decideLike base {actorStatus = status}) [Suspended, Leaving] @?= [Left ActorNotActive, Left ActorNotActive]
    , testCase "a bookmark asks nothing of the account's standing" do
        decideBookmark base {actorStatus = Suspended} @?= Right Place
        decideBookmark base {standing = True} @?= Right AlreadyPlaced
    , testCase "an author is told of what others do to her post, and never of what she did" do
        recipientOf (AccountId 1) (AccountId 2) @?= Just (AccountId 2)
        recipientOf (AccountId 2) (AccountId 2) @?= Nothing
    ]
  where
    base = Mark {actorStatus = Active, readable = True, standing = False}

-- | Rule CommentPublished.
commenting :: TestTree
commenting =
  testGroup
    "commenting"
    [ testCase "some text under a post that can be read is a comment" (checkComment defaultLimits base @?= [])
    , testCase "a comment needs text that is not only blank" do
        checkComment defaultLimits base {text = ""} @?= [EmptyComment]
        checkComment defaultLimits base {text = " \n\t "} @?= [EmptyComment]
    , testCase "a comment has a caption's largest length, counted in characters" do
        checkComment defaultLimits base {text = T.replicate 500 "é"} @?= []
        checkComment defaultLimits base {text = T.replicate 501 "é"} @?= [CommentTooLong 500]
    , testCase "a post can turn comments off" (checkComment defaultLimits base {commentsDisabled = True} @?= [CommentsAreOff])
    , testCase "a suspended or leaving account cannot comment" do
        map (\status -> checkComment defaultLimits base {authorStatus = status}) [Suspended, Leaving] @?= [[CommenterNotActive], [CommenterNotActive]]
    , testCase "every reason is given at once" do
        checkComment defaultLimits base {authorStatus = Suspended, commentsDisabled = True, text = ""} @?= [CommenterNotActive, CommentsAreOff, EmptyComment]
    , testCase "a parent that cannot be read is the only reason given, so nothing is said about it" do
        checkComment defaultLimits base {parentReadable = False, authorStatus = Suspended, commentsDisabled = True, text = ""} @?= [ParentOutOfReach]
    , testCase "every reason names the field it is about" do
        map (fst . describeCommentProblem) [ParentOutOfReach, CommenterNotActive, CommentsAreOff, EmptyComment, CommentTooLong 500]
          @?= ["in_reply_to_id", "account", "in_reply_to_id", "status", "status"]
    ]
  where
    base = CommentSubmission {authorStatus = Active, parentReadable = True, commentsDisabled = False, text = "Lovely light"}

-- | Enum NotificationKind and entity Notification's lifecycle.
notifications :: TestTree
notifications =
  testGroup
    "notifications"
    [ testCase "every kind and state has a name the store can read back" do
        map (parseKind . kindText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
        map (parseNotificationStatus . notificationStatusText) [minBound .. maxBound] @?= map Just [minBound .. maxBound]
    , testCase "what is produced is called what Pixelfed calls it" do
        map kindToApi [Followed, FollowRequested, Liked, Commented] @?= ["follow", "follow_request", "favourite", "comment"]
    , testCase "no two kinds share a word" do
        let names = map kindToApi [minBound .. maxBound]
        Set.size (Set.fromList names) @?= length names
    , testCase "a notification is seen once" (map notificationNext [Unread, Seen] @?= [[Seen], []])
    ]

-- | What rule ClientRegistered accepts as a place to send a code.
redirects :: TestTree
redirects =
  testGroup
    "redirects"
    [ testCase "https, loopback http, an app's own scheme and the out-of-band marker" do
        map
          validRedirect
          [ "https://app.example/cb"
          , "http://127.0.0.1:5555/callback"
          , "http://localhost:8080/cb"
          , "http://[::1]:9/cb"
          , "http://127.0.0.1/cb"
          , "pellucid://callback"
          , "urn:ietf:wg:oauth:2.0:oob"
          ]
          @?= replicate 7 Nothing
    , testCase "what cannot receive a code" do
        assertBool "http to another machine" (isJust (validRedirect "http://app.example/cb"))
        assertBool "a host that only starts like loopback" (isJust (validRedirect "http://127.0.0.1.evil.example/cb"))
        assertBool "loopback as a user name on another host" (isJust (validRedirect "http://127.0.0.1:5555@evil.example/cb"))
        assertBool "a user name on an https host" (isJust (validRedirect "https://app.example@evil.example/cb"))
        assertBool "a port that is not a number" (isJust (validRedirect "http://localhost:evil/cb"))
        assertBool "javascript" (isJust (validRedirect "javascript:alert(1)"))
        assertBool "data" (isJust (validRedirect "data:text/html,x"))
        assertBool "a fragment" (isJust (validRedirect "https://app.example/cb#x"))
        assertBool "no scheme" (isJust (validRedirect "/callback"))
        assertBool "https with no host" (isJust (validRedirect "https://"))
    ]
