{- | The three pages a person sees in a browser: sign in and allow, a
message, and a code to copy. They load nothing from anywhere, run no
script, and every value that came from a request or from the database
passes through 'escapeHtml' on its way in.
-}
module Pellucid.Http.Pages (
  SignIn (..),
  signInPage,
  messagePage,
  codePage,
  html,
) where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (Status, hContentType)
import Network.Wai qualified as Wai
import Pellucid.Http.Respond (escapeHtml, safetyHeaders)

data SignIn = SignIn
  { serverTitle :: Text
  , appName :: Text
  , scopes :: [Text]
  , carried :: [(Text, Text)]
  -- ^ The authorization request, repeated back as hidden fields. The
  -- server checks every one of them again when the form comes in.
  , username :: Text
  , problem :: Maybe Text
  }

{- | An HTML answer. The policy allows the page's own inline style and
nothing else: no script, no image, no frame, no request to anywhere.
-}
html :: Status -> Text -> Wai.Response
html status page =
  Wai.responseLBS
    status
    ( [ (hContentType, "text/html; charset=utf-8")
      , ("Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'; base-uri 'none'; frame-ancestors 'none'")
      , ("X-Frame-Options", "DENY")
      ]
        <> safetyHeaders
    )
    (BL.fromStrict (TE.encodeUtf8 page))

signInPage :: SignIn -> Text
signInPage view =
  shell ("Sign in to " <> view.serverTitle) $
    T.concat
      [ "<h1>" <> escapeHtml view.serverTitle <> "</h1>"
      , "<p><strong>" <> escapeHtml view.appName <> "</strong> is asking to use your account. It will be able to:</p>"
      , "<ul>" <> T.concat ["<li>" <> escapeHtml (describeScope scope) <> "</li>" | scope <- view.scopes] <> "</ul>"
      , maybe "" (\why -> "<p class=\"problem\" role=\"alert\">" <> escapeHtml why <> "</p>") view.problem
      , "<form method=\"post\" action=\"/oauth/authorize\">"
      , T.concat [hidden name value | (name, value) <- view.carried]
      , "<label for=\"username\">Username</label>"
      , "<input id=\"username\" name=\"username\" autocomplete=\"username\" autocapitalize=\"none\" spellcheck=\"false\" required value=\"" <> escapeHtml view.username <> "\">"
      , "<label for=\"password\">Password</label>"
      , "<input id=\"password\" name=\"password\" type=\"password\" autocomplete=\"current-password\" required>"
      , "<div class=\"row\">"
      , "<button name=\"decision\" value=\"allow\">Sign in and allow</button>"
      , "<button name=\"decision\" value=\"deny\" class=\"quiet\" formnovalidate>Cancel</button>"
      , "</div></form>"
      ]
  where
    hidden name value = "<input type=\"hidden\" name=\"" <> escapeHtml name <> "\" value=\"" <> escapeHtml value <> "\">"

describeScope :: Text -> Text
describeScope = \case
  "read" -> "read your posts, your timelines and who you follow"
  "write" -> "post, upload and delete on your behalf"
  "follow" -> "follow and unfollow accounts"
  "push" -> "receive your notifications"
  other -> other

messagePage :: Text -> Text -> Text
messagePage heading body = shell heading ("<h1>" <> escapeHtml heading <> "</h1><p>" <> escapeHtml body <> "</p>")

{- | For a client that registered the out-of-band redirect: the person
copies the code into the client by hand.
-}
codePage :: Text -> Text
codePage code =
  shell "Your code" ("<h1>Your code</h1><p>Paste this into the app that asked for it.</p><p><code>" <> escapeHtml code <> "</code></p>")

shell :: Text -> Text -> Text
shell title body =
  T.concat
    [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<title>" <> escapeHtml title <> "</title><style>"
    , "body{font:16px/1.5 system-ui,sans-serif;max-width:26rem;margin:3rem auto;padding:0 1rem;color:#1a1a1a;background:#fafafa}"
    , "h1{font-size:1.4rem}label{display:block;margin-top:1rem;font-weight:600}"
    , "input:not([type=hidden]){width:100%;box-sizing:border-box;padding:.6rem;font:inherit;border:1px solid #767676;border-radius:.3rem}"
    , ".row{display:flex;gap:.75rem;margin-top:1.5rem}"
    , "button{padding:.6rem 1rem;font:inherit;border:1px solid #1a1a1a;border-radius:.3rem;background:#1a1a1a;color:#fff}"
    , "button.quiet{background:transparent;color:#1a1a1a}"
    , ".problem{color:#a40000;font-weight:600}code{word-break:break-all}"
    , "@media(prefers-color-scheme:dark){body{color:#eee;background:#161616}"
    , "input:not([type=hidden]){background:#222;color:#eee;border-color:#999}"
    , "button{background:#eee;color:#161616;border-color:#eee}button.quiet{color:#eee}.problem{color:#ff8a80}}"
    , "</style></head><body>"
    , body
    , "</body></html>"
    ]
