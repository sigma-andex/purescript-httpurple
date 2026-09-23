module HTTPurple.Cookie
  ( Cookie
  , CookieAttributes
  , CookieAttributesR
  , CookieDeletionAttributesR
  , CookieName
  , CookieValue
  , SameSite(..)
  , addCookie
  , clearCookie
  , clearCookie'
  , cookie
  , cookie'
  , cookieHeader
  , defaultAttributes
  , expireCookie
  , expireCookie'
  , requestCookies
  , serialize
  , setCookie
  , setCookie'
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (catMaybes, mapMaybe) as Array
import Data.Map (Map)
import Data.Map (empty, fromFoldableWith, insertWith) as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (Pattern(Pattern), drop, indexOf, joinWith, null, split, take, trim)
import Data.String.CaseInsensitive (CaseInsensitiveString(CaseInsensitiveString))
import Data.Tuple (Tuple(Tuple))
import HTTPurple.Headers (RequestHeaders, ResponseHeaders(ResponseHeaders), header)
import HTTPurple.Lookup (lookup)
import HTTPurple.Utils (encodeURIComponent, urlDecode)
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

-- | The `SameSite` attribute controls whether a cookie is sent with cross-site requests.
data SameSite
  = -- | Never sent with cross-site requests, not even top-level navigations.
    Strict
  | -- | Sent with top-level cross-site navigations (e.g. following a link), but not with cross-site
    -- | subrequests (images, iframes, fetches).
    Lax
  | -- | Sent with all requests, same-site or cross-site; requires `secure = Just true`.
    None

derive instance Eq SameSite

instance Show SameSite where
  show Strict = "Strict"
  show Lax = "Lax"
  show None = "None"

-- | The set of attributes that identify which cookie to delete.
type CookieDeletionAttributesR = (path :: Maybe String, domain :: Maybe String)

-- | The set of attributes that can accompany a `Set-Cookie` directive.
type CookieAttributesR =
  ( maxAge :: Maybe Int
  , httpOnly :: Maybe Boolean
  , secure :: Maybe Boolean
  , sameSite :: Maybe SameSite
  | CookieDeletionAttributesR
  )

type CookieAttributes = { | CookieAttributesR }

-- | Attributes meant to be overridden as needed. `path`/`domain`/`maxAge` are left unset, but `httpOnly`,
-- | `secure`, and `sameSite` default to secure values: `httpOnly` keeps the cookie out of reach of JavaScript
-- | (guarding against theft via XSS), `secure` confines it to HTTPS connections, and `sameSite: Lax`
-- | withholds it from cross-site subrequests (guarding against CSRF). These follow the OWASP Session
-- | Management Cheat Sheet:
-- | https://cheatsheetseries.owasp.org/cheatsheets/Session_Management_Cheat_Sheet.html#cookies
-- |
-- | Note that `secure` means browsers will not send the cookie back over plain `http://` (except on
-- | `localhost`/`127.0.0.1`, which most treat as trustworthy). When developing over HTTP on a non-localhost
-- | host, set `secure: false` so the cookie is not silently dropped.
defaultAttributes :: CookieAttributes
defaultAttributes =
  { path: Nothing
  , domain: Nothing
  , maxAge: Nothing
  , httpOnly: Just true
  , secure: Just true
  , sameSite: Just Lax
  }

type CookieName = String
type CookieValue = String

-- | A cookie name, value, and the attributes to serialize alongside it.
type Cookie =
  { name :: CookieName
  , value :: CookieValue
  , attributes :: CookieAttributes
  }

-- | Build a `Cookie` with `defaultAttributes`.
cookie :: CookieName -> CookieValue -> Cookie
cookie name value = cookie' name value {}

-- | Build a `Cookie` from a name, value, and a partial attributes record — fields you omit fall back to
-- | `defaultAttributes`'s values.
cookie' ::
  forall from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing CookieAttributesR =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  CookieName ->
  CookieValue ->
  { | from } ->
  Cookie
cookie' name value attrs =
  { name, value, attributes: fillDefaults (justifill attrs) }
  where
  fillDefaults :: CookieAttributes -> CookieAttributes
  fillDefaults filled =
    { path: filled.path <|> defaultAttributes.path
    , domain: filled.domain <|> defaultAttributes.domain
    , maxAge: filled.maxAge <|> defaultAttributes.maxAge
    , httpOnly: filled.httpOnly <|> defaultAttributes.httpOnly
    , secure: filled.secure <|> defaultAttributes.secure
    , sameSite: filled.sameSite <|> defaultAttributes.sameSite
    }

-- Attributes aren't available here: the Cookie request header only ever carries name/value pairs.
-- | Parse the `Cookie` request header into a map of URL-decoded names to URL-decoded values.
requestCookies :: RequestHeaders -> Map CookieName CookieValue
requestCookies reqHeaders = lookup reqHeaders "Cookie" # maybe Map.empty parse
  where
  -- A browser sends same-named cookies most-specific-path first (RFC 6265 §5.4), so on a
  -- duplicate name keep the first occurrence, as other servers do.
  parse :: String -> Map CookieName CookieValue
  parse =
    Map.fromFoldableWith (flip const)
      <<< split (Pattern ";")
      >>> map trim
      >>> Array.mapMaybe toNameValuePair

  toNameValuePair :: String -> Maybe (Tuple CookieName CookieValue)
  toNameValuePair pair =
    indexOf (Pattern "=") pair >>= \i ->
      let
        name = (take i pair) # trim # urlDecode
        value = drop (i + 1) pair # urlDecode
      in
        if null name then Nothing else Just (Tuple name value)

-- | Turn a `Cookie` record into the string value used in a `Set-Cookie` header
serialize :: Cookie -> String
serialize { name, value, attributes: attrs } = joinWith "; " $ Array.catMaybes
  [ encodeURIComponent name <> "=" <> encodeURIComponent value # Just
  , ("Path=" <> _) <$> attrs.path
  , ("Domain=" <> _) <$> attrs.domain
  , ("Max-Age=" <> _) <<< show <$> attrs.maxAge
  , ("SameSite=" <> _) <<< show <$> attrs.sameSite
  , "HttpOnly" <$ (guard =<< attrs.httpOnly)
  , "Secure" <$ (guard =<< attrs.secure)
  ]

-- | Build a `ResponseHeaders` carrying a single `Set-Cookie` header for a `Cookie` value, e.g. one folded
-- | together with `addCookie`.
cookieHeader :: Cookie -> ResponseHeaders
cookieHeader c = header "Set-Cookie" (serialize c)

-- | Build `ResponseHeaders` that set a cookie by name and value, using `defaultAttributes`.
setCookie :: CookieName -> CookieValue -> ResponseHeaders
setCookie name value = cookieHeader (cookie name value)

-- | Like `setCookie`, filling any attributes not given with `defaultAttributes`'s values.
setCookie' ::
  forall from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing CookieAttributesR =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  CookieName ->
  CookieValue ->
  { | from } ->
  ResponseHeaders
setCookie' name value attrs = cookieHeader (cookie' name value attrs)

-- | Append a `Set-Cookie` header to existing `ResponseHeaders`, keeping any cookies already present since a
-- | response may carry several of them.
addCookie :: Cookie -> ResponseHeaders -> ResponseHeaders
addCookie c (ResponseHeaders m) =
  ResponseHeaders $ Map.insertWith (<>) (CaseInsensitiveString "Set-Cookie") [ serialize c ] m

-- | Build a `Cookie` that instructs the client to delete the named cookie, using `defaultAttributes`.
expireCookie :: CookieName -> Cookie
expireCookie name = expireCookie' name {}

-- | Like `expireCookie`, filling any attributes not given with `defaultAttributes`'s values. Pass matching
-- | `path`/`domain` here if the original cookie set them, since the client only clears a cookie whose scope
-- | matches exactly.
expireCookie' ::
  forall from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing CookieDeletionAttributesR =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  CookieName ->
  { | from } ->
  Cookie
expireCookie' name attrs =
  let
    { path, domain } = justifill attrs :: { | CookieDeletionAttributesR }
  in
    { name, value: "", attributes: defaultAttributes { path = path, domain = domain, maxAge = Just 0 } }

-- | Build `ResponseHeaders` that instruct the client to delete the named cookie, using `defaultAttributes`.
clearCookie :: CookieName -> ResponseHeaders
clearCookie name = cookieHeader (expireCookie name)

-- | Like `clearCookie`, filling any attributes not given with `defaultAttributes`'s values. Pass matching
-- | `path`/`domain` here if the original cookie set them, since the client only clears a cookie whose scope
-- | matches exactly.
clearCookie' ::
  forall from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing CookieDeletionAttributesR =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  CookieName ->
  { | from } ->
  ResponseHeaders
clearCookie' name attrs = cookieHeader (expireCookie' name attrs)
