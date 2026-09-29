-- | `resolveRoot` and `resolveRequestPath` are exported for testing and are deliberately kept
-- | out of the top-level `HTTPurple` re-export, like `HTTPurple.Path.read`.
module HTTPurple.Static
  ( Dotfiles(..)
  , RequestPathError(..)
  , RootError(..)
  , StaticOptions
  , StaticOptionsR
  , resolveRequestPath
  , resolveRoot
  , serveStatic
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), contains, null, stripPrefix, toLower)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import Foreign (readString, unsafeToForeign)
import Foreign.Index (readProp)
import HTTPurple.Headers (ResponseHeaders, headers)
import HTTPurple.Response (Response, badRequest', forbidden', internalServerError', notFound', ok')
import Justifill (justifill)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Sync as Sync
import Node.Path (FilePath)
import Node.Path as Path
import Prim.Row (class Union)
import Prim.RowList (class RowToList)

-- | What to do with a request naming a segment that starts with `.`:
-- | `Ignore` answers 404 as if nothing were there, `Deny` answers 403, `Allow` serves it.
-- | `Allow` is for the rare root that must publish dotted paths such as `/.well-known/`.
-- | It applies to the whole root, `.env` and `.git/` included.
data Dotfiles = Allow | Deny | Ignore

derive instance Eq Dotfiles
derive instance Generic Dotfiles _

instance Show Dotfiles where
  show = genericShow

-- | `root` is required. `dotfiles` defaults to `Ignore`, because a root is often a checkout or a
-- | build output and so holds `.env` or `.git/` — files that must never leak by default. Pass a
-- | plain record with any subset of the optional fields, as with `serve`'s options; `justifill`
-- | completes it.
type StaticOptionsR =
  ( root :: FilePath
  , dotfiles :: Maybe Dotfiles
  )

type StaticOptions = { | StaticOptionsR }

-- | Serves the one file under `root` that the router names. A request naming a directory is a 404:
-- | the handler never looks for an `index.html` inside it. To serve an index page, name the file in
-- | a route clause, e.g. `router { route: Home } = serveAsset { asset: [ "index.html" ] }`.
-- |
-- | The handler is method-blind: dispatching on `method` (and any 405) is the router's job. `HEAD`
-- | needs no special case: Node discards the body of a response to a `HEAD` request on its own, so
-- | the same `Response` yields the correct headers, `Content-Length` included, and no body.
-- |
-- | Run this **once at startup** and reuse the returned handler: every run re-resolves the root.
serveStatic ::
  forall m from fromRL via missing missingList.
  MonadAff m =>
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing StaticOptionsR =>
  RowToList from fromRL =>
  JustifiableFields fromRL from () via =>
  { | from } ->
  Effect (Either RootError ({ asset :: Array String } -> m Response))
serveStatic inputOptions = map serveFrom <$> resolveRoot root
  -- Resolving once at startup pins the containment base against a later `chdir`
  where
  options :: StaticOptions
  options = justifill inputOptions

  root = options.root
  dotfiles = fromMaybe Ignore options.dotfiles

  -- `root` is captured before the per-request lambda, so requests reuse the already-resolved path
  serveFrom :: FilePath -> { asset :: Array String } -> m Response
  serveFrom resolvedRoot { asset } =
    -- routing-duplex decodes captured segments, so `asset` arrives already decoded
    resolveRequestPath dotfiles resolvedRoot asset # either refuse serve

  refuse :: RequestPathError -> m Response
  refuse = case _ of
    MalformedPath -> badRequest
    -- refusing without saying whether the target exists
    EscapedRoot -> forbidden
    -- answers exactly like a missing file, so probing can't tell hidden from absent
    DotfileHidden -> notFound
    DotfileDenied -> forbidden

  -- whole-file read, and no `ETag`/`Last-Modified` support to date, so every request re-sends the entire body
  serve :: FilePath -> m Response
  serve filepath =
    FS.readFile filepath # try # liftAff >>= either readFailed readSucceeded
    where
    readSucceeded = ok' (contentTypeHeader filepath)

  readFailed :: Error -> m Response
  readFailed err
    | isMissing err = notFound
    -- a directory exists but has nothing to serve: index files and a trailing-slash redirect
    -- are not supported yet, so it answers like a missing file (this is where either would go)
    | isDirectoryError err = notFound
    -- EACCES/EMFILE/EIO are server faults; reporting them as 404 would hide real outages
    | otherwise = internalServerError

  -- the bodies are empty today; declaring a `Content-Type` anyway means a body added later can't be
  -- sniffed (an HTML body would also need `Content-Security-Policy`, to limit what the rendered page
  -- may load or run)
  badRequest = badRequest' plainHeaders ""
  forbidden = forbidden' plainHeaders
  notFound = notFound' plainHeaders
  internalServerError = internalServerError' plainHeaders ""

plainHeaders :: ResponseHeaders
plainHeaders =
  headers
    [ Tuple "Content-Type" "text/plain; charset=utf-8"
    , Tuple "X-Content-Type-Options" "nosniff"
    ]

-- | A filesystem failure meaning "nothing is at this path" rather than "this server is broken".
isMissing :: Error -> Boolean
isMissing err =
  -- Node's filesystem errors carry a string `code` naming what went wrong, otherwise `errorCode` is `Nothing`
  case errorCode err of
    -- the name is too long to look up at all
    Just "ENAMETOOLONG" -> true
    -- no entry with that name
    Just "ENOENT" -> true
    -- a component along the path is a file, so nothing can exist below it
    Just "ENOTDIR" -> true
    -- anything else can't claim the file is absent. Another code (`EACCES`, `EIO`,
    -- `ERR_FS_FILE_TOO_LARGE`, etc.) means the read failed on a file that may well exist.
    -- No code means the error was not raised by Node's filesystem layer, which attaches a
    -- code to every error it raises. Whatever threw it, it says nothing about the file
    _ -> false

-- | A filesystem failure meaning "something is at this path, but it is a directory, not a file".
-- | Not a case of `isMissing`, because the target exists. Both answer 404 today, for different
-- | reasons. Nothing `stat`s the path before the read, so this error is how a directory is noticed.
isDirectoryError :: Error -> Boolean
-- `EISDIR` is short for "is a directory", and is the one code Node uses for it
isDirectoryError err = errorCode err == Just "EISDIR"

-- Node never exports the `SystemError` class, so there is nothing to `instanceof` against and the
-- field is read off the plain `Error` at runtime, absent-tolerant
errorCode :: Error -> Maybe String
errorCode err = readProp "code" (unsafeToForeign err) >>= readString # runExcept >>> hush

-- | Always asserts a type, and forbids the browser from second-guessing it: with no `Content-Type`
-- | (or without `nosniff`) a file whose bytes look like markup would be sniffed as HTML and its
-- | scripts run in this origin. Unknown extensions become an opaque download instead.
-- | The type comes from the file on disk, never from the request.
contentTypeHeader :: FilePath -> ResponseHeaders
contentTypeHeader filepath =
  headers
    [ Tuple "Content-Type" contentType
    , Tuple "X-Content-Type-Options" "nosniff"
    ]
  where
  -- `charset` pins text types so a UTF-8 stylesheet or bundle isn't decoded against the referring page's encoding
  contentType = case toLower (Path.extname filepath) of
    ".css" -> "text/css; charset=utf-8"
    ".gif" -> "image/gif"
    ".html" -> "text/html; charset=utf-8"
    ".ico" -> "image/x-icon"
    ".jpeg" -> "image/jpeg"
    ".jpg" -> "image/jpeg"
    ".js" -> "text/javascript; charset=utf-8"
    ".json" -> "application/json; charset=utf-8"
    ".mjs" -> "text/javascript; charset=utf-8"
    ".png" -> "image/png"
    ".svg" -> "image/svg+xml"
    ".txt" -> "text/plain; charset=utf-8"
    ".wasm" -> "application/wasm"
    ".webp" -> "image/webp"
    ".woff" -> "font/woff"
    ".woff2" -> "font/woff2"
    _ -> "application/octet-stream"

-- | Why a request path was refused. Callers map these onto responses:
-- | `MalformedPath` is a 400, `EscapedRoot` a 403, `DotfileHidden` a 404, `DotfileDenied` a 403.
-- | The two dotfile refusals name the policy that refused: `Ignore` hides, `Deny` denies.
-- | Malformed percent-escapes don't appear here — routing-duplex rejects those while parsing the route.
data RequestPathError
  = MalformedPath
  | EscapedRoot
  | DotfileHidden
  | DotfileDenied

derive instance Eq RequestPathError
derive instance Generic RequestPathError _

instance Show RequestPathError where
  show = genericShow

-- | Resolves already-decoded request segments (as `routing-duplex`'s `rest` hands them over)
-- | against an already-resolved root, refusing anything that would land outside that root.
-- |
-- | The segments must arrive decoded: decoding again would turn a literal `%252e%252e`
-- | back into `..`, and would reject legitimate filenames containing a `%`.
-- |
-- | The request is checked *before* the root is joined, so the root's spelling (`/` vs
-- | `/srv/public`) never enters the verdict. Working on segments rather than a joined string
-- | means `..` is handled by popping, and the root can only be left by popping past empty.
-- |
-- | Pure — every step is a string operation, so nothing here touches the filesystem.
resolveRequestPath :: Dotfiles -> FilePath -> Array String -> Either RequestPathError FilePath
resolveRequestPath dotfiles resolvedRoot segments = do
  -- a fold, not a traversal: segments are dropped (`""`, `.`) and popped (`..`), not mapped one to one
  cleaned <- Array.foldM step [] segments
  -- checked on the surviving segments: `.git/../img` never touches `.git`, and only the request's
  -- own segments are inspected, never the root's, so a root under `~/.config` isn't refused
  checkDotfiles cleaned
  pure $ Path.concat $ resolvedRoot `Array.cons` cleaned
  where
  -- Invariant: `acc` holds plain names only (never "", ".", ".." or a separator), so
  -- `root <> acc` always denotes a location at or below `root`. A push goes one level
  -- down. A pop goes one level up, but only from a non-empty `acc`, i.e. only when we
  -- are strictly below `root`. Popping from `[]` is the one way to leave the root, and
  -- that case is `Left EscapedRoot`. So no fold result names anything above `root`, and
  -- `Path.concat` on separator-free names can't change that.
  step :: Array String -> String -> Either RequestPathError (Array String)
  step acc segment
    -- routing-duplex decodes `%00` straight through, so this guard is still load-bearing
    | contains (Pattern "\x0") segment = Left MalformedPath
    -- a decoded `%2F`/`%5C` would turn one URL segment into two path segments on disk, and the
    -- `..` guard below only sees whole segments: `..%2F..` would be pushed as one plain name and
    -- climb out once `Path.concat` joins it. The `\\` pattern is there because `\` is a
    -- separator on Windows
    | contains (Pattern "/") segment || contains (Pattern "\\") segment = Left MalformedPath
    -- `//` yields an interior empty; a trailing slash yields a trailing one (kept by `rest`)
    | segment == "" || segment == "." = Right acc
    -- nothing to pop means we are at the root, so going up would escape it
    | segment == ".." = Array.unsnoc acc # maybe (Left EscapedRoot) (_.init >>> Right)
    -- purely lexical: a symlink inside the root pointing outside it still passes, because following
    -- it would cost a `realpath` per request and make this function effectful
    | otherwise = Right $ acc `Array.snoc` segment

  -- the refusal names the policy, so the caller maps it to a status without consulting the policy again
  checkDotfiles :: Array String -> Either RequestPathError Unit
  checkDotfiles names
    | Array.any isDottedFile names = case dotfiles of
        Allow -> Right unit
        Deny -> Left DotfileDenied
        Ignore -> Left DotfileHidden
    | otherwise = Right unit

  -- `.` and `..` never reach `cleaned`, so a leading dot is enough
  isDottedFile :: String -> Boolean
  isDottedFile = stripPrefix (Pattern ".") >>> isJust

-- | Why the root can't be served from. The path is the resolved one, so the message names
-- | the directory actually looked for rather than what was typed.
data RootError
  = EmptyRoot
  | MissingRoot FilePath
  | NotADirectory FilePath

derive instance Eq RootError
derive instance Generic RootError _

instance Show RootError where
  show = genericShow

-- | Resolves the root to a single, unambiguous location. Callers run this at startup and refuse
-- | to boot on `Left`, so a mistyped root fails immediately instead of 404ing every asset at runtime.
resolveRoot :: FilePath -> Effect (Either RootError FilePath)
resolveRoot root
  -- the only input `stat` can't catch: `resolve` turns `""` into `cwd`, which exists
  | null root = pure $ Left EmptyRoot
  | otherwise = do
      -- Node's `path.resolve`, bound in `Effect` because it consults `cwd` for a relative path
      resolvedRoot <- Path.resolve [] root
      -- `resolve` is a string-only operation, so existence needs a `stat`; once per startup, not per request
      Sync.stat resolvedRoot # try <#> case _ of
        -- an unreadable root (EACCES) is as unservable as an absent one; the path says which it was
        Left _ -> Left $ MissingRoot resolvedRoot
        Right s
          | Stats.isDirectory s -> Right resolvedRoot
          | otherwise -> Left $ NotADirectory resolvedRoot
