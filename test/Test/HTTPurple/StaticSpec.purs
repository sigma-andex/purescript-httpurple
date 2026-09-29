module Test.HTTPurple.StaticSpec (staticSpec) where

import Prelude hiding ((/))

import Data.Either (Either(..), either, isLeft, isRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign.Object (delete, empty, member)
import HTTPurple.Response (Response)
import HTTPurple.Routes (mkRoute)
import HTTPurple.Static (Dotfiles(..), RequestPathError(..), RootError(..), resolveRequestPath, resolveRoot, serveStatic)
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.Types (IMClientRequest, IncomingMessage)
import Node.Path as Path
import Routing.Duplex as RD
import Routing.Duplex.Generic.Syntax ((/))
import Test.HTTPurple.TestHelpers (Test, extractHeader, request', serveAwaitClosed, serveAwaitStarted, toString, (?=))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)

type StaticHandler = { asset :: Array String } -> Aff Response

data Route = Asset (Array String)

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = mkRoute { "Asset": "static" / RD.rest }

-- | The fixture tree: `hello.txt` (13 bytes), `subdir/nested.txt`, `unknown.ext` and `.secret`.
fixtureRoot :: String
fixtureRoot = "test/Mocks/public"

-- | Boots a server that serves the fixture tree under the given dotfile policy (`Nothing` leaves
-- | `serveStatic`'s default in place), runs the test against its port, and shuts it down.
withStaticServer' :: Maybe Dotfiles -> (Int -> Aff Unit) -> Aff Unit
withStaticServer' dotfiles test = do
  serveAsset <- serveStatic { root: fixtureRoot, dotfiles }
    >>= either (show >>> append "static root: " >>> throw) pure
    # liftEffect
  let router { route: Asset asset } = serveAsset { asset }
  bracket
    (serveAwaitStarted { hostname: "localhost", port } { route, router })
    serveAwaitClosed
    \_ -> test port
  where
  port = 8080

withStaticServer :: (Int -> Aff Unit) -> Aff Unit
withStaticServer = withStaticServer' Nothing

-- | A plain `GET` against the test server, keeping the raw response so that status, headers and
-- | body can all be checked on the same reply.
get :: Int -> String -> Aff (IncomingMessage IMClientRequest)
get port = request' false port "GET" empty

-- | The assertion every refusal test ends with: the status, and the headers along with it, so that
-- | a refusal can't be checked for one without the other.
refusedWithCode :: IncomingMessage IMClientRequest -> Int -> Aff Unit
refusedWithCode response status = do
  IM.statusCode response ?= status
  expectPlainText
  where
  -- pins `plainHeaders`: every response, refusals included, carries a `Content-Type` and `nosniff`,
  -- so an empty error body can't be sniffed into something else if a body is ever added
  expectPlainText = do
    extractHeader "Content-Type" response ?= "text/plain; charset=utf-8"
    extractHeader "X-Content-Type-Options" response ?= "nosniff"

-- | The handler's branches, driven through the router so that what routing-duplex does to the URL
-- | (decoding, splitting) is part of what's pinned. The refusals carry no body, so status and
-- | headers are all there is to check; every refusal is checked for both.
-- |
-- | The 404/500 split is Node's, not the handler's: `readFile` rejects with a `code`. `ENOENT`
-- | (missing), `ENOTDIR` (a path through a file) and `ENAMETOOLONG` mean "nothing at this path";
-- | `EISDIR` (a directory) is checked apart, since the target exists, but answers 404 too;
-- | anything else (`EACCES`, `EMFILE`, `EIO`) is a 500.
-- | The 500 case isn't tested: provoking it needs a `chmod 000` fixture, which is readable
-- | anyway when CI runs as root.
handlerSpec :: Test
handlerSpec =
  describe "handler" do
    describe "served files" do
      it "serves a nested file with the type of its extension" do
        withStaticServer \port -> do
          response <- get port "/static/subdir/nested.txt"
          IM.statusCode response ?= 200
          extractHeader "Content-Type" response ?= "text/plain; charset=utf-8"
          extractHeader "X-Content-Type-Options" response ?= "nosniff"
          body <- toString response
          body ?= "nested\n"
      it "serves an unknown extension as an opaque download" do
        withStaticServer \port -> do
          response <- get port "/static/unknown.ext"
          IM.statusCode response ?= 200
          extractHeader "Content-Type" response ?= "application/octet-stream"
          extractHeader "X-Content-Type-Options" response ?= "nosniff"
          body <- toString response
          body ?= "opaque\n"

    describe "misses" do
      it "answers 404 for a missing file" do
        withStaticServer \port -> do
          response <- get port "/static/missing.txt"
          response `refusedWithCode` 404
      -- `EISDIR`: index files are not supported yet, so a directory has nothing to serve
      it "answers 404 for a directory" do
        withStaticServer \port -> do
          response <- get port "/static/subdir"
          response `refusedWithCode` 404
      -- `/static` and `/static/` both name the root, but they arrive differently: `rest` yields `[]`
      -- for the first and `[""]` for the second, and only the handler's dropping of empty segments
      -- makes them meet. Two requests, so a change on either side shows. The root is a directory too
      it "answers 404 for the root, with and without a trailing slash" do
        withStaticServer \port -> do
          bare <- get port "/static"
          bare `refusedWithCode` 404
          slashed <- get port "/static/"
          slashed `refusedWithCode` 404
      -- `ENOTDIR`: a path that treats a file as a directory
      it "answers 404 for a path through a file" do
        withStaticServer \port -> do
          response <- get port "/static/hello.txt/nested.txt"
          response `refusedWithCode` 404
      -- `ENAMETOOLONG`: 4500 bytes is past the whole-path limit (1024 on macOS, 4096 on Linux), so the
      -- kernel refuses the name before any filesystem, whatever that filesystem's per-name limit, is asked
      it "answers 404 for a name longer than any path may be" do
        withStaticServer \port -> do
          response <- get port ("/static/" <> power "abc" 1500 <> ".txt")
          response `refusedWithCode` 404

    describe "refusals" do
      -- 403 rather than 404: the answer must not depend on whether the target exists
      it "answers 403 for a traversal out of the root" do
        withStaticServer \port -> do
          response <- get port "/static/../hello.txt"
          response `refusedWithCode` 403
      -- the escape is refused, not its destination: this path leaves the root and comes straight
      -- back to a file that exists, and is still a 403
      it "answers 403 for a traversal that escapes and re-enters the root" do
        withStaticServer \port -> do
          response <- get port "/static/../public/hello.txt"
          response `refusedWithCode` 403
      -- routing-duplex decodes `%00` straight through
      it "answers 400 for a null byte" do
        withStaticServer \port -> do
          response <- get port "/static/hello%00.txt"
          response `refusedWithCode` 400
      -- routing-duplex splits before decoding, so `%2F` lands inside one segment rather than
      -- making two; decoding first and splitting second would have served `subdir/nested.txt`
      it "answers 400 for an encoded slash" do
        withStaticServer \port -> do
          response <- get port "/static/subdir%2Fnested.txt"
          response `refusedWithCode` 400
      -- `%2e%2e` decodes to `..`, so a 403 proves the router decoded at least once: not decoded, it
      -- would be a plain filename and a 404. The next test pins that it decodes no more than once
      it "decodes `%2e%2e` into `..`, a 403" do
        withStaticServer \port -> do
          response <- get port "/static/%2e%2e/hello.txt"
          response `refusedWithCode` 403
      -- `%252e%252e` decodes to the literal filename `%2e%2e`, which doesn't exist: a 404. Decoded
      -- twice it would be `..` and a 403 (forbidden), so the two tests together pin "decoded exactly once"
      it "decodes `%252e%252e` only once, into the plain name `%2e%2e`, a 404" do
        withStaticServer \port -> do
          response <- get port "/static/%252e%252e/hello.txt"
          response `refusedWithCode` 404

    describe "dotfiles" do
      -- indistinguishable from a missing file, so a probe can't tell hidden from absent
      it "answers 404 by default" do
        withStaticServer \port -> do
          response <- get port "/static/.secret"
          response `refusedWithCode` 404
      it "answers 403 under `Deny`" do
        withStaticServer' (Just Deny) \port -> do
          response <- get port "/static/.secret"
          response `refusedWithCode` 403
      it "serves it under `Allow`" do
        withStaticServer' (Just Allow) \port -> do
          response <- get port "/static/.secret"
          IM.statusCode response ?= 200
          body <- toString response
          body ?= "hidden\n"

-- | Nothing in `serveStatic` looks at the method, so a `HEAD` response is whatever Node makes of the
-- | `GET` one. Node drops the body on its own, but it only emits `Content-Length` on a `HEAD` if the
-- | handler set the header explicitly; the length Node would infer from the buffer is skipped. This
-- | pins that HTTPurple's `Buffer` body does set it, so `HEAD` gets `GET`'s headers and no body.
headSpec :: Test
headSpec =
  describe "HEAD" do
    it "answers with GET's headers and an empty body" do
      withStaticServer \port -> do
        viaGet <- get port "/static/hello.txt"
        viaHead <- request' false port "HEAD" empty "/static/hello.txt"
        IM.statusCode viaHead ?= 200
        -- `hello.txt` is 13 bytes; the value must come from the header, not the (absent) body
        extractHeader "Content-Length" viaHead ?= "13"
        -- `Date` is Node's, stamped per response to the second: comparing its value would make the test
        -- flaky, failing whenever the two requests are answered in different seconds. So it is compared
        -- by presence, and every other header by value
        member "date" (IM.headers viaHead) ?= member "date" (IM.headers viaGet)
        delete "date" (IM.headers viaHead) ?= delete "date" (IM.headers viaGet)
        headBody <- toString viaHead
        headBody ?= ""
        getBody <- toString viaGet
        getBody ?= "hello, world\n"

serveStaticSpec :: Test
serveStaticSpec =
  describe "serveStatic" do
    -- `root` is the only required field; `dotfiles` is filled with its default
    it "accepts just a root" do
      handler :: _ _ StaticHandler <- serveStatic { root: fixtureRoot } # liftEffect
      isRight handler ?= true
    it "accepts optional fields as plain values" do
      handler :: _ _ StaticHandler <- serveStatic { root: fixtureRoot, dotfiles: Allow } # liftEffect
      isRight handler ?= true
    it "refuses to start on an unusable root" do
      handler :: _ _ StaticHandler <- serveStatic { root: fixtureRoot <> "/hello.txt" } # liftEffect
      isLeft handler ?= true

resolveRootSpec :: Test
resolveRootSpec =
  describe "resolveRoot" do
    it "rejects an empty root" do
      resolved <- resolveRoot "" # liftEffect
      resolved ?= Left EmptyRoot
    -- asserting the exact path would just pin down the test runner's `cwd`
    it "resolves a relative root to an absolute path" do
      -- the test is only meaningful if the input is in fact relative
      Path.isAbsolute fixtureRoot ?= false
      resolved <- resolveRoot fixtureRoot # liftEffect
      Path.isAbsolute <$> resolved ?= Right true
    it "rejects a root that doesn't exist, naming the resolved path" do
      resolved <- resolveRoot (fixtureRoot <> "/missing") # liftEffect
      case resolved of
        Left (MissingRoot path) -> Path.isAbsolute path ?= true
        other -> fail $ "expected MissingRoot, got " <> show other
    it "rejects a root that is a file rather than a directory" do
      resolved <- resolveRoot (fixtureRoot <> "/hello.txt") # liftEffect
      case resolved of
        Left (NotADirectory path) -> Path.isAbsolute path ?= true
        other -> fail $ "expected NotADirectory, got " <> show other

resolveRequestPathSpec :: Test
resolveRequestPathSpec =
  describe "resolveRequestPath" do
    let resolve = resolveRequestPath Ignore "/srv/public"

    it "accepts a nested path" do
      resolve [ "img", "logo.png" ] ?= Right "/srv/public/img/logo.png"
    -- the read stage 404s it for being a directory
    it "accepts the root itself" do
      resolve [] ?= Right "/srv/public"
    -- `rest` keeps a trailing slash as a trailing empty segment; `//` yields an interior one
    it "ignores empty segments" do
      resolve [ "", "img", "" ] ?= Right "/srv/public/img"
    it "ignores `.` segments" do
      resolve [ ".", "img", ".", "logo.png" ] ?= Right "/srv/public/img/logo.png"
    it "collapses interior `..` segments that stay inside the root" do
      resolve [ "img", "..", "logo.png" ] ?= Right "/srv/public/logo.png"
    -- a string check like `startsWith (root <> "/")` would report a false escape for everything under `/`
    -- (no path starts with `//`); the verdict comes from the segments alone and the root is joined after
    it "serves from a filesystem root" do
      resolveRequestPath Ignore "/" [ "etc", "passwd" ] ?= Right "/etc/passwd"

    it "refuses a traversal out of the root" do
      resolve [ "..", "etc", "passwd" ] ?= Left EscapedRoot
    -- popping past the root is an escape even if later segments would land back inside it
    it "refuses a traversal that re-enters the root" do
      resolve [ "..", "public", "logo.png" ] ?= Left EscapedRoot
    it "refuses a sibling directory sharing the root's prefix" do
      resolve [ "..", "public-secrets", "keys" ] ?= Left EscapedRoot
    it "refuses an embedded null byte" do
      resolve [ "logo\x0.png" ] ?= Left MalformedPath
    -- routing-duplex decodes `%2F`/`%5C` into a literal separator inside a single segment
    it "refuses a segment containing a forward slash" do
      resolve [ "img/logo.png" ] ?= Left MalformedPath
    it "refuses a segment containing a backslash" do
      resolve [ "img\\logo.png" ] ?= Left MalformedPath

    -- routing-duplex has already decoded by this point, so escapes are literal
    -- text here; decoding them again would revive the traversal
    it "treats a percent-encoded traversal as a literal filename" do
      resolve [ "%2e%2e", "etc", "passwd" ] ?= Right "/srv/public/%2e%2e/etc/passwd"
    it "keeps a literal percent in a filename" do
      resolve [ "50%.png" ] ?= Right "/srv/public/50%.png"

    it "refuses a dotfile" do
      resolve [ ".env" ] ?= Left DotfileHidden
    it "refuses a path through a dotted directory" do
      resolve [ ".git", "config" ] ?= Left DotfileHidden
    -- the refusal names the policy, which is what lets the handler answer 403 rather than 404
    it "refuses a dotfile under `Deny`, as denied rather than hidden" do
      resolveRequestPath Deny "/srv/public" [ ".env" ] ?= Left DotfileDenied
    it "serves a dotfile under `Allow`" do
      resolveRequestPath Allow "/srv/public" [ ".env" ] ?= Right "/srv/public/.env"
    -- the dotfile rule looks at where a request ends up, not at every segment along the way:
    -- this one collapses to `["img"]` and never enters `.git`
    it "ignores a dotted segment cancelled by `..`" do
      resolve [ ".git", "..", "img" ] ?= Right "/srv/public/img"
    -- only the request's own segments are inspected, never the root's
    it "does not trip on a root that sits under a dotted directory" do
      resolveRequestPath Ignore "/home/me/.config/site" [ "img" ] ?= Right "/home/me/.config/site/img"

staticSpec :: Test
staticSpec =
  describe "Static" do
    serveStaticSpec
    resolveRootSpec
    resolveRequestPathSpec
    handlerSpec
    headSpec
