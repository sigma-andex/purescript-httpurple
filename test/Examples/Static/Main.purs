module Examples.Static.Main where

import Prelude hiding ((/))

import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Exception (throw)
import HTTPurple (Request, Response, ServerM, serve, serveStatic)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Routing.Duplex.Generic.Syntax ((/))

data Route = Home | Asset (Array String)

derive instance Generic Route _

-- | Everything under `/static/` is an asset; `rest` captures the remaining segments
route :: RouteDuplex' Route
route = RD.root $ RG.sum
  { "Home": RG.noArgs
  , "Asset": "static" / RD.rest
  }

-- | The directory served. The server refuses to start if it doesn't exist.
root :: String
root = "./docs/Examples/Static/public"

-- | Specify the routes. The home page is a route clause pointing at a file:
-- | `serveStatic` has no index-file lookup, the router decides which URLs are documents.
router :: ({ asset :: Array String } -> Aff Response) -> Request Route -> Aff Response
router serveAsset { route: Home } = serveAsset { asset: [ "index.html" ] }
router serveAsset { route: Asset asset } = serveAsset { asset }

-- | Boot up the server
main :: ServerM
main = do
  -- resolved once at startup, then reused by every request
  serveAsset <- serveStatic { root } >>= either (show >>> throw) pure
  serve { hostname: "localhost", port: 8080, onStarted } { route, router: router serveAsset }
  where
  onStarted = do
    log " ┌────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                 │"
    log " │                                            │"
    log " │ To test, run:                              │"
    log " │  > curl localhost:8080                     │"
    log " │    # => the index.html page                │"
    log " │  > curl localhost:8080/static/style.css    │"
    log " │    # => the stylesheet                     │"
    log " │  > curl -I localhost:8080/static/.secret   │"
    log " │    # => 404, dotfiles are hidden           │"
    log " └────────────────────────────────────────────┘"
