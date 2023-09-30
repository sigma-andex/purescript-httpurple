module Examples.MultiRoute.Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Request, Response, ServerM, ok, serve)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Routing.Duplex.Generic.Syntax ((/))

data Route = Hello | GoodBye

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ RG.sum
  { "Hello": "hello" / RG.noArgs
  , "GoodBye": "goodbye" / RG.noArgs
  }

-- | Specify the routes
router :: Request Route -> Aff Response
router { route: Hello } = ok "hello"
router { route: GoodBye } = ok "goodbye"

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌────────────────────────────────┐"
    log " │ Server now up on port 8080     │"
    log " │                                │"
    log " │ To test, run:                  │"
    log " │  > curl localhost:8080/hello   │"
    log " │    # => hello                  │"
    log " │  > curl localhost:8080/goodbye │"
    log " │    # => goodbye                │"
    log " └────────────────────────────────┘"
