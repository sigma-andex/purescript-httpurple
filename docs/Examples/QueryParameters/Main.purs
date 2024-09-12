module Examples.QueryParameters.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPurple (Request, Response, ServerM, notFound, ok, serve)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G

data Route = Route { foo :: Boolean, bar :: Maybe String, baz :: Maybe String }

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Route": RD.params { foo: RD.flag <<< RD.string, bar: RD.optional <<< RD.string, baz: RD.optional <<< RD.string }
  }

-- | Specify the routes
router :: Request Route -> Aff Response
router { route: (Route { foo: true }) } = ok "foo"
router { route: (Route { bar: Just "test" }) } = ok "bar"
router { route: (Route { bar: Just _ }) } = ok ""
router { route: Route { baz: Just baz } } = ok $ baz
router _ = notFound

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted :: Effect Unit
  onStarted = do
    log " ┌───────────────────────────────────────┐"
    log " │ Server now up on port 8080            │"
    log " │                                       │"
    log " │ To test, run:                         │"
    log " │  > curl localhost:8080?foo            │"
    log " │    # => foo                           │"
    log " │  > curl localhost:8080?bar=test       │"
    log " │    # => bar                           │"
    log " │  > curl localhost:8080?baz=<anything> │"
    log " │    # => <anything>                    │"
    log " └───────────────────────────────────────┘"
