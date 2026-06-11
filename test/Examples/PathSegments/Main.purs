module Examples.PathSegments.Main where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Request, Response, ServerM, ok, serve)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

data Route = Segment String | ManySegments (Array String)

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Segment": "segment" / RD.segment
  , "ManySegments": RD.many RD.segment :: RD.RouteDuplex' (Array String)
  }

-- | Specify the routes
router :: Request Route -> Aff Response
router { route: Segment elem } = ok elem
router { route: ManySegments elems } = ok $ show elems

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌───────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                    │"
    log " │                                               │"
    log " │ To test, run:                                 │"
    log " │  > curl localhost:8080/segment/<anything>     │"
    log " │    # => <anything>                            │"
    log " │  > curl localhost:8080/<anything>/<else>/...  │"
    log " │    # => [ <anything>, <else>, ... ]           │"
    log " └───────────────────────────────────────────────┘"
