module Examples.Post.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Method(Post), Request, Response, ServerM, notFound, ok, serve, toString)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G

data Route = Test

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Test": G.noArgs
  }

-- | Route to the correct handler
router :: Request Route -> Aff Response
router { body, method: Post } = toString body >>= ok
router _ = notFound

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌───────────────────────────────────────────┐"
    log " │ Server now up on port 8080                │"
    log " │                                           │"
    log " │ To test, run:                             │"
    log " │  > curl -XPOST --data test localhost:8080 │"
    log " │    # => test                              │"
    log " └───────────────────────────────────────────┘"
