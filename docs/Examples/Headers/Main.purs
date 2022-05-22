module Examples.Headers.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import HTTPurple (Headers, Request, ResponseM, ServerM, header, ok', serve, (!@))
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }


-- | The headers that will be included in every response.
responseHeaders :: Headers
responseHeaders = header "X-Example" "hello world!"

-- | Route to the correct handler
router :: Request Route -> ResponseM
router { headers } = ok' responseHeaders $ headers !@ "X-Input"

-- | Boot up the server
main :: ServerM
main =
  serve 8080 { route, router, notFoundHandler: Nothing} do
    log " ┌──────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                   │"
    log " │                                              │"
    log " │ To test, run:                                │"
    log " │  > curl -H 'X-Input: test' -v localhost:8080 │"
    log " │    # => ...                                  │"
    log " │    # => ...< X-Example: hello world!         │"
    log " │    # => ...                                  │"
    log " │    # => test                                 │"
    log " └──────────────────────────────────────────────┘"
