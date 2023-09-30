module Examples.Headers.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Request, ResponseHeaders, Response, ServerM, ok', serve, (!@))
import HTTPurple.Headers (headers)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

-- | The headers that will be included in every response.
responseHeaders :: ResponseHeaders
responseHeaders = headers
  { "X-Example": "hello world!"
  , "X-Example2": "hello world!"
  }

-- | Route to the correct handler
router :: Request Route -> Aff Response
router { headers } = ok' responseHeaders $ headers !@ "X-Input"

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
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
