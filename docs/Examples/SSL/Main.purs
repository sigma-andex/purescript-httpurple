module Examples.SSL.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import HTTPurple (Request, ResponseM, ServerM, ok, serve)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic as RG

data Route = Test

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Test": RG.noArgs
  }

-- | The path to the certificate file
cert :: String
cert = "./docs/Examples/SSL/Certificate.cer"

-- | The path to the key file
key :: String
key = "./docs/Examples/SSL/Key.key"

-- | Say 'hello world!' when run
sayHello :: Request Route -> ResponseM
sayHello _ = ok "hello world!"

-- | Boot up the server
main :: ServerM
main =
  serve { port: 8080, certFile: cert, keyFile: key, onStarted } { route, router: sayHello } 
  where 
    onStarted = 
      do
        log " ┌───────────────────────────────────────────┐"
        log " │ Server now up on port 8080                │"
        log " │                                           │"
        log " │ To test, run:                             │"
        log " │  > curl --insecure https://localhost:8080 │"
        log " │    # => hello world!                      │"
        log " └───────────────────────────────────────────┘"
