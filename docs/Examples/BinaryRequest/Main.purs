module Examples.BinaryRequest.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (Request, Response, ServerM, ok, serve, toBuffer)
import Node.Buffer (Buffer)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

foreign import sha256sum :: Buffer -> String

-- | Respond with file's sha256sum
router :: Request Route -> Aff Response
router { body } = toBuffer body >>= sha256sum >>> ok

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  onStarted = do
    log " ┌─────────────────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                              │"
    log " │                                                         │"
    log " │ To test, run:                                           │"
    log " │  > curl -XPOST --data-binary @circle.png localhost:8080 │"
    log " │                          # => d5e776724dd5...           │"
    log " └─────────────────────────────────────────────────────────┘"
