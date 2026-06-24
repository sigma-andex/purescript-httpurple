module Examples.JsonParsing.Main where

import Prelude

import Data.Either (Either)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect.Console (log)
import HTTPurple (JsonDecoder(..), JsonEncoder(..), ServerM, fromJson, notFound, serve, toJson, usingCont)
import HTTPurple as Json
import HTTPurple.Method (Method(..))
import HTTPurple.Response (ok')
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

type HelloWorldRequest = { name :: String }
type HelloWorldResponse = { hello :: String }

-- the following test decoder/encoder code is just for testing. in your project you will want to use
-- jsonEncoder and jsonDecoder from httpurple-argonaut or httpurple-yoga-json
foreign import data Json :: Type

foreign import parseJson :: String -> Maybe Json

foreign import getName :: Json -> Maybe String

testDecoder :: JsonDecoder String HelloWorldRequest
testDecoder = JsonDecoder fromJsonString
  where
  fromJsonString :: String -> Either String HelloWorldRequest
  fromJsonString = (parseJson >=> getName) >>> map { name: _ } >>> Either.note "Invalid json"

testEncoder :: JsonEncoder HelloWorldResponse
testEncoder = JsonEncoder $ \{ hello } -> "{\"hello\": \"" <> hello <> "\" }"

-- | Boot up the server
main :: ServerM
main =
  serve { hostname: "localhost", port: 8080, onStarted } { route, router }
  where
  router { route: SayHello, method: Post, body } = usingCont do
    -- in your project you will want to use Argonaut.jsonDecoder from httpurple-argonaut
    -- or Yoga.jsonDecoder from httpurple-yoga-json here instead of the testDecoder
    { name } :: HelloWorldRequest <- fromJson testDecoder body
    ok' Json.jsonHeaders $ toJson testEncoder $ { hello: name } -- same here for the encoder
  router { route: SayHello } = notFound

  onStarted = do
    log " ┌────────────────────────────────────────────┐"
    log " │ Server now up on port 8080                 │"
    log " │                                            │"
    log " │ To test, run:                              │"
    log " │  > http -v POST localhost:8080 hello=world │"
    log " | # => { \"hello\": \"world\" }                  │"
    log " └────────────────────────────────────────────┘"
