module Test.HTTPurple.TestHelpers where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (untilJust)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.String (toLower)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, delay, effectCanceler, makeAff, nonCanceler)
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object (Object, lookup)
import Foreign.Object (empty, fromFoldable) as Object
import HTTPurple.Server (BasicRoutingSettings, ListenOptionsR, serve)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Node.Buffer (Buffer, create, fromString)
import Node.Buffer (alloc, concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.EventEmitter (EventHandle(..), once_)
import Node.EventEmitter.UtilTypes (EventHandle1)
import Node.HTTP as HTTP
import Node.HTTP.ClientRequest as HTTPClient
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.OutgoingMessage as OM
import Node.HTTP.Types (IMClientRequest, IncomingMessage, ServerResponse)
import Node.HTTPS as HTTPS
import Node.Net.Socket as Socket
import Node.Net.Types (Socket)
import Node.Stream (Readable)
import Node.Stream as Stream
import Node.Stream.Aff (readableToBuffers)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as Record
import Test.Spec (Spec)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

awaitStarted :: Int -> Aff Unit
awaitStarted port =
  void $ untilJust do
    delay $ wrap 10.0
    b <- liftEffect $ Buffer.alloc 0
    hush <$> try (request false port "GET" Object.empty "/" b)

awaitStartedSecure :: Int -> Aff Unit
awaitStartedSecure port =
  void $ untilJust do
    delay $ wrap 10.0
    b <- liftEffect $ Buffer.alloc 0
    hush <$> try (request true port "GET" Object.empty "/" b)

serveAwaitStarted ::
  forall route from fromRL via missing missingList.
  RowToList missing missingList =>
  FillableFields missingList () missing =>
  Union via missing (ListenOptionsR Aff) =>
  RowToList (onStarted :: Maybe (Effect Unit) | from) fromRL =>
  Lacks "onStarted" from =>
  -- Cons "onStarted" (Maybe (Effect Unit)) from (onStarted :: Maybe (Effect Unit) | from) =>
  JustifiableFields fromRL (onStarted :: Maybe (Effect Unit) | from) () via =>
  { | from } ->
  BasicRoutingSettings Aff route ->
  Aff (Effect Unit -> Effect Unit)
serveAwaitStarted cfg rt =
  makeAff \done -> do
    stopRef <- Ref.new Nothing
    let
      onStarted = do
        mstop <- Ref.read stopRef
        done $ Right $ fromMaybe (const $ pure unit) mstop
      opts = Record.insert (Proxy @"onStarted") (Just onStarted) cfg
    stop <- serve opts rt
    Ref.write (Just stop) stopRef
    pure $ effectCanceler $ stop $ pure unit

infix 1 shouldEqual as ?=

-- | The type for integration tests.
type Test = Spec Unit

-- | The type for the entire test suite.
type TestSuite = Effect Unit

-- | Given a URL, a failure handler, and a success handler, create an HTTP
-- | client request.
request ::
  Boolean ->
  Int ->
  String ->
  Object String ->
  String ->
  Buffer ->
  Aff (IncomingMessage IMClientRequest)
request secure port' method' headers' path' body =
  makeAff \done -> do
    req <- case secure of
      true -> HTTPS.requestOpts
        { method: method'
        , host: "localhost"
        , port: port'
        , path: path'
        , headers: unsafeCoerce headers' :: Object Foreign
        , rejectUnauthorized: false
        }
      false -> HTTP.requestOpts
        { method: method'
        , host: "localhost"
        , port: port'
        , path: path'
        , headers: unsafeCoerce headers' :: Object Foreign
        }
    let
      connectionAttemptFailedH :: forall t. EventHandle1 (Socket t) Error
      connectionAttemptFailedH = EventHandle "connectionAttemptFailed" mkEffectFn1

    req # once_ HTTPClient.socketH
      ( \socket ->
          once_ Stream.errorH (Left >>> done) (Socket.toDuplex socket)
            *> once_ connectionAttemptFailedH (Left >>> done) socket
      )
    req # once_ HTTPClient.responseH (Right >>> done)
    let stream = OM.toWriteable $ HTTPClient.toOutgoingMessage req
    void
      $ Stream.write' stream body
      $ const
      $ Stream.end stream
    pure nonCanceler

-- | Same as `request` but without.
request' ::
  Boolean ->
  Int ->
  String ->
  Object String ->
  String ->
  Aff (IncomingMessage IMClientRequest)
request' secure port method headers path =
  liftEffect (create 0)
    >>= request secure port method headers path

-- | Same as `request` but with a `String` body.
requestString ::
  Boolean ->
  Int ->
  String ->
  Object String ->
  String ->
  String ->
  Aff (IncomingMessage IMClientRequest)
requestString secure port method headers path body = do
  liftEffect (fromString body UTF8)
    >>= request secure port method headers path

-- | Convert a request to an Aff containing the `Buffer with the response body.
toBuffer :: IncomingMessage IMClientRequest -> Aff Buffer
toBuffer response = do
  bufs <- readableToBuffers $ IM.toReadable response
  liftEffect $ Buffer.concat bufs

-- | Convert a request to an Aff containing the string with the response body.
toString :: IncomingMessage IMClientRequest -> Aff String
toString resp = do
  buf <- toBuffer resp
  liftEffect $ Buffer.toString UTF8 buf

-- | Run an HTTP GET with the given url and return an Aff that contains the
-- | string with the response body.
get ::
  Int ->
  Object String ->
  String ->
  Aff String
get port headers path = request' false port "GET" headers path >>= toString

-- | Like `get` but return a response body in a `Buffer`
getBinary ::
  Int ->
  Object String ->
  String ->
  Aff Buffer
getBinary port headers path = request' false port "GET" headers path >>= toBuffer

-- | Run an HTTPS GET with the given url and return an Aff that contains the
-- | string with the response body.
get' ::
  Int ->
  Object String ->
  String ->
  Aff String
get' port headers path = request' true port "GET" headers path >>= toString

-- | Run an HTTP POST with the given url and body and return an Aff that
-- | contains the string with the response body.
post ::
  Int ->
  Object String ->
  String ->
  String ->
  Aff String
post port headers path = requestString false port "POST" headers path >=> toString

-- | Run an HTTP POST with the given url and binary buffer body and return an
-- | Aff that contains the string with the response body.
postBinary ::
  Int ->
  Object String ->
  String ->
  Buffer ->
  Aff String
postBinary port headers path = request false port "POST" headers path >=> toString

-- | Convert a request to an Aff containing the string with the given header
-- | value.
extractHeader :: String -> IncomingMessage IMClientRequest -> String
extractHeader header = unmaybe <<< lookup' <<< IM.headers
  where
  unmaybe = fromMaybe ""

  lookup' = lookup $ toLower header

-- | Run an HTTP GET with the given url and return an Aff that contains the
-- | string with the header value for the given header.
getHeader ::
  Int ->
  Object String ->
  String ->
  String ->
  Aff String
getHeader port headers path header = extractHeader header <$> request' false port "GET" headers path

getStatus ::
  Int ->
  Object String ->
  String ->
  Aff Int
getStatus port headers path = IM.statusCode <$> request' false port "GET" headers path

-- | Mock an HTTP Request object
foreign import mockRequestImpl ::
  forall a.
  String ->
  String ->
  String ->
  String ->
  Object String ->
  Effect (IncomingMessage a)

-- | Mock an HTTP Request object
mockRequest ::
  forall a.
  String ->
  String ->
  String ->
  String ->
  Array (Tuple String String) ->
  Aff (IncomingMessage a)
mockRequest httpVersion method url body = liftEffect <<< mockRequestImpl httpVersion method url body <<< Object.fromFoldable

-- | Mock an HTTP Response object
foreign import mockResponse :: Effect ServerResponse

-- | Get the current body from an HTTP Response object (note this will only work
-- | with an object returned from mockResponse).
getResponseBody :: ServerResponse -> String
getResponseBody = _.body <<< unsafeCoerce

-- | Get the currently set status from an HTTP Response object.
getResponseStatus :: ServerResponse -> Int
getResponseStatus = _.statusCode <<< unsafeCoerce

-- | Get all current headers on the HTTP Response object.
getResponseHeaders :: ServerResponse -> Object (Array String)
getResponseHeaders = unsafeCoerce <<< _.headers <<< unsafeCoerce

-- | Get the current value for the header on the HTTP Response object.
getResponseHeader :: String -> ServerResponse -> Array String
getResponseHeader header = fromMaybe [ "" ] <<< lookup header <<< getResponseHeaders

-- | Create a stream out of a string.
foreign import stringToStream :: String -> Readable ()
