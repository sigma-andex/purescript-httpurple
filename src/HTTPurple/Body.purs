module HTTPurple.Body
  ( class Body
  , RequestBody
  , defaultHeaders
  , write
  , read
  , toBuffer
  , toStream
  , toString
  ) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HTTPurple.Headers (RequestHeaders, mkRequestHeader)
import Node.Buffer (Buffer, fromString, size)
import Node.Buffer (concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.EventEmitter (once_)
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.OutgoingMessage as OM
import Node.HTTP.ServerResponse as SR
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Node.Stream (Readable, Stream, end', pipe, writeString')
import Node.Stream (endH, write') as Stream
import Node.Stream.Aff (readableToBuffers)
import Type.Equality (class TypeEquals, to)

type RequestBody =
  { buffer :: Ref (Maybe Buffer)
  , stream :: Readable ()
  , string :: Ref (Maybe String)
  }

-- | Read the body `Readable` stream out of the incoming request
read :: IncomingMessage IMServer -> Effect RequestBody
read request = do
  buffer <- Ref.new Nothing
  string <- Ref.new Nothing
  pure
    { buffer
    , stream: IM.toReadable request
    , string
    }

-- | Turn `RequestBody` into a `String`
-- |
-- | This drains the `Readable` stream in `RequestBody` for the first time
-- | and returns cached result from then on.
toString :: forall m. MonadAff m => RequestBody -> m String
toString requestBody = do
  maybeString <-
    liftEffect
      $ Ref.read requestBody.string
  case maybeString of
    Nothing -> do
      buffer <- toBuffer requestBody
      string <- liftEffect
        $ Buffer.toString UTF8 buffer
      liftEffect
        $ Ref.write (Just string) requestBody.string
      pure string
    Just string -> pure string

-- | Turn `RequestBody` into a `Buffer`
-- |
-- | This drains the `Readable` stream in `RequestBody` for the first time
-- | and returns cached result from then on.
toBuffer :: forall m. MonadAff m => RequestBody -> m Buffer
toBuffer requestBody = do
  maybeBuffer <-
    liftEffect
      $ Ref.read requestBody.buffer
  case maybeBuffer of
    Nothing -> do
      buffers <- liftAff $ readableToBuffers requestBody.stream
      liftEffect do
        buffer <- Buffer.concat buffers
        Ref.write (Just buffer) requestBody.buffer
        pure buffer
    Just buffer -> pure buffer

-- | Return the `Readable` stream directly from `RequestBody`
toStream :: RequestBody -> Readable ()
toStream = _.stream

-- | Types that implement the `Body` class can be used as a body to an HTTPurple
-- | response, and can be used with all the response helpers.
class Body b where
  -- | Return any default headers that need to be sent with this body type,
  -- | things like `Content-Type`, `Content-Length`, and `Transfer-Encoding`.
  -- | Note that any headers passed in a response helper such as `ok'` will take
  -- | precedence over these.
  defaultHeaders :: b -> Effect RequestHeaders
  -- | Given a body value and a Node HTTP `Response` value, write the body value
  -- | to the Node response.
  write :: b -> ServerResponse -> Aff Unit

-- | The instance for `String` will convert the string to a buffer first in
-- | order to determine it's additional headers.  This is to ensure that the
-- | `Content-Length` header properly accounts for UTF-8 characters in the
-- | string.  Writing is simply implemented by writing the string to the
-- | response stream and closing the response stream.
instance Body String where
  defaultHeaders body = do
    buf :: Buffer <- fromString body UTF8
    defaultHeaders buf
  write body response = makeAff \done -> do
    let stream = OM.toWriteable $ SR.toOutgoingMessage response
    void $ writeString' stream UTF8 body $ const $ end' stream $ const $ done $ Right unit
    pure nonCanceler

-- | The instance for `Buffer` is trivial--we add a `Content-Length` header
-- | using `Buffer.size`, and to send the response, we just write the buffer to
-- | the stream and end the stream.
instance Body Buffer where
  defaultHeaders buf = mkRequestHeader "Content-Length" <$> show <$> size buf
  write body response = makeAff \done -> do
    let stream = OM.toWriteable $ SR.toOutgoingMessage response
    void $ Stream.write' stream body $ const $ end' stream $ const $ done $ Right unit
    pure nonCanceler

-- | This instance can be used to send chunked data.  Here, we add a
-- | `Transfer-Encoding` header to indicate chunked data.  To write the data, we
-- | simply pipe the newtype-wrapped `Stream` to the response.
instance
  TypeEquals (Stream r) (Readable s) =>
  Body (Stream r) where
  defaultHeaders _ = pure $ mkRequestHeader "Transfer-Encoding" "chunked"
  write body response = makeAff \done -> do
    let stream = to body
    void $ pipe stream $ OM.toWriteable $ SR.toOutgoingMessage response
    stream # once_ Stream.endH (done $ Right unit)
    pure nonCanceler
