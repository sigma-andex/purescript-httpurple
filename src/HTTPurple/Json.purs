module HTTPurple.Json
  ( JsonDecoder(..)
  , JsonEncoder(..)
  , fromJson
  , fromJsonE
  , jsonHeaders
  , toJson
  ) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either, either)
import Data.Function as Function
import Data.Newtype (class Newtype, un)
import Effect.Aff.Class (class MonadAff)
import HTTPurple.Body (RequestBody, toString)
import HTTPurple.Headers (ResponseHeaders, headers)
import HTTPurple.Response (Response, badRequest)

newtype JsonDecoder err json = JsonDecoder (String -> Either err json)

instance Newtype (JsonDecoder err json) (String -> Either err json)

newtype JsonEncoder json = JsonEncoder (json -> String)

instance Newtype (JsonEncoder json) (json -> String)

jsonHeaders :: ResponseHeaders
jsonHeaders = headers { "Content-Type": "application/json" }

fromJsonContinuation ::
  forall err json m.
  MonadAff m =>
  JsonDecoder err json ->
  (err -> m Response) ->
  RequestBody ->
  (json -> m Response) ->
  m Response
fromJsonContinuation (JsonDecoder decode) errorHandler body handler = do
  bodyStr <- toString body
  let
    parseJson :: Either err json
    parseJson = decode $ bodyStr

  either errorHandler handler parseJson

defaultErrorHandler :: forall (err :: Type) (m :: Type -> Type). MonadAff m => err -> m Response
defaultErrorHandler = const $ badRequest ""

-- | Parse the `RequestBody` as json using the provided `JsonDecoder`. 
-- | If it fails, the error handler is called.
-- | Returns a continuation
fromJsonE ::
  forall (err :: Type) (json :: Type) (m :: Type -> Type).
  MonadAff m =>
  JsonDecoder err json ->
  (err -> m Response) ->
  RequestBody ->
  ContT Response m json
fromJsonE driver errorHandler body = ContT $ (fromJsonContinuation driver errorHandler body)

-- | Parse the `RequestBody` as json using the provided `JsonDecoder`. 
-- | If it fails, an empty bad request is returned
-- | Returns a continuation
fromJson :: forall (err :: Type) (json :: Type) (m :: Type -> Type). MonadAff m => JsonDecoder err json -> RequestBody -> ContT Response m json
fromJson driver = fromJsonE driver defaultErrorHandler

-- | Serialise a type to json using the given driver.
toJson :: forall (json :: Type). JsonEncoder json -> json -> String
toJson = un JsonEncoder >>> Function.apply
