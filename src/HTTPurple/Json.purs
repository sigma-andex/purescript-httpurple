module HTTPurple.Json
  ( JsonDecoder(..)
  , fromJson
  , fromJsonE
  , jsonHeader
  , jsonHeaders
  ) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either, either)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import HTTPurple.Body (RequestBody, toString)
import HTTPurple.Headers (Headers, headers)
import HTTPurple.Response (Response, ResponseM, badRequest')

newtype JsonDecoder err json = JsonDecoder (String -> Either err json)

instance Newtype (JsonDecoder err json) (String -> Either err json)

jsonHeader :: Tuple String String
jsonHeader = Tuple "Content-Type" "application/json"

jsonHeaders :: Headers
jsonHeaders = headers [ jsonHeader ]

fromJsonContinuation ::
  forall err json.
  JsonDecoder err json ->
  (err -> ResponseM) ->
  RequestBody ->
  (json -> ResponseM) ->
  ResponseM
fromJsonContinuation (JsonDecoder decode) errorHandler body handler = do
  bodyStr <- toString body
  let
    parseJson :: Either err json
    parseJson = decode $ bodyStr

    toBadRequest err = errorHandler err
  either toBadRequest handler parseJson

defaultErrorHandler :: forall (t47 :: Type) (m :: Type -> Type). MonadAff m => t47 -> m Response
defaultErrorHandler = const $ badRequest' jsonHeaders ""

fromJsonE :: forall (err :: Type) (json :: Type). JsonDecoder err json -> (err -> ResponseM) -> RequestBody -> ContT Response Aff json
fromJsonE driver errorHandler body = ContT $ (fromJsonContinuation driver errorHandler body)

fromJson :: forall (err :: Type) (json :: Type). JsonDecoder err json -> RequestBody -> ContT Response Aff json
fromJson driver = fromJsonE driver defaultErrorHandler

