module HTTPurple.Validation
  ( fromValidated
  , fromValidatedE
  ) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either, either)
import Effect.Aff.Class (class MonadAff)
import HTTPurple.Response (Response, badRequest)

fromValidatedContinuation ::
  forall input err validated m.
  MonadAff m =>
  (input -> Either err validated) ->
  (err -> m Response) ->
  input ->
  (validated -> m Response) ->
  m Response
fromValidatedContinuation validate errorHandler input handler =
  either errorHandler handler $ validate $ input

defaultErrorHandler :: forall (err :: Type) (m :: Type -> Type). MonadAff m => err -> m Response
defaultErrorHandler = const $ badRequest ""

-- | Validate an input using a validate function.
-- | If validation fails, the error handler is called.
-- | Returns a continuation
fromValidatedE ::
  forall input err validated m.
  MonadAff m =>
  (input -> Either err validated) ->
  (err -> m Response) ->
  input ->
  ContT Response m validated
fromValidatedE validate errorHandler input = ContT $ fromValidatedContinuation validate errorHandler input

-- | Validate an input using a validate function.
-- | If validation fails, a bad request is resturned.
-- | Returns a continuation
fromValidated ::
  forall input err validated m.
  MonadAff m =>
  (input -> Either err validated) ->
  input ->
  ContT Response m validated
fromValidated validator = fromValidatedE validator defaultErrorHandler
