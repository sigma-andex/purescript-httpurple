module HTTPurple.NodeMiddleware
  ( MiddlewareResult(..)
  , MiddlewareResultR
  , NextHandlerArg
  , NextInvocation(..)
  , NodeMiddleware(..)
  , NodeMiddlewareStack(..)
  , usingMiddleware
  , callNext
  , callNextWithError
  , class UsingMiddleware
  , dontCallNext
  ) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn3)
import Literals.Undefined (Undefined, undefined)
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Prim.Row (class Union)
import Untagged.Union (type (|+|), UndefinedOr, asOneOf, uorToMaybe)

newtype NodeMiddleware :: forall k. k -> Type
newtype NodeMiddleware extended =
  NodeMiddleware (EffectFn3 (IncomingMessage IMServer) ServerResponse (EffectFn1 (UndefinedOr Error) Unit) (Effect Unit))

derive instance Newtype (NodeMiddleware extended) _

data NextInvocation = NotCalled | ProcessingFailed Error | ProcessingSucceeded

type MiddlewareResultR =
  (request :: IncomingMessage IMServer, response :: ServerResponse, middlewareResult :: NextInvocation)

newtype MiddlewareResult :: forall k. k -> Type
newtype MiddlewareResult input = MiddlewareResult { | MiddlewareResultR }

derive instance Generic NextInvocation _
instance Show NextInvocation where
  show = genericShow

usingContinuation ::
  forall input thru tmp tmp2 output.
  Union input thru tmp =>
  Union tmp tmp2 output =>
  NodeMiddleware thru ->
  MiddlewareResult input ->
  (MiddlewareResult input -> Effect (MiddlewareResult output)) ->
  Effect (MiddlewareResult output)
usingContinuation (NodeMiddleware middleware) reqResp@(MiddlewareResult { request, response }) nextHandler = do
  middlewareResult <- callMiddleware reqResp
  case middlewareResult of
    res@(ProcessingSucceeded) ->
      nextHandler (MiddlewareResult { request, response, middlewareResult: res })
    res@(ProcessingFailed _) -> pure (MiddlewareResult { request, response, middlewareResult: res })
    res@(NotCalled) -> pure (MiddlewareResult { request, response, middlewareResult: res })
  where
  callMiddleware :: MiddlewareResult input -> Effect (NextInvocation)
  callMiddleware (MiddlewareResult { request: rIn, response: rOut }) = do
    ref <- Ref.new NotCalled
    let
      next :: EffectFn1 (UndefinedOr Error) Unit
      next = mkEffectFn1 \error -> case uorToMaybe error of
        Just err -> Ref.write (ProcessingFailed err) ref
        Nothing -> Ref.write (ProcessingSucceeded) ref
    _ <- runEffectFn3 middleware rIn rOut next
    Ref.read ref

class UsingMiddleware :: Row Type -> Row Type -> Row Type -> Constraint
class UsingMiddleware input thru output | input thru -> output where
  usingMiddleware ::
    NodeMiddleware thru ->
    MiddlewareResult input ->
    ContT (MiddlewareResult output) Effect (MiddlewareResult input)

instance (Union input thru tmp, Union tmp tmp2 output) => UsingMiddleware input thru output where
  usingMiddleware nodeMiddleware reqResp = ContT $ usingContinuation nodeMiddleware reqResp

type NextHandlerArg = Undefined |+| Error

dontCallNext :: forall (m :: Type -> Type). Applicative m => m Unit
dontCallNext = pure unit

callNext :: EffectFn1 NextHandlerArg Unit -> Effect Unit
callNext next = runEffectFn1 next (asOneOf $ undefined)

callNextWithError :: EffectFn1 NextHandlerArg Unit -> Error -> Effect Unit
callNextWithError next err = runEffectFn1 next (asOneOf err)

newtype NodeMiddlewareStack :: Row Type -> Row Type -> Type
newtype NodeMiddlewareStack input output =
  NodeMiddlewareStack (MiddlewareResult input -> ContT (MiddlewareResult output) Effect (MiddlewareResult input))

instance Newtype (NodeMiddlewareStack input output) (MiddlewareResult input -> ContT (MiddlewareResult output) Effect (MiddlewareResult input))
