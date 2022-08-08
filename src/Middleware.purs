module Middleware
  ( MiddlewareProcessing(..)
  , NodeMiddleware
  , using
  , usingContinuation
  ) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import HTTPurple.Request (Request, fromHTTPRequest)
import HTTPurple.Response (Response, send)
import Node.HTTP as HTTP
import Routing.Duplex (RouteDuplex')

type NodeMiddleware = HTTP.Request -> HTTP.Response -> Effect Unit -> Effect Unit

data MiddlewareProcessing = NotStarted | ProcessingFailed | ProcessingSucceeded (HTTP.Request /\ HTTP.Response)

usingContinuation ::
  NodeMiddleware ->
  (HTTP.Request /\ HTTP.Response) ->
  ((HTTP.Request /\ HTTP.Response) -> Effect MiddlewareProcessing) ->
  Effect MiddlewareProcessing
usingContinuation middleware reqResp nextHandler = do
  middlewareResult <- callMiddleware reqResp
  case middlewareResult of
    res@(ProcessingSucceeded newReqResp) -> let _ = spy "Next called" res in nextHandler newReqResp
    res -> let _ = spy "Next not called" res in pure res
  where
  callMiddleware :: HTTP.Request /\ HTTP.Response -> Effect (MiddlewareProcessing)
  callMiddleware (req /\ resp) = do
    ref <- Ref.new NotStarted
    let
      next :: Effect Unit
      next = Ref.write (ProcessingSucceeded reqResp) ref
    middleware req resp next
    value <- Ref.read ref
    pure value

using ::
  (HTTP.Request -> HTTP.Response -> Effect Unit -> Effect Unit) ->
  HTTP.Request /\ HTTP.Response ->
  ContT MiddlewareProcessing Effect (HTTP.Request /\ HTTP.Response)
using nodeMiddleware reqResp = ContT $ usingContinuation nodeMiddleware reqResp

type Handler route m = Request route -> m Response

-- runHandler ::
--   forall route m.
--   MonadAff m =>
--   RouteDuplex' route ->
--   Handler route m ->
--   HTTP.Request ->
--   HTTP.Response ->
--   Effect Unit
-- runHandler rd handler req resp = launchAff_ do
--   httpurpleReq <- fromHTTPRequest rd req
--   httpurpleResp <- handler httpurpleReq
--   send resp httpurpleResp
