module HTTPurple.Routes
  ( (<+>)
  , catchAll
  , combineRoutes
  , mkRoute
  , orElse
  , type (<+>)
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Profunctor.Choice ((|||))
import HTTPurple.Request (Request)
import HTTPurple.Response (ResponseM)
import Record as Record
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG
import Type.Proxy (Proxy(..))

-- | Type-level operator two combine two route definitions.
infixr 0 type Either as <+>

-- | Combine two routes
combineRoutes ::
  forall left right.
  RD.RouteDuplex' left ->
  RD.RouteDuplex' right ->
  RD.RouteDuplex' (Either left right)
combineRoutes (RD.RouteDuplex lEnc lDec) (RD.RouteDuplex rEnc rDec) = (RD.RouteDuplex enc dec)
  where
  enc = lEnc ||| rEnc
  dec = (lDec <#> Left) <|> (rDec <#> Right)

-- | Infix operator for `orElse`
infixr 3 combineRoutes as <+>

-- | Combine two request handlers.
orElse ::
  forall left right.
  (Request left -> ResponseM) ->
  (Request right -> ResponseM) ->
  Request (left <+> right) ->
  ResponseM
orElse leftRouter _ request@{ route: Left l } = leftRouter $ Record.set (Proxy :: _ "route") l request
orElse _ rightRouter request@{ route: Right r } = rightRouter $ Record.set (Proxy :: _ "route") r request

-- | Make a route from a `RoudeDuplex` definition.
mkRoute ::
  forall i iGen r.
  Generic i iGen =>
  RG.GRouteDuplex iGen r =>
  Record r ->
  RD.RouteDuplex i i
mkRoute = RD.root <<< RG.sum

-- | A catch-all route that matches any request.
catchAll :: RD.RouteDuplex' (Array String)
catchAll = RD.many RD.segment
