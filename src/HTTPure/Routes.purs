module HTTPure.Routes
  ( (<+>)
  , combineRoutes
  , orElse
  , type (<+>)
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Profunctor.Choice ((|||))
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Record as Record
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))

infixr 0 type Either as <+>

combineRoutes ::
  forall left right.
  RD.RouteDuplex' left ->
  RD.RouteDuplex' right ->
  RD.RouteDuplex' (Either left right)
combineRoutes (RD.RouteDuplex lEnc lDec) (RD.RouteDuplex rEnc rDec) = (RD.RouteDuplex enc dec)
  where
  enc = lEnc ||| rEnc
  dec = (lDec <#> Left) <|> (rDec <#> Right)

infixr 3 combineRoutes as <+>

orElse ::
  forall left right.
  (Request left -> ResponseM) ->
  (Request right -> ResponseM) ->
  Request (left <+> right) ->
  ResponseM
orElse leftRouter _ request@{ route: Left l } = leftRouter $ Record.set (Proxy :: _ "route") l request
orElse _ rightRouter request@{ route: Right r } = rightRouter $ Record.set (Proxy :: _ "route") r request

