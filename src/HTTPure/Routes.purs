module HTTPure.Routes
  ( (<+>)
  , combineRoutes
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Profunctor.Choice ((|||))
import Routing.Duplex as RD

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
