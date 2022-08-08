module HTTPurple.Record.Extra where

import Prelude

import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List, (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class Keys (xs :: RL.RowList Type) where
  keysImpl :: Proxy xs -> List String

instance Keys RL.Nil where
  keysImpl _ = mempty

instance
  ( IsSymbol name
  , Keys tail
  ) =>
  Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
    first = reflectSymbol (Proxy :: _ name)
    rest = keysImpl (Proxy :: _ tail)

keys ::
  forall g row rl.
  RL.RowToList row rl =>
  Keys rl =>
  g row -- this will work for any type with the row as a param!
    ->
  List String
keys _ = keysImpl (Proxy :: _ rl)

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick ::
  forall a r b l.
  Row.Union b r a =>
  RL.RowToList b l =>
  Keys l =>
  Record a ->
  Record b
pick = runFn2 pickFn ks
  where
  ks = fromFoldable $ keys (Proxy :: _ b)
