module HTTPurple.Cont
  ( usingCont
  ) where

import Prelude

import Control.Monad.Cont (ContT, runContT)

usingCont :: forall output m. Applicative m => ContT output m output -> m output
usingCont = flip runContT pure
