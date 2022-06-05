module HTTPurple.Cont
  ( usingCont
  ) where

import Prelude

import Control.Monad.Cont (ContT, runContT)

-- | Run the continuation
usingCont :: forall output m. Applicative m => ContT output m output -> m output
usingCont = flip runContT pure
