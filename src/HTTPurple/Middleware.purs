module HTTPurple.Middleware
  ( Middleware
  , MiddlewareM
  ) where

import Effect.Aff (Aff)
import HTTPurple.Request (ExtRequest)
import HTTPurple.Response (Response)

type MiddlewareM m route extIn extOut = (ExtRequest route extOut -> m Response) -> ExtRequest route extIn -> m Response
type Middleware route extIn extOut = MiddlewareM Aff route extIn extOut
