module HTTPure.Request
  ( Request
  , fromHTTPRequest
  , fullPath
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (isEmpty, toArrayWithKey)
import HTTPure.Body (RequestBody)
import HTTPure.Body (read) as Body
import HTTPure.Headers (Headers)
import HTTPure.Headers (read) as Headers
import HTTPure.Method (Method)
import HTTPure.Method (read) as Method
import HTTPure.Path (Path)
import HTTPure.Path (read) as Path
import HTTPure.Query (Query)
import HTTPure.Query (read) as Query
import HTTPure.Utils (encodeURIComponent)
import HTTPure.Version (Version)
import HTTPure.Version (read) as Version
import Node.HTTP (Request) as HTTP
import Node.HTTP (requestURL)
import Routing.Duplex as RD

-- | The `Request` type is a `Record` type that includes fields for accessing
-- | the different parts of the HTTP request.
type Request route =
  { method :: Method
  , path :: Path
  , query :: Query
  , route :: route
  , headers :: Headers
  , body :: RequestBody
  , httpVersion :: Version
  , url :: String
  }

-- | Return the full resolved path, including query parameters. This may not
-- | match the requested path--for instance, if there are empty path segments in
-- | the request--but it is equivalent.
fullPath :: forall route. Request route -> String
fullPath request = "/" <> path <> questionMark <> queryParams
  where
  path = joinWith "/" request.path
  questionMark = if isEmpty request.query then "" else "?"
  queryParams = joinWith "&" queryParamsArr
  queryParamsArr = toArrayWithKey stringifyQueryParam request.query
  stringifyQueryParam key value = encodeURIComponent key <> "=" <> encodeURIComponent value

-- | Given an HTTP `Request` object, this method will convert it to an HTTPure
-- | `Request` object.
fromHTTPRequest :: forall route. RD.RouteDuplex' route -> HTTP.Request -> Aff (Either (Request Unit) (Request route))
fromHTTPRequest route request = do
  body <- liftEffect $ Body.read request
  let
    mkRequest :: forall r. r -> Request r
    mkRequest r =
      { method: Method.read request
      , path: Path.read request
      , query: Query.read request
      , route: r
      , headers: Headers.read request
      , body
      , httpVersion: Version.read request
      , url: requestURL request
      }
  pure $ bimap (const $ mkRequest unit) mkRequest $ RD.parse route (requestURL request)

