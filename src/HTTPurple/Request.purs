module HTTPurple.Request
  ( ExtRequestNT(..)
  , ExtRequest
  , Request
  , RequestR
  , fromHTTPRequest
  , fromHTTPRequestExt
  , fromHTTPRequestUnit
  , fullPath
  ) where

import Prelude

import Data.Bifunctor (rmap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (isEmpty, toArrayWithKey)
import HTTPurple.Body (RequestBody)
import HTTPurple.Body (read) as Body
import HTTPurple.Headers (RequestHeaders)
import HTTPurple.Headers (read) as Headers
import HTTPurple.Method (Method)
import HTTPurple.Method (read) as Method
import HTTPurple.Path (Path)
import HTTPurple.Path (read) as Path
import HTTPurple.Query (Query)
import HTTPurple.Query (read) as Query
import HTTPurple.Utils (encodeURIComponent)
import HTTPurple.Version (Version)
import HTTPurple.Version (read) as Version
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.Types (IMServer, IncomingMessage)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Record.Studio (shrink)
import Record.Studio.Keys (class Keys)
import Routing.Duplex as RD
import Type.Prelude (Proxy)
import Unsafe.Coerce (unsafeCoerce)

type RequestR route r =
  ( method :: Method
  , path :: Path
  , query :: Query
  , route :: route
  , headers :: RequestHeaders
  , body :: RequestBody
  , httpVersion :: Version
  , url :: String
  | r
  )

-- | The `Request` type is a `Record` type that includes fields for accessing
-- | the different parts of the HTTP request.
type Request route = { | RequestR route () }

-- | Like `Request`, but can contain additional fields
type ExtRequest route ext = { | RequestR route ext }

-- | Newtype wrapping an extended request
-- | For internal use only. Use `ExtRequest` instead.
newtype ExtRequestNT :: Type -> Row Type -> Type
newtype ExtRequestNT route ext = ExtRequestNT { | RequestR route ext }

derive instance Newtype (ExtRequestNT route ext) _

-- | Return the full resolved path, including query parameters. This may not
-- | match the requested path--for instance, if there are empty path segments in
-- | the request--but it is equivalent.
fullPath :: forall r. { path :: Path, query :: Query | r } -> String
fullPath { path: p, query } = "/" <> path <> questionMark <> queryParams
  where
  path = joinWith "/" p
  questionMark = if isEmpty query then "" else "?"
  queryParams = joinWith "&" queryParamsArr
  queryParamsArr = toArrayWithKey stringifyQueryParam query
  stringifyQueryParam key value = encodeURIComponent key <> "=" <> encodeURIComponent value

mkRequest :: forall route m. MonadEffect m => IncomingMessage IMServer -> route -> m (Request route)
mkRequest request route = do
  body <- liftEffect $ Body.read request
  pure
    { method: Method.read request
    , path: Path.read request
    , query: Query.read request
    , route: route
    , headers: Headers.read request
    , body
    , httpVersion: Version.read request
    , url: IM.url request
    }

-- | Given an HTTP `Request` object, this method will convert it to an HTTPurple
-- | `Request` object.
fromHTTPRequest :: forall route. RD.RouteDuplex' route -> IncomingMessage IMServer -> Aff (Either (Request Unit) (Request route))
fromHTTPRequest route request = do
  RD.parse route (IM.url request) #
    bitraverse (const $ mkRequest request unit) (mkRequest request)

fromHTTPRequestUnit :: IncomingMessage IMServer -> Aff (Request Unit)
fromHTTPRequestUnit = flip mkRequest unit

fromHTTPRequestExt ::
  forall ctx ctxRL thru route.
  Union ctx thru ctx =>
  Nub (RequestR route ctx) (RequestR route ctx) =>
  RowToList ctx ctxRL =>
  Keys ctx =>
  RD.RouteDuplex' route ->
  Proxy ctx ->
  IncomingMessage IMServer ->
  Aff (Either (Request Unit) (ExtRequestNT route ctx))
fromHTTPRequestExt route _ nodeRequest = do
  let
    extension :: Record ctx
    extension = shrink (unsafeCoerce nodeRequest :: Record ctx)

    addExtension :: Request route -> ExtRequestNT route ctx
    addExtension = flip merge extension >>> ExtRequestNT

  request <- fromHTTPRequest route nodeRequest
  pure $ rmap addExtension request
