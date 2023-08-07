module HTTPurple.Headers
  ( RequestHeaders(..)
  , ResponseHeaders(..)
  , class ToHeaders
  , class ToHeadersHelper
  , empty
  , header
  , headers
  , headersImpl
  , mkRequestHeader
  , mkRequestHeaders
  , read
  , toResponseHeaders
  , write
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map, insert, singleton, union)
import Data.Map (empty) as Map
import Data.Newtype (class Newtype, un, unwrap)
import Data.String.CaseInsensitive (CaseInsensitiveString(CaseInsensitiveString))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Foreign.Object (fold)
import HTTPurple.Lookup (class Lookup, (!!))
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.OutgoingMessage (setHeader')
import Node.HTTP.ServerResponse (toOutgoingMessage)
import Node.HTTP.Types (IMServer, IncomingMessage, ServerResponse)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil)
import Record as Record
import Type.Proxy (Proxy(..))

-- | The `RequestHeaders` type is a wrapper for a map
-- | that represents the set of headers in an HTTP request.
-- | A request header contains maximum one value per key.
newtype RequestHeaders = RequestHeaders (Map CaseInsensitiveString String)

derive instance Newtype RequestHeaders _

-- | Given a string, return a `Maybe` containing the value of the matching
-- | header, if there is any.
instance Lookup RequestHeaders String String where
  lookup (RequestHeaders headers') key = headers' !! key

-- | Allow a `RequestHeaders` to be represented as a string. This string is formatted
-- | in HTTP headers format.
instance Show RequestHeaders where
  show (RequestHeaders headers') = foldMapWithIndex showField headers' <> "\n"
    where
    showField key value = unwrap key <> ": " <> value <> "\n"

-- | Compare two `RequestHeaders` objects by comparing the underlying `Objects`.
instance Eq RequestHeaders where
  eq (RequestHeaders a) (RequestHeaders b) = eq a b

-- | Allow one `RequestHeaders` objects to be appended to another.
instance Semigroup RequestHeaders where
  append (RequestHeaders a) (RequestHeaders b) = RequestHeaders $ union b a

-- | The `ResponseHeaders` type is a wrapper for a map
-- | that represents the set of headers in an HTTP response.
-- | A response header can contain multiple values per key, 
-- | e.g. in the case of multiple Set-Cookie directives.
newtype ResponseHeaders = ResponseHeaders (Map CaseInsensitiveString (Array String))

-- | Allow one `ResponseHeaders` objects to be appended to another.
instance Semigroup ResponseHeaders where
  append (ResponseHeaders a) (ResponseHeaders b) = ResponseHeaders $ union b a

-- | Allow a `ResponseHeaders` to be represented as a string. This string is formatted
-- | in HTTP headers format.
instance Show ResponseHeaders where
  show (ResponseHeaders headers') = foldMapWithIndex showField headers' <> "\n"
    where
    showField key value = Array.foldMap (\v -> unwrap key <> ": " <> v <> "\n") value

-- | Compare two `ResponseHeaders` objects by comparing the underlying `Objects`.
instance Eq ResponseHeaders where
  eq (ResponseHeaders a) (ResponseHeaders b) = eq a b

-- | Get the headers out of a HTTP `RequestHeaders` object.
read :: IncomingMessage IMServer -> RequestHeaders
read = IM.headers >>> fold insertField Map.empty >>> RequestHeaders
  where
  insertField x key value = insert (CaseInsensitiveString key) value x

-- | Given an HTTP `Response` and a `ResponseHeaders` object, return an effect that will
-- | write the `ResponseHeaders` to the `Response`.
write :: ServerResponse -> ResponseHeaders -> Effect Unit
write response (ResponseHeaders headers') = void $ traverseWithIndex writeField headers'
  where
  om = toOutgoingMessage response
  writeField key values = om # setHeader' (unwrap key) values

-- | Return a `ResponseHeaders` containing no headers.
empty :: ResponseHeaders
empty = ResponseHeaders Map.empty

-- | Convert an `Array` of `Tuples` of 2 `Strings` to a `RequestHeaders` object.
-- | This is intended mainly for internal use. 
mkRequestHeaders :: Array (Tuple String String) -> RequestHeaders
mkRequestHeaders = foldl insertField Map.empty >>> RequestHeaders
  where
  insertField x (Tuple key value) = insert (CaseInsensitiveString key) value x

-- | Create a singleton header from a key-value pair.
-- | This is intended mainly for internal use. 
mkRequestHeader :: String -> String -> RequestHeaders
mkRequestHeader key = singleton (CaseInsensitiveString key) >>> RequestHeaders

-- | Create a singleton header from a key-value pair.
header :: String -> String -> ResponseHeaders
header key = Array.singleton >>> singleton (CaseInsensitiveString key) >>> ResponseHeaders

-- | Copy the request headers to the response headers
-- | This is intended mainly for internal use. 
toResponseHeaders :: RequestHeaders -> ResponseHeaders
toResponseHeaders = un RequestHeaders >>> map (Array.singleton) >>> ResponseHeaders

class ToHeadersHelper :: forall k. Row Type -> k -> Constraint
class ToHeadersHelper r rl where
  headersImpl :: Proxy rl -> Record r -> ResponseHeaders

instance ToHeadersHelper r (Nil) where
  headersImpl _ _ = empty
else instance
  ( IsSymbol sym
  , RowToList r rl
  , RowToList tail tailRL
  , Row.Cons sym String tail r
  , Row.Lacks sym tail
  , ToHeadersHelper tail tailRL
  ) =>
  ToHeadersHelper r (Cons sym String tailRL) where
  headersImpl _ rec = header key value <> headersImpl (Proxy :: Proxy tailRL) tail
    where
    key = reflectSymbol (Proxy :: Proxy sym)
    value = Record.get (Proxy :: Proxy sym) rec

    tail = Record.delete (Proxy :: Proxy sym) rec
else instance
  ( IsSymbol sym
  , RowToList r rl
  , RowToList tail tailRL
  , Row.Cons sym (Array String) tail r
  , Row.Lacks sym tail
  , ToHeadersHelper tail tailRL
  ) =>
  ToHeadersHelper r (Cons sym (Array String) tailRL) where
  headersImpl _ rec = constructHeaders key value <> headersImpl (Proxy :: Proxy tailRL) tail
    where
    constructHeaders k = singleton (CaseInsensitiveString k) >>> ResponseHeaders
    key = reflectSymbol (Proxy :: Proxy sym)
    value = Record.get (Proxy :: Proxy sym) rec

    tail = Record.delete (Proxy :: Proxy sym) rec

class ToHeaders r where
  -- | Create `ResponseHeaders` from a record, an `Array (Tuple String String)` or an `Array (Tuple String (Array String))`
  headers :: r -> ResponseHeaders

instance ToHeaders (Array (Tuple String String)) where
  headers = foldl insertField Map.empty >>> ResponseHeaders
    where
    insertField x (Tuple key value) = insert (CaseInsensitiveString key) (Array.singleton value) x
else instance ToHeaders (Array (Tuple String (Array String))) where
  headers = foldl insertField Map.empty >>> ResponseHeaders
    where
    insertField x (Tuple key value) = insert (CaseInsensitiveString key) value x
else instance (RowToList r rl, ToHeadersHelper r rl) => ToHeaders (Record r) where
  headers = headersImpl (Proxy :: Proxy rl)
