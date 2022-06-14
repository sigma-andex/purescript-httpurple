module Test.HTTPurple.RequestSpec where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..), either, fromRight)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign.Object (singleton)
import HTTPurple.Body (toString)
import HTTPurple.Headers (headers, mkRequestHeaders)
import HTTPurple.Method (Method(Post))
import HTTPurple.Request (fromHTTPRequest, fullPath)
import HTTPurple.Version (Version(HTTP1_1))
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((?))
import Test.HTTPurple.TestHelpers (Test, mockRequest, (?=))
import Test.Spec (describe, it)

data Route = Test { a :: Maybe String }

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ G.sum
  { "Test": "test" ? { a: RD.optional <<< RD.string }
  }

getRight :: forall a b. Aff (Either a b) -> Aff b
getRight input = input >>= either (const throwLeft) pure
  where
  throwLeft = throwError (error "Invalid route")

fromHTTPRequestSpec :: Test
fromHTTPRequestSpec =
  describe "fromHTTPRequest" do
    it "contains the correct method" do
      mock <- mockRequest' # getRight
      mock.method ?= Post
    it "contains the correct path" do
      mock <- mockRequest' # getRight
      mock.path ?= [ "test" ]
    it "contains the correct query" do
      mock <- mockRequest' # getRight
      mock.query ?= singleton "a" "b"
    it "contains the correct headers" do
      mock <- mockRequest' # getRight
      mock.headers ?= mkRequestHeaders mockHeaders
    it "contains the correct body" do
      mockBody <- mockRequest' # getRight >>= (_.body >>> toString)
      mockBody ?= "body"
    it "contains the correct httpVersion" do
      mock <- mockRequest' # getRight
      mock.httpVersion ?= HTTP1_1
  where
  mockHeaders = [ Tuple "Test" "test" ]

  mockHTTPRequest = mockRequest "1.1" "POST" "/test?a=b" "body" mockHeaders

  mockRequest' = mockHTTPRequest >>= fromHTTPRequest route

-- [TODO] Fix this tests or remove them because we can get it from RoutingDuplex
-- fullPathSpec :: Test
-- fullPathSpec =
--   describe "fullPath" do
--     describe "without query parameters" do
--       it "is correct" do
--         mock <- mockRequest' "/foo/bar" # getRight
--         fullPath mock ?= "/foo/bar"
-- describe "with empty path segments" do
--   it "strips the empty segments" do
--     mock <- mockRequest' "//foo////bar/"
--     fullPath mock ?= "/foo/bar"
-- describe "with only query parameters" do
--   it "is correct" do
--     mock <- mockRequest' "?a=b&c=d"
--     fullPath mock ?= "/?a=b&c=d"
-- describe "with only empty query parameters" do
--   it "is has the default value of '' for the empty parameters" do
--     mock <- mockRequest' "?a"
--     fullPath mock ?= "/?a="
-- describe "with query parameters that have special characters" do
--   it "percent encodes query params" do
--     mock <- mockRequest' "?a=%3Fx%3Dtest"
--     fullPath mock ?= "/?a=%3Fx%3Dtest"
-- describe "with empty query parameters" do
--   it "strips out the empty arameters" do
--     mock <- mockRequest' "?a=b&&&"
--     fullPath mock ?= "/?a=b"
-- describe "with a mix of segments and query parameters" do
--   it "is correct" do
--     mock <- mockRequest' "/foo///bar/?&a=b&&c"
--     fullPath mock ?= "/foo/bar?a=b&c="
-- where
-- mockHTTPRequest path = mockRequest "" "POST" path "body" []

-- mockRequest' path = mockHTTPRequest path >>= fromHTTPRequest route

requestSpec :: Test
requestSpec =
  describe "Request" do
    fromHTTPRequestSpec
--fullPathSpec
