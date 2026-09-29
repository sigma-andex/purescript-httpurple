module Test.HTTPurple.IntegrationSpec where

import Prelude

import Effect.Class (liftEffect)
import Examples.AsyncResponse.Main as AsyncResponse
import Examples.BinaryRequest.Main as BinaryRequest
import Examples.BinaryResponse.Main as BinaryResponse
import Examples.Chunked.Main as Chunked
import Examples.CustomStack.Main as CustomStack
import Examples.ExtensibleMiddleware.Main as ExtensibleMiddleware
import Examples.Headers.Main as Headers
import Examples.HelloWorld.Main as HelloWorld
import Examples.JsonParsing.Main as JsonParsing
import Examples.Middleware.Main as Middleware
import Examples.MultiRoute.Main as MultiRoute
import Examples.NodeMiddleware.Main as NodeMiddleware
import Examples.PathSegments.Main as PathSegments
import Examples.Post.Main as Post
import Examples.Query.Main as Query
import Examples.QueryParameters.Main as QueryParameters
import Examples.SSL.Main as SSL
import Examples.Static.Main as Static
import Foreign.Object (empty, singleton)
import Foreign.Object as Object
import Node.Buffer (toArray)
import Node.FS.Aff (readFile)
import Test.HTTPurple.TestHelpers (Test, awaitStarted, awaitStartedSecure, get, get', getBinary, getHeader, getStatus, post, postBinary, query, query', (?=))
import Test.Spec (describe, it)
import Test.Spec.Assertions.String (shouldContain, shouldStartWith)

asyncResponseSpec :: Test
asyncResponseSpec =
  it "runs the async response example" do
    close <- liftEffect AsyncResponse.main
    awaitStarted 8080
    response <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    response ?= "hello world!"

queryWithRequestBodySpec :: Test
queryWithRequestBodySpec =
  it "runs a QUERY and accepts a request body" do
    close <- liftEffect Query.main
    awaitStarted 8080
    let body = "{ \"lastName\": { \"eq\": \"waltz\", \"caseSensitive\": false } } "
    response <- query 8080 empty "/" body
    liftEffect $ close $ pure unit
    response ?= body

queryWithEmptyBodySpec :: Test
queryWithEmptyBodySpec =
  it "runs a QUERY with an empty request body" do
    close <- liftEffect Query.main
    awaitStarted 8080
    response <- query 8080 empty "/" ""
    liftEffect $ close $ pure unit
    response ?= ""

queryVsGetSpec :: Test
queryVsGetSpec =
  it "distinguishes a QUERY from a GET on the same route" do
    close <- liftEffect Query.main
    awaitStarted 8080
    getResponse <- get 8080 empty "/"
    queryResponse <- query 8080 empty "/" "q=foo"
    liftEffect $ close $ pure unit
    getResponse ?= "send a QUERY request with a body to see it echoed back"
    queryResponse ?= "q=foo"

querySslSpec :: Test
querySslSpec =
  it "runs a QUERY over HTTPS" do
    close <- liftEffect SSL.main
    awaitStartedSecure 8080
    response <- query' 8080 empty "/" "q=foo"
    liftEffect $ close $ pure unit
    response ?= "hello world!"

binaryRequestSpec :: Test
binaryRequestSpec =
  it "runs the binary request example" do
    close <- liftEffect BinaryRequest.main
    awaitStarted 8080
    binaryBuf <- readFile BinaryResponse.filePath
    response <- postBinary 8080 empty "/" binaryBuf
    liftEffect $ close $ pure unit
    response ?= "d5e776724dd545d8b54123b46362a553d10257cee688ef1be62166c984b34405"

binaryResponseSpec :: Test
binaryResponseSpec =
  it "runs the binary response example" do
    close <- liftEffect BinaryResponse.main
    awaitStarted 8080
    responseBuf <- getBinary 8080 empty "/"
    liftEffect $ close $ pure unit
    binaryBuf <- readFile BinaryResponse.filePath
    expected <- liftEffect $ toArray binaryBuf
    response <- liftEffect $ toArray responseBuf
    response ?= expected

chunkedSpec :: Test
chunkedSpec =
  it "runs the chunked example" do
    close <- liftEffect Chunked.main
    awaitStarted 8080
    response <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    -- TODO this isn't a great way to validate this, we need a way of inspecting
    -- each individual chunk instead of just looking at the entire response
    response ?= "hello \nworld!\n"

customStackSpec :: Test
customStackSpec =
  it "runs the custom stack example" do
    close <- liftEffect CustomStack.main
    awaitStarted 8080
    response <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    response ?= "hello, joe"

headersSpec :: Test
headersSpec =
  it "runs the headers example" do
    close <- liftEffect Headers.main
    awaitStarted 8080
    header <- getHeader 8080 empty "/" "X-Example"
    response <- get 8080 (singleton "X-Input" "test") "/"
    liftEffect $ close $ pure unit
    header ?= "hello world!"
    response ?= "test"

helloWorldSpec :: Test
helloWorldSpec =
  it "runs the hello world example" do
    close <- liftEffect HelloWorld.main
    awaitStarted 8080
    response <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    response ?= "hello world!"

jsonParsingSpec :: Test
jsonParsingSpec =
  it "runs the hello world example" do
    close <- liftEffect JsonParsing.main
    awaitStarted 8080
    response <- post 8080 empty "/" "{\"name\":\"world\"}"
    liftEffect $ close $ pure unit
    response ?= "{\"hello\": \"world\" }"

middlewareSpec :: Test
middlewareSpec =
  it "runs the middleware example" do
    close <- liftEffect Middleware.main
    awaitStarted 8080
    header <- getHeader 8080 empty "/" "X-Middleware"
    body <- get 8080 empty "/"
    header' <- getHeader 8080 empty "/middleware" "X-Middleware"
    body' <- get 8080 empty "/middleware"
    liftEffect $ close $ pure unit
    header ?= "router"
    body ?= "hello"
    header' ?= "middleware"
    body' ?= "Middleware!"

multiRouteSpec :: Test
multiRouteSpec =
  it "runs the multi route example" do
    close <- liftEffect MultiRoute.main
    awaitStarted 8080
    hello <- get 8080 empty "/hello"
    goodbye <- get 8080 empty "/goodbye"
    liftEffect $ close $ pure unit
    hello ?= "hello"
    goodbye ?= "goodbye"

pathSegmentsSpec :: Test
pathSegmentsSpec =
  it "runs the path segments example" do
    close <- liftEffect PathSegments.main
    awaitStarted 8080
    foo <- get 8080 empty "/segment/foo"
    somebars <- get 8080 empty "/some/bars"
    liftEffect $ close $ pure unit
    foo ?= "foo"
    somebars ?= "[\"some\",\"bars\"]"

postSpec :: Test
postSpec =
  it "runs the post example" do
    close <- liftEffect Post.main
    awaitStarted 8080
    response <- post 8080 empty "/" "test"
    liftEffect $ close $ pure unit
    response ?= "test"

queryParametersSpec :: Test
queryParametersSpec =
  it "runs the query parameters example" do
    close <- liftEffect QueryParameters.main
    awaitStarted 8080
    foo <- get 8080 empty "/?foo"
    bar <- get 8080 empty "/?bar=test"
    notbar <- get 8080 empty "/?bar=nottest"
    baz <- get 8080 empty "/?baz=test"
    liftEffect $ close $ pure unit
    foo ?= "foo"
    bar ?= "bar"
    notbar ?= ""
    baz ?= "test"

sslSpec :: Test
sslSpec =
  it "runs the ssl example" do
    close <- liftEffect SSL.main
    awaitStartedSecure 8080
    response <- get' 8080 empty "/"
    liftEffect $ close $ pure unit
    response ?= "hello world!"

extensibleMiddlewareSpec :: Test
extensibleMiddlewareSpec =
  it "runs the extensible middleware example" do
    close <- liftEffect ExtensibleMiddleware.main
    awaitStarted 8080
    let headers = Object.singleton "X-Token" "123"
    body <- get 8080 headers "/"
    body' <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    body `shouldStartWith` "hello John Doe, it is"
    body' `shouldStartWith` "hello anonymous, it is"

nodeMiddlewareSpec :: Test
nodeMiddlewareSpec =
  it "runs the node middleware example" do
    close <- liftEffect NodeMiddleware.main
    awaitStarted 8080
    let headers = Object.singleton "X-Token" "123"
    body <- get 8080 headers "/"
    body' <- get 8080 empty "/"
    liftEffect $ close $ pure unit
    body `shouldStartWith` "hello John Doe"
    body' `shouldStartWith` "hello anonymous"

staticSpec :: Test
staticSpec =
  it "runs the static example" do
    close <- liftEffect Static.main
    awaitStarted 8080
    home <- get 8080 empty "/"
    stylesheetType <- getHeader 8080 empty "/static/style.css" "Content-Type"
    image <- getBinary 8080 empty "/static/circle.png"
    missing <- getStatus 8080 empty "/static/missing.css"
    secret <- getStatus 8080 empty "/static/.secret"
    liftEffect $ close $ pure unit
    home `shouldContain` "<title>HTTPurple static example</title>"
    stylesheetType ?= "text/css; charset=utf-8"
    imageBuf <- readFile (Static.root <> "/circle.png")
    expected <- liftEffect $ toArray imageBuf
    actual <- liftEffect $ toArray image
    actual ?= expected
    missing ?= 404
    secret ?= 404

integrationSpec :: Test
integrationSpec =
  describe "Integration" do
    asyncResponseSpec
    queryWithRequestBodySpec
    queryWithEmptyBodySpec
    queryVsGetSpec
    querySslSpec
    binaryRequestSpec
    binaryResponseSpec
    chunkedSpec
    customStackSpec
    headersSpec
    helloWorldSpec
    middlewareSpec
    multiRouteSpec
    pathSegmentsSpec
    postSpec
    queryParametersSpec
    sslSpec
    staticSpec
    jsonParsingSpec
    extensibleMiddlewareSpec
    nodeMiddlewareSpec
