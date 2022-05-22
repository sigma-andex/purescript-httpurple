module Test.HTTPurple.ServerSpec where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Options ((:=))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty)
import HTTPurple.Request (Request)
import HTTPurple.Response (ResponseM, notFound, ok)
import HTTPurple.Server (serve, serve', serveSecure, serveSecure')
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.HTTP.Secure (key, keyString, cert, certString)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic as RG
import Test.HTTPurple.TestHelpers (Test, (?=), get, get', getStatus)
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError)

data Route = Test

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Test": RD.path "test" RG.noArgs
  }

mockRouter :: Request Route -> ResponseM
mockRouter { route: Test } = ok $ RD.print route Test

serveSpec :: Test
serveSpec =
  describe "serve" do
    it "boots a server on the given port" do
      close <- liftEffect $ serve 8080 { route, router: mockRouter, notFoundHandler: Nothing } $ pure unit
      out <- get 8080 empty "/test"
      liftEffect $ close $ pure unit
      out ?= "/test"
    it "responds with a 500 upon unhandled exceptions" do
      let router _ = throwError $ error "fail!"
      close <- liftEffect $ serve 8080 { route, router, notFoundHandler: Nothing } $ pure unit
      status <- getStatus 8080 empty "/test"
      liftEffect $ close $ pure unit
      status ?= 500

serve'Spec :: Test
serve'Spec =
  describe "serve'" do
    it "boots a server with the given options" do
      let options = { hostname: "localhost", port: 8080, backlog: Nothing }
      close <-
        liftEffect
          $ serve' options { route, router: mockRouter, notFoundHandler: Nothing }
          $ pure unit
      out <- get 8080 empty "/test"
      liftEffect $ close $ pure unit
      out ?= "/test"

serveSecureSpec :: Test
serveSecureSpec =
  describe "serveSecure" do
    describe "with valid key and cert files" do
      it "boots a server on the given port" do
        close <-
          liftEffect
            $ serveSecure 8080 "./test/Mocks/Certificate.cer" "./test/Mocks/Key.key" { route, router: mockRouter, notFoundHandler: Nothing }
            $ pure unit
        out <- get' 8080 empty "/test"
        liftEffect $ close $ pure unit
        out ?= "/test"
    describe "with invalid key and cert files" do
      it "throws" do
        expectError $ liftEffect
          $ serveSecure 8080 "" "" { route, router: mockRouter, notFoundHandler: Nothing }
          $ pure unit

serveSecure'Spec :: Test
serveSecure'Spec =
  describe "serveSecure'" do
    describe "with valid key and cert files" do
      it "boots a server on the given port" do
        let
          options = { hostname: "localhost", port: 8080, backlog: Nothing }
          sslOptions = do
            cert' <- readTextFile UTF8 "./test/Mocks/Certificate.cer"
            key' <- readTextFile UTF8 "./test/Mocks/Key.key"
            pure $ key := keyString key' <> cert := certString cert'
        sslOpts <- liftEffect $ sslOptions
        close <-
          liftEffect
            $ serveSecure' sslOpts options { route, router: mockRouter, notFoundHandler: Nothing }
            $ pure unit
        out <- get' 8080 empty "/test"
        liftEffect $ close $ pure unit
        out ?= "/test"

serverSpec :: Test
serverSpec =
  describe "Server" do
    serveSpec
    serve'Spec
    serveSecureSpec
    serveSecure'Spec
