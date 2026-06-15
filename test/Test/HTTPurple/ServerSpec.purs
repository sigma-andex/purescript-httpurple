module Test.HTTPurple.ServerSpec where

import Prelude

import Control.Monad.Except (throwError)
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff, bracket)
import Effect.Exception (error)
import Foreign.Object (empty)
import HTTPurple.Request (Request)
import HTTPurple.Response (Response, ok)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic as RG
import Test.HTTPurple.TestHelpers (Test, get, get', getStatus, serveAwaitClosed, serveAwaitStarted, (?=))
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError)

data Route = Test

derive instance Generic Route _

route :: RouteDuplex' Route
route = RD.root $ G.sum
  { "Test": RD.path "test" RG.noArgs
  }

mockRouter :: Request Route -> Aff Response
mockRouter { route: Test } = ok $ RD.print route Test

serveSpec :: Test
serveSpec =
  describe "serve" do
    it "boots a server on the given port" do
      bracket
        (serveAwaitStarted { hostname: "localhost", port: 8080 } { route, router: mockRouter })
        serveAwaitClosed
        \_ -> do
          out <- get 8080 empty "/test"
          out ?= "/test"
    it "responds with a 500 upon unhandled exceptions" do
      let router _ = throwError $ error "fail!"
      bracket
        (serveAwaitStarted { hostname: "localhost", port: 8080 } { route, router })
        serveAwaitClosed
        \_ -> do
          status <- getStatus 8080 empty "/test"
          status ?= 500

serve'Spec :: Test
serve'Spec =
  describe "serve'" do
    it "boots a server with the given options" do
      bracket
        (serveAwaitStarted { hostname: "localhost", port: 8080 } { route, router: mockRouter })
        serveAwaitClosed
        \_ -> do
          out <- get 8080 empty "/test"
          out ?= "/test"

serveSecureSpec :: Test
serveSecureSpec =
  describe "serveSecure" do
    describe "with valid key and cert files" do
      it "boots a server on the given port" do
        bracket
          (serveAwaitStarted { hostname: "localhost", port: 8080, certFile: "./test/Mocks/Certificate.cer", keyFile: "./test/Mocks/Key.key" } { route, router: mockRouter })
          serveAwaitClosed
          \_ -> do
            out <- get' 8080 empty "/test"
            out ?= "/test"
    describe "with invalid key and cert files" do
      it "throws" do
        expectError $ serveAwaitStarted { hostname: "localhost", port: 8080, certFile: "", keyFile: "" } { route, router: mockRouter }

serveSecure'Spec :: Test
serveSecure'Spec =
  describe "serveSecure'" do
    describe "with valid key and cert files" do
      it "boots a server on the given port" do
        let
          options = { hostname: "localhost", port: 8080, certFile: "./test/Mocks/Certificate.cer", keyFile: "./test/Mocks/Key.key" }
        bracket
          (serveAwaitStarted options { route, router: mockRouter })
          serveAwaitClosed
          \_ -> do
            out <- get' 8080 empty "/test"
            out ?= "/test"

serverSpec :: Test
serverSpec =
  describe "Server" do
    serveSpec
    serve'Spec
    serveSecureSpec
    serveSecure'Spec
