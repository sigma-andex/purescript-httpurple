module Test.HTTPurple.CookieSpec where

import Prelude

import Data.Array (head) as Array
import Data.Map (empty, fromFoldable, singleton) as Map
import Data.Maybe (fromMaybe)
import Data.String (Pattern(Pattern), split)
import Data.Tuple (Tuple(Tuple))
import HTTPurple.Cookie (SameSite(Lax), addCookie, clearCookie, clearCookie', cookie, cookie', cookieHeader, expireCookie, requestCookies, serialize, setCookie, setCookie')
import HTTPurple.Headers (empty, header, mkRequestHeaders)
import Test.HTTPurple.TestHelpers ((?=))
import Test.HTTPurple.TestHelpers as TestHelpers
import Test.Spec (describe, it)

requestCookiesSpec :: TestHelpers.Test
requestCookiesSpec =
  describe "requestCookies" do
    describe "when there is no Cookie header" do
      it "is an empty map" do
        requestCookies (mkRequestHeaders []) ?= Map.empty
    describe "with a single cookie" do
      it "parses the name and value" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "foo=bar" ])
          ?= Map.singleton "foo" "bar"
    describe "with multiple cookies" do
      it "parses each pair" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "foo=bar; baz=qux" ])
          ?= Map.fromFoldable [ Tuple "foo" "bar", Tuple "baz" "qux" ]
    describe "with a url-encoded value" do
      it "decodes the value" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "foo=a%20b" ])
          ?= Map.singleton "foo" "a b"
    describe "with a value containing '='" do
      it "splits only on the first '='" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "foo=a=b" ])
          ?= Map.singleton "foo" "a=b"
    describe "with empty segments" do
      it "ignores them" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "; foo=bar; " ])
          ?= Map.singleton "foo" "bar"
    describe "with duplicate names" do
      it "keeps the first occurrence" do
        requestCookies (mkRequestHeaders [ Tuple "Cookie" "foo=a; foo=b" ])
          ?= Map.singleton "foo" "a"

serializeSpec :: TestHelpers.Test
serializeSpec =
  describe "serialize" do
    describe "with no explicit attributes" do
      it "applies the secure defaults" do
        serialize (cookie "foo" "bar") ?= "foo=bar; SameSite=Lax; HttpOnly; Secure"
    describe "with a value needing encoding" do
      it "url-encodes the value" do
        serialize (cookie "foo" "a b") ?= "foo=a%20b; SameSite=Lax; HttpOnly; Secure"
    describe "with all attributes set" do
      it "renders each attribute" do
        let
          c = cookie' "foo" "bar"
            { path: "/"
            , domain: "example.com"
            , maxAge: 3600
            , httpOnly: true
            , secure: true
            , sameSite: Lax
            }
        serialize c
          ?= "foo=bar; Path=/; Domain=example.com; Max-Age=3600; SameSite=Lax; HttpOnly; Secure"

setCookieSpec :: TestHelpers.Test
setCookieSpec =
  describe "setCookie" do
    it "builds a single Set-Cookie header with the secure defaults" do
      setCookie "foo" "bar" ?= header "Set-Cookie" "foo=bar; SameSite=Lax; HttpOnly; Secure"
    describe "setCookie'" do
      it "keeps the secure defaults for attributes not given" do
        setCookie' "foo" "bar" { httpOnly: false } ?= header "Set-Cookie" "foo=bar; SameSite=Lax; Secure"

cookieHeaderSpec :: TestHelpers.Test
cookieHeaderSpec =
  describe "cookieHeader" do
    it "builds a single Set-Cookie header from a Cookie" do
      cookieHeader (cookie "foo" "bar") ?= header "Set-Cookie" "foo=bar; SameSite=Lax; HttpOnly; Secure"

addCookieSpec :: TestHelpers.Test
addCookieSpec =
  describe "addCookie" do
    it "accumulates multiple Set-Cookie values under one header" do
      let
        result =
          addCookie (cookie "b" "2")
            $ addCookie (cookie "a" "1") empty
      show result ?= "Set-Cookie: a=1; SameSite=Lax; HttpOnly; Secure\nSet-Cookie: b=2; SameSite=Lax; HttpOnly; Secure\n\n"

expireCookieSpec :: TestHelpers.Test
expireCookieSpec =
  describe "expireCookie" do
    it "sets an empty value and Max-Age=0" do
      cookieHeader (expireCookie "foo") ?= header "Set-Cookie" "foo=; Max-Age=0; SameSite=Lax; HttpOnly; Secure"
    it "composes with addCookie" do
      let
        result =
          addCookie (cookie "theme" "dark")
            $ addCookie (expireCookie "session") empty
      show result ?= "Set-Cookie: session=; Max-Age=0; SameSite=Lax; HttpOnly; Secure\nSet-Cookie: theme=dark; SameSite=Lax; HttpOnly; Secure\n\n"

clearCookieSpec :: TestHelpers.Test
clearCookieSpec =
  describe "clearCookie" do
    it "sets an empty value and Max-Age=0" do
      clearCookie "foo" ?= header "Set-Cookie" "foo=; Max-Age=0; SameSite=Lax; HttpOnly; Secure"
    describe "clearCookie'" do
      it "keeps a matching path so the client actually clears it" do
        clearCookie' "foo" { path: "/app" } ?= header "Set-Cookie" "foo=; Path=/app; Max-Age=0; SameSite=Lax; HttpOnly; Secure"

roundtripSpec :: TestHelpers.Test
roundtripSpec =
  describe "serialize/requestCookies symmetry" do
    it "round-trips names and values that need url-encoding" do
      let
        name = "a b"
        value = "x=y z"
        -- the name=value pair a client would echo back, i.e. the first serialized segment
        pair = fromMaybe "" $ Array.head $ split (Pattern "; ") $ serialize (cookie name value)
      requestCookies (mkRequestHeaders [ Tuple "Cookie" pair ]) ?= Map.singleton name value

cookieSpec :: TestHelpers.Test
cookieSpec =
  describe "Cookie" do
    requestCookiesSpec
    serializeSpec
    setCookieSpec
    cookieHeaderSpec
    addCookieSpec
    expireCookieSpec
    clearCookieSpec
    roundtripSpec
