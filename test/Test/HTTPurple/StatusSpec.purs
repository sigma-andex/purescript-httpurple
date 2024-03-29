module Test.HTTPurple.StatusSpec where

import Prelude

import Effect.Class (liftEffect)
import HTTPurple.Status (write)
import Test.HTTPurple.TestHelpers (Test, getResponseStatus, mockResponse, (?=))
import Test.Spec (describe, it)

writeSpec :: Test
writeSpec =
  describe "write" do
    it "writes the given status code" do
      status <-
        liftEffect do
          mock <- mockResponse
          write mock 123
          pure $ getResponseStatus mock
      status ?= 123

statusSpec :: Test
statusSpec =
  describe "Status" do
    writeSpec
