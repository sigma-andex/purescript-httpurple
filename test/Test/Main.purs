module Test.Main where

import Prelude

import Effect.Aff (launchAff_)
import Test.HTTPurple.BodySpec (bodySpec)
import Test.HTTPurple.HeadersSpec (headersSpec)
import Test.HTTPurple.IntegrationSpec (integrationSpec)
import Test.HTTPurple.LookupSpec (lookupSpec)
import Test.HTTPurple.MethodSpec (methodSpec)
import Test.HTTPurple.PathSpec (pathSpec)
import Test.HTTPurple.QuerySpec (querySpec)
import Test.HTTPurple.RequestSpec (requestSpec)
import Test.HTTPurple.ResponseSpec (responseSpec)
import Test.HTTPurple.ServerSpec (serverSpec)
import Test.HTTPurple.StatusSpec (statusSpec)
import Test.HTTPurple.TestHelpers (TestSuite)
import Test.HTTPurple.UtilsSpec (utilsSpec)
import Test.HTTPurple.VersionSpec (versionSpec)
import Test.Spec (describe)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

main :: TestSuite
main = launchAff_ $ runSpec [ specReporter ] $ describe "HTTPurple" do
  bodySpec
  headersSpec
  lookupSpec
  methodSpec
  pathSpec
  querySpec
  requestSpec
  responseSpec
  serverSpec
  statusSpec
  utilsSpec
  versionSpec
  integrationSpec
