module Test.Main where

import Prelude

import Effect (Effect)
import Test.Format as Format
import Test.Options as Options
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Format.tests
  Options.tests
