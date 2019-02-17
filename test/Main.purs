module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Main as Main
import Test.Format as Format
import Test.Unit.Main (runTest)
import Test.WeekDate as WeekDate

main :: Effect Unit
main = do
  Main.main
  log "You should add some tests."
  runTest do
    Format.tests
    WeekDate.tests
