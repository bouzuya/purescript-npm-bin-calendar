module Test.Options
  ( tests
  ) where

import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Date (Month, Weekday, Year, exactDate)
import Data.Date as Date
import Data.Enum (enumFromTo, toEnum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Options as Options
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Options" do
  test "parse" do
    Assert.equal
      ({ year: Nothing })
      (Options.parse [])
    Assert.equal
      ({ year: Enum.toEnum 2019 :: _ Year })
      (Options.parse ["2019"])
    Assert.equal
      ({ year: Nothing })
      (Options.parse ["abc"])
    Assert.equal
      ({ year: Nothing })
      (Options.parse ["2018", "2019"])
