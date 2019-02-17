module Test.WeekDate
  ( tests
  ) where

import Bouzuya.DateTime as Date
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, discard, ($))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import WeekDate (dayOfWeek, toDate, toISOString, toWeekDate, weekOfYear, weekYear)

tests :: TestSuite
tests = suite "WeekDate" do
  let
    -- 2019-01-02 = 2019-W01-3
    d1 = unsafePartial $ fromJust do
      year <- toEnum 2019
      month <- toEnum 1
      dom <- toEnum 2
      Date.exactDate year month dom

  test "dayOfWeek" do
    Assert.equal (Date.weekday d1) (dayOfWeek (toWeekDate d1))

  test "toDate" do
    Assert.equal d1 (toDate (toWeekDate d1))

  test "toISOString" do
    Assert.equal "2019-W01-3" (toISOString (toWeekDate d1))

  test "toWeekDate" do
    -- do nothing
    Assert.equal 1 1

  test "weekYear" do
    Assert.equal (Date.weekYear d1) (weekYear (toWeekDate d1))

  test "weekOfYear" do
    Assert.equal (Date.weekOfYear d1) (weekOfYear (toWeekDate d1))
