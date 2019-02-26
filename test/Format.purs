module Test.Format
  ( tests
  ) where

import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Date (Month, Weekday, exactDate)
import Data.Date as Date
import Data.Enum (enumFromTo, toEnum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Format (dayOfWeekShortName, iso8601Date, monthShortName)
import Format as Format
import Prelude (bind, bottom, discard, top, (<$>), (<<<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Format" do
  test "dayOfWeekShortName" do
    Assert.equal
      (["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])
      (dayOfWeekShortName <$> enumFromTo bottom top :: Array Weekday)

  test "iso8601Date" do
    Assert.equal
      (Just "2019-01-02")
      (iso8601Date <$> do
        year <- toEnum 2019
        month <- toEnum 1
        dom <- toEnum 2
        exactDate year month dom)

  test "monthShortName" do
    Assert.equal
      [ "Jan"
      , "Feb"
      , "Mar"
      , "Apr"
      , "May"
      , "Jun"
      , "Jul"
      , "Aug"
      , "Sep"
      , "Oct"
      , "Nov"
      , "Dec"
      ]
      (monthShortName <$> enumFromTo bottom top :: Array Month)

  test "weekDate" do
    let
      -- 2019-01-02 = 2019-W01-3
      d1 = do
        y <- Enum.toEnum 2019
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        Date.exactDate y m d
    Assert.equal
      (Just "2019-W01-3")
      ((Format.weekDate <<< WeekDate.fromDate) <$> d1)
