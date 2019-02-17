module Test.Format
  ( tests
  ) where

import Data.Date (Month, Weekday, exactDate)
import Data.Enum (enumFromTo, toEnum)
import Data.Maybe (Maybe(..))
import Format (dayOfWeekShortName, iso8601Date, monthShortName)
import Prelude (bind, bottom, discard, top, (<$>))
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
