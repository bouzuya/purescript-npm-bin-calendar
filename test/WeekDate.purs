module Test.WeekDate
  ( tests
  ) where

import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bind, (<$>), (<<<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import WeekDate as WeekDateFormatter

tests :: TestSuite
tests = suite "WeekDate" do
  test "toISOString" do
    let
      -- 2019-01-02 = 2019-W01-3
      d1 = do
        y <- Enum.toEnum 2019
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        Date.exactDate y m d
    Assert.equal
      (Just "2019-W01-3")
      ((WeekDateFormatter.toISOString <<< WeekDate.fromDate) <$> d1)
