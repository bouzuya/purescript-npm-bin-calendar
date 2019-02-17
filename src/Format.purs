module Format
  ( dayOfWeekShortName
  , iso8601Date
  , monthShortName
  ) where

import Data.Date as Date
import Data.DateTime as DateTime
import Data.Formatter.DateTime as DateTimeFormatter
import Data.List as List
import Data.String as String
import Prelude (bottom, show, (<<<))

-- Mon, Tue, ...
dayOfWeekShortName :: Date.Weekday -> String
dayOfWeekShortName = String.take 3 <<< show

-- YYYY-MM-DD
iso8601Date :: Date.Date -> String
iso8601Date d =
  DateTimeFormatter.format
  (
    List.fromFoldable
    [ DateTimeFormatter.YearFull
    , DateTimeFormatter.Placeholder "-"
    , DateTimeFormatter.MonthTwoDigits
    , DateTimeFormatter.Placeholder "-"
    , DateTimeFormatter.DayOfMonthTwoDigits
    ]
  )
  (DateTime.DateTime d bottom)

-- Jan, Feb, ...
monthShortName :: Date.Month -> String
monthShortName = String.take 3 <<< show
