module Format
  ( dayOfWeekShortName
  , iso8601Date
  , monthShortName
  , weekDate
  ) where

import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Array as Array
import Data.Date (Date, Month, Weekday)
import Data.DateTime as DateTime
import Data.Either as Either
import Data.Enum as Enum
import Data.Formatter.DateTime as DateTimeFormatter
import Data.Formatter.Number as NumberFormatter
import Data.Int as Int
import Data.List as List
import Data.String as String
import Partial.Unsafe as Unsafe
import Prelude (bottom, (<<<), otherwise, show, (<), (<>))

-- Mon, Tue, ...
dayOfWeekShortName :: Weekday -> String
dayOfWeekShortName = String.take 3 <<< show

-- YYYY-MM-DD
iso8601Date :: Date -> String
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
monthShortName :: Month -> String
monthShortName = String.take 3 <<< show

-- YYYY-Www-D
weekDate :: WeekDate -> String
weekDate wd =
  let
    y = Enum.fromEnum (WeekDate.weekYear wd)
    woy = Enum.fromEnum (WeekDate.week wd)
    dow = Enum.fromEnum (WeekDate.weekday wd)
  in Array.fold [pad4 y, "-W", pad2 woy, "-", show dow]

pad2 :: Int -> String
pad2 n =
  Unsafe.unsafePartial
    (Either.fromRight
      (NumberFormatter.formatNumber "00" (Int.toNumber n)))

pad4 :: Int -> String
pad4 n =
  Unsafe.unsafePartial
    (Either.fromRight
      (NumberFormatter.formatNumber "0000" (Int.toNumber n)))
