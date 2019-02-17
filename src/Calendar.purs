module Calendar
  ( calendarDates
  ) where

import Bouzuya.DateTime (Weekday)
import Bouzuya.DateTime as BouzuyaDateTime
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Date (Date, Year)
import Data.Date as Date
import Data.Enum (enumFromTo)
import Data.Foldable as Foldable
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (bind, bottom, compose, map, pure, top, (&&), (<>), (==))
import WeekDate (WeekDate)
import WeekDate as WeekDate

calendarDates :: Year -> Maybe (Array (Tuple Weekday (Array WeekDate)))
calendarDates year = do
  f <- firstDayOfCalendar year
  l <- lastDayOfCalendar year
  pure
    (Array.zipWith
      Tuple
      weekdays
      (groupByWeekday (map WeekDate.toWeekDate (enumFromTo f l :: Array Date))))
  where
    firstDayOfCalendar :: Year -> Maybe Date
    firstDayOfCalendar y = do
      d <- Date.exactDate y bottom bottom
      BouzuyaDateTime.exactDateFromWeekOfYear
        (BouzuyaDateTime.weekYear d)
        (BouzuyaDateTime.weekOfYear d)
        bottom

    lastDayOfCalendar :: Year -> Maybe Date
    lastDayOfCalendar y = do
      d <- Date.exactDate y top top
      BouzuyaDateTime.exactDateFromWeekOfYear
        (BouzuyaDateTime.weekYear d)
        (BouzuyaDateTime.weekOfYear d)
        top

    weekdays :: Array Weekday
    weekdays = enumFromTo bottom top

    groupByWeekday :: Array WeekDate -> Array (Array WeekDate)
    groupByWeekday = -- [ [W53-1, W01-1, ...], [W53-2, W01-2, ...], ... ]
      compose
        (Foldable.foldl
          (\b a -> Array.zipWith (<>) b (map Array.singleton a))
          (Array.replicate (Array.length weekdays) []))
        groupByWeek

    groupByWeek :: Array WeekDate -> Array (Array WeekDate)
    groupByWeek = -- [ [W53-1, W53-2, ...], [W01-1, W01-2, ...], ...]
      compose
        (map NonEmptyArray.toArray)
        (Array.groupBy
          (\a b ->
            WeekDate.weekYear a == WeekDate.weekYear b &&
            WeekDate.weekOfYear a == WeekDate.weekOfYear b))
