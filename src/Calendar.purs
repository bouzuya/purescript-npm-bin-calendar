module Calendar
  ( calendarDates
  ) where

import Bouzuya.DateTime.Date.Extra as DateExtra
import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Date (Weekday, Year)
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (bind, bottom, compose, map, pure, top, (&&), (<<<), (<>), (==))

calendarDates :: Year -> Maybe (Array (Tuple Weekday (Array WeekDate)))
calendarDates year = do
  f <- firstWeekDateOfCalendar year
  l <- lastWeekDateOfCalendar year
  pure
    (Array.zipWith
      Tuple
      weekdays
      (groupByWeekday (Enum.enumFromTo f l :: Array WeekDate)))
  where
    firstWeekDateOfCalendar :: Year -> Maybe WeekDate
    firstWeekDateOfCalendar =
      WeekDate.firstWeekDateOfWeekYear <<<
        WeekDate.weekYear <<<
        WeekDate.fromDate <<<
        DateExtra.firstDateOfYear

    lastWeekDateOfCalendar :: Year -> Maybe WeekDate
    lastWeekDateOfCalendar =
      WeekDate.lastWeekDateOfWeekYear <<<
        WeekDate.weekYear <<<
        WeekDate.fromDate <<<
        DateExtra.lastDateOfYear

    weekdays :: Array Weekday
    weekdays = Enum.enumFromTo bottom top

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
            WeekDate.week a == WeekDate.week b))
