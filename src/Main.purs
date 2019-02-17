module Main where

import Prelude

import Calendar (calendarDates)
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Foldable as Foldable
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (throw)
import Format as Format
import WeekDate as WeekDate

main :: Effect Unit
main = do
  year <- maybe (throw "invalid year") pure (toEnum 2019)
  calendar <- maybe (throw "invalid calendar") pure (calendarDates year)
  Foldable.for_ calendar (Console.log <<< buildLine year)
  where
    buildLine year (Tuple dow wdates) =
      Foldable.intercalate
        " "
        [ Format.dayOfWeekShortName dow
        , Foldable.fold (map (buildChar year) wdates)
        ]
    buildChar year wdate =
      let date = WeekDate.toDate wdate
      in if Date.year date == year then "_" else " "
