module Main where

import Prelude

import Calendar (calendarDates)
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (throw)
import Foreign.Object (Object)
import Foreign.Object as Object
import Format as Format
import WeekDate as WeekDate

calendarData :: Object Boolean
calendarData =
  Object.fromFoldable
    [ Tuple "2019-01-01" true
    , Tuple "2019-01-03" true
    , Tuple "2019-01-05" true
    , Tuple "2019-02-01" true
    , Tuple "2019-03-01" true
    , Tuple "2019-04-01" true
    , Tuple "2019-05-01" true
    , Tuple "2019-06-01" true
    , Tuple "2019-07-01" true
    , Tuple "2019-08-01" true
    , Tuple "2019-09-01" true
    , Tuple "2019-10-01" true
    , Tuple "2019-11-01" true
    , Tuple "2019-12-30" true
    ]

main :: Effect Unit
main = do
  year <- maybe (throw "invalid year") pure (toEnum 2019)
  calendar <- maybe (throw "invalid calendar") pure (calendarDates year)
  Foldable.for_ calendar (Console.log <<< (buildLine year))
  where
    buildLine year (Tuple dow wdates) =
      Foldable.intercalate
        " "
        [ Format.dayOfWeekShortName dow
        , Foldable.fold (map (buildChar year) wdates)
        ]
    buildChar year wdate =
      let date = WeekDate.toDate wdate
      in
        if Date.year date == year
        then
          case Object.lookup (Format.iso8601Date date) calendarData of
            Just true -> "O"
            Just false -> "_"
            Nothing -> "_"
        else " "
