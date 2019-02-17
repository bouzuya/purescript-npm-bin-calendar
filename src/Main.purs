module Main where

import Prelude

import Calendar (calendarDates)
import Data.Array as Array
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
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Simple.JSON as SimpleJSON
import WeekDate as WeekDate

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  file <-
    maybe
      (throw "Usage: purescript-npm-bin-calendar <FILE>")
      pure
      (Array.head args)
  text <- FS.readTextFile Encoding.UTF8 file
  calendarData <-
    maybe
      (throw "invalid file format")
      pure
      (SimpleJSON.readJSON_ text :: _ (Object Boolean))
  year <- maybe (throw "invalid year") pure (toEnum 2019)
  calendar <- maybe (throw "invalid calendar") pure (calendarDates year)
  Foldable.for_ calendar (Console.log <<< (buildLine calendarData year))
  where
    buildLine calendarData year (Tuple dow wdates) =
      Foldable.intercalate
        " "
        [ Format.dayOfWeekShortName dow
        , Foldable.fold (map (buildChar calendarData year) wdates)
        ]
    buildChar calendarData year wdate =
      let date = WeekDate.toDate wdate
      in
        if Date.year date == year
        then
          case Object.lookup (Format.iso8601Date date) calendarData of
            Just true -> "O"
            Just false -> "_"
            Nothing -> "_"
        else " "
