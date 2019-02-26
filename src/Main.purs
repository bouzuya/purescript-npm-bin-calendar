module Main where

import Prelude

import Bouzuya.DateTime.WeekDate as WeekDate
import Calendar as Calendar
import Data.Array as Array
import Data.Date as Date
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Format as Format
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream (Readable)
import Node.Stream as Stream
import Simple.JSON as SimpleJSON

readFromStream :: Readable () -> Aff String
readFromStream r = Aff.makeAff \callback -> do
  ref <- Ref.new ""
  Stream.onDataString r Encoding.UTF8 \s -> do
    buffer <- Ref.read ref
    Ref.write (buffer <> s) ref
  Stream.onEnd r do
    buffer <- Ref.read ref
    callback (pure buffer)
  pure mempty

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- liftEffect (map (Array.drop 2) Process.argv) -- TODO
  text <- readFromStream Process.stdin
  calendarData <-
    liftEffect
      (maybe
        (throw "invalid file format")
        pure
        (SimpleJSON.readJSON_ text :: _ (Object Boolean)))
  year <- liftEffect (map Date.year Now.nowDate)
  calendar <-
    liftEffect
      (maybe (throw "invalid calendar") pure (Calendar.calendarDates year))
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
