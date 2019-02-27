module Main where

import Prelude

import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Calendar as Calendar
import Data.Array as Array
import Data.Date (Weekday, Year)
import Data.Date as Date
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Now as Now
import Effect.Ref as Ref
import Format as Format
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream (Readable)
import Node.Stream as Stream
import Options as Options
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
  args <- liftEffect (map (Array.drop 2) Process.argv)
  options <- pure (Options.parse args)
  year <-
    Maybe.maybe
      (liftEffect (map Date.year Now.nowDate))
      pure
      options.year
  text <- readFromStream Process.stdin
  calendarData <-
    liftEffect
      (Maybe.maybe
        (throw "invalid file format")
        pure
        (SimpleJSON.readJSON_ text :: _ (Array String)))
  calendar <-
    liftEffect
      (Maybe.maybe
        (throw "invalid calendar")
        pure
        (Calendar.calendarDates year))
  Console.log (Format.calendar calendar calendarData year)
