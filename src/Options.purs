module Options
  ( Options
  , parse
  ) where

import Data.Array.NonEmpty as NonEmptyArray
import Data.Date (Year)
import Data.Either as Either
import Data.Enum as Enum
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Partial.Unsafe as Unsafe
import Prelude (bind, join)

type Options =
  { year :: Maybe Year
  }

parse :: Array String -> Options
parse [] = { year: Nothing }
parse [s] =
  { year: do
      matches <-
        Regex.match
          (Unsafe.unsafePartial
            (Either.fromRight (Regex.regex "^(\\d{4})$" RegexFlags.noFlags)))
          s
      yearString <- join (NonEmptyArray.index matches 1)
      yearInt <- Int.fromString yearString
      Enum.toEnum yearInt :: _ Year
  }
parse _ = { year: Nothing }
