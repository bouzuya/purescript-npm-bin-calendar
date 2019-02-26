module WeekDate
  ( toISOString
  ) where

import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Enum as Enum
import Data.Foldable (intercalate)
import Prelude (otherwise, show, (<), (<>))

-- YYYY-Www-D
toISOString :: WeekDate -> String
toISOString wd =
  let
    y = Enum.fromEnum (WeekDate.weekYear wd)
    woy = Enum.fromEnum (WeekDate.week wd)
    dow = Enum.fromEnum (WeekDate.weekday wd)
    pad2 n
      | n < 10 = "0" <> show n
      | otherwise = show n
    pad4 n
      | n < 10 = "000" <> show n
      | n < 100 = "00" <> show n
      | n < 1000 = "0" <> show n
      | otherwise = show n
  in intercalate "" [pad4 y, "-W", pad2 woy, "-", show dow]
