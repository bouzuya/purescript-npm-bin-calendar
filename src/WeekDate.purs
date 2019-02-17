module WeekDate
  ( WeekDate
  , dayOfWeek
  , toDate
  , toISOString
  , toWeekDate
  , weekYear
  , weekOfYear
  ) where

import Bouzuya.DateTime as BouzuyaDate
import Data.Enum (class Enum, fromEnum, pred, succ)
import Data.Foldable (intercalate)
import Data.Tuple.Nested as Tuple
import Prelude (class Eq, class Ord, class Show, bind, compare, eq, otherwise, pure, show, (<>), (<))

data WeekDate =
  WeekDate
    (
      Tuple.Tuple4
        BouzuyaDate.Year
        BouzuyaDate.WeekOfYear
        BouzuyaDate.Weekday
        BouzuyaDate.Date
    )

instance enumWeekDate :: Enum WeekDate where
  succ wd = do
    d <- succ (toDate wd)
    pure (toWeekDate d)
  pred wd = do
    d <- pred (toDate wd)
    pure (toWeekDate d)

instance eqWeekDate :: Eq WeekDate where
  eq a b = eq (toDate a) (toDate b)

instance ordWeekDate :: Ord WeekDate where
  compare a b = compare (toDate a) (toDate b)

instance showWeekDate :: Show WeekDate where
  show wd = "(WeekDate " <> toISOString wd <> ")"

dayOfWeek :: WeekDate -> BouzuyaDate.Weekday
dayOfWeek (WeekDate t) = Tuple.get3 t

toDate :: WeekDate -> BouzuyaDate.Date
toDate (WeekDate t) = Tuple.get4 t

toISOString :: WeekDate -> String
toISOString wd =
  let
    y = fromEnum (weekYear wd)
    woy = fromEnum (weekOfYear wd)
    dow = fromEnum (dayOfWeek wd)
    pad2 n
      | n < 10 = "0" <> show n
      | otherwise = show n
    pad4 n
      | n < 10 = "000" <> show n
      | n < 100 = "00" <> show n
      | n < 1000 = "0" <> show n
      | otherwise = show n
  in intercalate "" [pad4 y, "-W", pad2 woy, "-", show dow]

toWeekDate :: BouzuyaDate.Date -> WeekDate
toWeekDate d =
  WeekDate
    ( Tuple.tuple4
        (BouzuyaDate.weekYear d)
        (BouzuyaDate.weekOfYear d)
        (BouzuyaDate.weekday d)
        d)

weekYear :: WeekDate -> BouzuyaDate.Year
weekYear (WeekDate t) = Tuple.get1 t

weekOfYear :: WeekDate -> BouzuyaDate.WeekOfYear
weekOfYear (WeekDate t) = Tuple.get2 t
