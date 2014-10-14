module Data.Moment.GetSet where

import Data.Moment
import Data.Moment.Duration
import Data.Moment.Month
import Data.Moment.WeekDay
import Data.Foreign.OOFFI
import Data.Maybe
import Data.Enum

milliseconds :: Moment -> Duration 
milliseconds = method0 "milliseconds" >>> Milliseconds

setMilliseconds :: Duration -> Moment -> Moment
setMilliseconds (Milliseconds m) = method1' "milliseconds" m

seconds :: Moment -> Duration
seconds = method0 "seconds" >>> Seconds

setSeconds :: Duration -> Moment -> Moment
setSeconds (Seconds s) = method1' "seconds" s

minutes :: Moment -> Duration
minutes = method0 "minutes" >>> Minutes

setMinutes :: Duration -> Moment -> Moment
setMinutes (Minutes m) = method1' "minutes" m

hours :: Moment -> Duration
hours = method0 "hours" >>> Hours

setHours :: Duration -> Moment -> Moment
setHours (Hours h) = method1' "hours" h 

dayOfMonth :: Moment -> DayOfMonth
dayOfMonth = method0 "dates"

setDayOfMonth :: DayOfMonth -> Moment -> Moment
setDayOfMonth = method1' "dates"

dayOfWeek :: Moment -> Maybe WeekDay
dayOfWeek = method0 "days" >>> toEnum

setDayOfWeek :: WeekDay -> Moment -> Moment
setDayOfWeek = fromEnum >>> method1' "days"

dayOfWeek' :: Moment -> Maybe WeekDay
dayOfWeek' = method0 "weekday" >>> toEnum

setDayOfWeek' :: WeekDay -> Moment -> Moment
setDayOfWeek' = fromEnum >>> method1' "weekday"

dayOfYear :: Moment -> DayOfYear
dayOfYear = method0 "dayOfYear"

setDayOfYear :: DayOfYear -> Moment -> Moment
setDayOfYear = method1' "dayOfYear"

weekOfYear :: Moment -> WeekOfYear
weekOfYear = method0 "weeks"

setWeekOfYear :: WeekOfYear -> Moment -> Moment
setWeekOfYear = method1' "weeks"

month :: Moment -> Maybe Month
month = method0 "month" >>> toEnum

setMonth :: Month -> Moment -> Moment
setMonth = fromEnum >>> method1' "month"

quarter :: Moment -> Number
quarter = method0 "quarter"

setQuarter :: Number -> Moment -> Moment
setQuarter = method1' "quarter"

year :: Moment -> Number
year = method0 "year"

setYear :: Number -> Moment -> Moment
setYear = method1' "year"

setZone :: String -> Moment -> Moment
setZone = method1' "zone"

getZone :: Moment -> Zone
getZone = method0 "zone"

valueOf :: Moment -> Epoch
valueOf = method0 "valueOf" 