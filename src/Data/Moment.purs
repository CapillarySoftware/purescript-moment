module Data.Moment where

import Control.Monad.Eff
import Data.Function
import Data.Foreign.OOFFI
import Data.Maybe
import Data.Enum
import Data.Foldable
import Data.Tuple
import Data.Array

-- milliseconds
type Epoch        = Number 
-- seconds
type Unix         = Number 
-- timezone
type Zone         = Number

data Time         = Milliseconds Number
                  | Seconds Number
                  | Minutes Number
                  | Hours Number 
                  | Months Number
                  | Weeks Number
                  | Years Number
                  | Days Number

foldTime :: Time -> Number
foldTime (Milliseconds x) = x
foldTime (Seconds x)      = x
foldTime (Minutes x)      = x
foldTime (Hours x)        = x
foldTime (Months x)       = x
foldTime (Weeks x)        = x
foldTime (Years x)        = x
foldTime (Days x)         = x

stringTime :: Time -> String
stringTime (Milliseconds _) = "ms"
stringTime (Seconds _)      = "s"
stringTime (Minutes _)      = "m"
stringTime (Hours _)        = "h"
stringTime (Months _)       = "M"
stringTime (Weeks _)        = "w"
stringTime (Years _)        = "y"
stringTime (Days _)         = "d"

instance showTime :: Show Time where
  show (Milliseconds x) = "Milliseconds " ++ show x
  show (Seconds x)      = "Seconds "      ++ show x
  show (Minutes x)      = "Minutes "      ++ show x
  show (Hours x)        = "Hours "        ++ show x
  show (Months x)       = "Months "       ++ show x
  show (Weeks x)        = "Weeks "        ++ show x
  show (Years x)        = "Years "        ++ show x
  show (Days x)         = "Days "         ++ show x

type DayOfMonth   = Number
type DayOfYear    = Number
type WeekOfYear   = Number
type Quarter      = Number
type Year         = Number

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday

weekDays = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

instance eqWeekDay :: Eq WeekDay where
  (==) Sunday Sunday         = true                
  (==) Monday Monday         = true                
  (==) Tuesday Tuesday       = true            
  (==) Wednesday Wednesday   = true
  (==) Thursday Thursday     = true        
  (==) Friday Friday         = true                
  (==) Saturday Saturday     = true
  (/=) _ _                   = false

instance ordWeekDay :: Ord WeekDay where
  compare a b | a == b = EQ
  compare a b = if elem b $ take (elemIndex a weekDays) weekDays
                then LT 
                else GT

instance enumWeekDay :: Enum WeekDay where
  cardinality     = Cardinality 7
  firstEnum       = Sunday
  lastEnum        = Saturday
  succ a = weekDays !! (elemIndex a weekDays + 1)
  pred a = weekDays !! (elemIndex a weekDays - 1)

instance showWeekDay :: Show WeekDay where
  show Sunday     = "Sunday"
  show Monday     = "Monday"
  show Tuesday    = "Tuesday"
  show Wednesday  = "Wednesday"
  show Thursday   = "Thursday"
  show Friday     = "Friday"
  show Saturday   = "Saturday"

data Month = January
           | February 
           | March    
           | April    
           | May      
           | June     
           | July     
           | August   
           | September
           | October  
           | November 
           | December 

months = [January, February, March, April, May, June, July, August, September, October, November, December]

instance eqMonth :: Eq Month where 
  (==) January   January   = true
  (==) February  February  = true
  (==) March     March     = true
  (==) April     April     = true
  (==) May       May       = true
  (==) June      June      = true
  (==) July      July      = true
  (==) August    August    = true
  (==) September September = true
  (==) October   October   = true
  (==) November  November  = true
  (==) December  December  = true
  (/=) _ _ = false

instance ordMonth :: Ord Month where
  compare a b | a == b = EQ
  compare a b = if elem b $ take (elemIndex a months) months
                then LT 
                else GT

instance enumMonth :: Enum Month where
  cardinality     = Cardinality 12
  firstEnum       = January
  lastEnum        = December
  succ a = months !! (elemIndex a months + 1)
  pred a = months !! (elemIndex a months - 1)

instance showMonth :: Show Month where
  show January   = "January"
  show February  = "February"
  show March     = "March"
  show April     = "April"
  show May       = "May"
  show June      = "June"
  show July      = "July"
  show August    = "August"
  show September = "September"
  show October   = "October"
  show November  = "November"
  show December  = "December"

type MomentObj = {
    years       :: Time,
    months      :: Time,
    days        :: Time,
    hours       :: Time,
    minutes     :: Time,
    seconds     :: Time,
    milliseconds:: Time
  }

parseObjImpl  :: { years        :: Number
                 , months       :: Number
                 , days         :: Number
                 , hours        :: Number
                 , minutes      :: Number
                 , seconds      :: Number
                 , milliseconds :: Number } -> Moment

parseObjImpl = unsafeToMoment

parseObj :: MomentObj -> Maybe Moment
parseObj mo = let 
    f   = foldTime
    mo' = parseObjImpl mo{ years        = f mo.years
                         , months       = f mo.months
                         , days         = f mo.days
                         , hours        = f mo.hours
                         , minutes      = f mo.minutes
                         , seconds      = f mo.seconds
                         , milliseconds = f mo.milliseconds}
  in if isValid mo' then Just mo' else Nothing

foreign import initMoment """
  moment().format()
""" :: Unit

foreign import data Moment :: *
foreign import data Now    :: !

foreign import now """
  function now(){ return function(){ return moment(); }; }
""" :: forall e. Eff (now :: Now | e) Moment

foreign import parseUnix """
  function parseUnix(u){ return moment.unix(u); }
""" :: Unix -> Moment

foreign import parseString_ """
  function parseString_(Nothing, Just, fs, s){ 
    var m = moment(s, fs, true);
    return m.isValid() ? Just(m) : Nothing;
  }
""" :: forall a e. Fn4 (Maybe Moment) (a -> Maybe Moment) [String] String (Maybe Moment)
parseString = runFn4 parseString_ Nothing Just

parseEpoch :: Epoch -> Moment
parseEpoch = unsafeToMoment

foreign import unsafeToMoment """ function unsafeToMoment(e){ return m(e); }
""" :: forall a. a -> Moment

method1' :: forall a. String -> a -> Moment -> Moment
method1' s a m = method1 s (clone m) a

method2' :: forall a b. String -> a -> b -> Moment -> Moment
method2' s a b m = method2 s (clone m) a b

valueOf :: Moment -> Epoch
valueOf = method0 "valueOf" 

getZone :: Moment -> Zone
getZone = method0 "zone"

isValid :: Moment -> Boolean
isValid = method0 "isValid"

isValidAt :: Moment -> String
isValidAt = method0 "isValidAt"

clone :: Moment -> Moment
clone = method0 "clone"

setZone :: String -> Moment -> Moment
setZone = method1' "zone"

parseStringZ :: [String] -> String -> Maybe Moment
parseStringZ ss s = setZone s <$> parseString ss s

milliseconds :: Moment -> Time 
milliseconds = method0 "milliseconds" >>> Milliseconds

setMilliseconds :: Time -> Moment -> Moment
setMilliseconds (Milliseconds m) = method1' "milliseconds" m

seconds :: Moment -> Time
seconds = method0 "seconds" >>> Seconds

setSeconds :: Time -> Moment -> Moment
setSeconds (Seconds s) = method1' "seconds" s

minutes :: Moment -> Time
minutes = method0 "minutes" >>> Minutes

setMinutes :: Time -> Moment -> Moment
setMinutes (Minutes m) = method1' "minutes" m

hours :: Moment -> Time
hours = method0 "hours" >>> Hours

setHours :: Time -> Moment -> Moment
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

quarter :: Moment -> Quarter
quarter = method0 "quarter"

setQuarter :: Quarter -> Moment -> Moment
setQuarter = method1' "quarter"

year :: Moment -> Year
year = method0 "year"

setYear :: Year -> Moment -> Moment
setYear = method1' "year"

foreign import max "var max = moment.max;" :: Moment -> Moment -> Moment
foreign import min "var min = moment.min;" :: Moment -> Moment -> Moment

add :: Time -> Moment -> Moment
add t = method2' "add" (foldTime t) (stringTime t)

subtract :: Time -> Moment -> Moment
subtract t = method2' "subtract" (foldTime t) (stringTime t)