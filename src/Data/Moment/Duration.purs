module Data.Moment.Duration where

import Data.Moment
import Data.Function
import Data.Foreign.OOFFI

data Duration = Milliseconds Number
              | Seconds Number
              | Minutes Number
              | Hours Number 
              | Months Number
              | Weeks Number
              | Years Number
              | Days Number

foldDuration :: Duration -> Number
foldDuration (Milliseconds x) = x
foldDuration (Seconds x)      = x
foldDuration (Minutes x)      = x
foldDuration (Hours x)        = x
foldDuration (Months x)       = x
foldDuration (Weeks x)        = x
foldDuration (Years x)        = x
foldDuration (Days x)         = x

stringDuration :: Duration -> String
stringDuration (Milliseconds _) = "milliseconds"
stringDuration (Seconds _)      = "seconds"
stringDuration (Minutes _)      = "minutes"
stringDuration (Hours _)        = "hours"
stringDuration (Months _)       = "months"
stringDuration (Weeks _)        = "weeks"
stringDuration (Years _)        = "years"
stringDuration (Days _)         = "days"

instance showDuration :: Show Duration where
  show (Milliseconds x) = "Milliseconds " ++ show x
  show (Seconds x)      = "Seconds "      ++ show x
  show (Minutes x)      = "Minutes "      ++ show x
  show (Hours x)        = "Hours "        ++ show x
  show (Months x)       = "Months "       ++ show x
  show (Weeks x)        = "Weeks "        ++ show x
  show (Years x)        = "Years "        ++ show x
  show (Days x)         = "Days "         ++ show x

foreign import data MDuration :: *

foreign import durToMDurImpl """
  function durToMDurImpl(n, s){
    return moment.duration(n, s);
  }
""" :: Fn2 Number String MDuration

durToMDur :: Duration -> MDuration
durToMDur d = runFn2 durToMDurImpl (foldDuration d) (stringDuration d)

humanize :: Duration -> String
humanize = durToMDur >>> method0 "humanize"

humanize' :: Duration -> String
humanize' = durToMDur >>> flip (method1 "humanize") true

asMilliseconds :: Duration -> Duration
asMilliseconds = durToMDur >>> method0 "asMilliseconds" >>> Milliseconds

asSeconds :: Duration -> Duration
asSeconds = durToMDur >>> method0 "asSeconds" >>> Seconds

asMinutes :: Duration -> Duration
asMinutes = durToMDur >>> method0 "asMinutes" >>> Minutes

asHours :: Duration -> Duration
asHours = durToMDur >>> method0 "asHours" >>> Hours

asDays :: Duration -> Duration
asDays = durToMDur >>> method0 "asDays" >>> Days

asMonths :: Duration -> Duration
asMonths = durToMDur >>> method0 "asMonths" >>> Months

asYears :: Duration -> Duration
asYears = durToMDur >>> method0 "asYears" >>> Years