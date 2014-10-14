module Data.Moment.Duration where

import Data.Moment

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
stringDuration (Milliseconds _) = "ms"
stringDuration (Seconds _)      = "s"
stringDuration (Minutes _)      = "m"
stringDuration (Hours _)        = "h"
stringDuration (Months _)       = "M"
stringDuration (Weeks _)        = "w"
stringDuration (Years _)        = "y"
stringDuration (Days _)         = "d"

instance showDuration :: Show Duration where
  show (Milliseconds x) = "Milliseconds " ++ show x
  show (Seconds x)      = "Seconds "      ++ show x
  show (Minutes x)      = "Minutes "      ++ show x
  show (Hours x)        = "Hours "        ++ show x
  show (Months x)       = "Months "       ++ show x
  show (Weeks x)        = "Weeks "        ++ show x
  show (Years x)        = "Years "        ++ show x
  show (Days x)         = "Days "         ++ show x