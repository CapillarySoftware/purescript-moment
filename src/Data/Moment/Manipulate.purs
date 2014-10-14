module Data.Moment.Manipulate where

import Data.Moment
import Data.Moment.Duration

import Data.Foreign.OOFFI
import Data.Enum

add :: Duration -> Moment -> Moment
add t = method2' "add" (foldDuration t) (stringDuration t)

subtract :: Duration -> Moment -> Moment
subtract t = method2' "subtract" (foldDuration t) (stringDuration t)

data Granularity  = Second 
                  | Minute
                  | Hour
                  | Day 
                  | Week
                  | Quarter
                  | Month
                  | Year 

-- TODO : write Granularity Enum

instance showGranularity :: Show Granularity where
  show Second  = "second"
  show Minute  = "minute"
  show Hour    = "hour"
  show Day     = "day"
  show Week    = "week"
  show Quarter = "quarter"
  show Month   = "month"
  show Year    = "year"

startOf :: Granularity -> Moment -> Moment
startOf g = method1' "startOf" $ show g

endOf :: Granularity -> Moment -> Moment
endOf g   = method1' "endOf"   $ show g
