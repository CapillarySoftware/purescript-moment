module Data.Moment.Manipulate where

import Data.Moment
import Data.Moment.Duration

import Data.Foreign.OOFFI
import Data.Enum
import Data.Maybe
import Data.Function

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

instance eqGranularity :: Eq Granularity where 
  (==) Second   Second  = true
  (==) Minute   Minute  = true
  (==) Hour     Hour    = true
  (==) Day      Day     = true
  (==) Week     Week    = true
  (==) Quarter  Quarter = true
  (==) Month    Month   = true
  (==) Year     Year    = true
  (/=) _ _              = false

instance ordGranularity :: Ord Granularity where
  compare = compare `on` fromEnum

instance enmGranularity :: Enum Granularity where 
  cardinality  = Cardinality 8
  firstEnum    = Second
  lastEnum     = Year
  
  succ Second  = Just Minute
  succ Minute  = Just Hour
  succ Hour    = Just Day
  succ Day     = Just Week
  succ Week    = Just Month
  succ Month   = Just Quarter
  succ Quarter = Just Year
  succ Year    = Nothing

  pred Second  = Nothing
  pred Minute  = Just Second
  pred Hour    = Just Minute
  pred Day     = Just Hour
  pred Week    = Just Day
  pred Month   = Just Week
  pred Quarter = Just Month
  pred Year    = Just Quarter


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
