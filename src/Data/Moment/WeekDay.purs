module Data.Moment.WeekDay where

import Data.Moment
import Data.Enum
import Data.Maybe
import Data.Function(on)

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday

instance eqWeekDay :: Eq WeekDay where
  (==) Sunday Sunday         = true                
  (==) Monday Monday         = true                
  (==) Tuesday Tuesday       = true            
  (==) Wednesday Wednesday   = true
  (==) Thursday Thursday     = true        
  (==) Friday Friday         = true                
  (==) Saturday Saturday     = true
  (/=) _ _                   = false

instance ordDayOfWeek :: Ord WeekDay where
  compare = compare `on` fromEnum

instance enumWeekDay :: Enum WeekDay where
  cardinality     = Cardinality 7
  firstEnum       = Sunday
  lastEnum        = Saturday

  succ Sunday     = Just Monday
  succ Monday     = Just Tuesday
  succ Tuesday    = Just Wednesday
  succ Wednesday  = Just Thursday
  succ Thursday   = Just Friday
  succ Friday     = Just Saturday
  succ Saturday   = Nothing

  pred Sunday     = Nothing
  pred Monday     = Just Sunday
  pred Tuesday    = Just Monday
  pred Wednesday  = Just Tuesday
  pred Thursday   = Just Wednesday
  pred Friday     = Just Thursday
  pred Saturday   = Just Friday

instance showWeekDay :: Show WeekDay where
  show Sunday     = "Sunday"
  show Monday     = "Monday"
  show Tuesday    = "Tuesday"
  show Wednesday  = "Wednesday"
  show Thursday   = "Thursday"
  show Friday     = "Friday"
  show Saturday   = "Saturday"