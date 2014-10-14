module Data.Moment.WeekDay where

import Data.Moment
import Data.Enum
import Data.Foldable(elem)
import Data.Array(take, elemIndex, (!!))

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