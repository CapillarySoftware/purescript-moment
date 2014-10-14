module Data.Moment.Month where

import Data.Moment 
import Data.Enum
import Data.Foldable(elem)
import Data.Array(take, elemIndex, (!!))

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