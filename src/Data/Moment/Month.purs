module Data.Moment.Month where

import Data.Moment 
import Data.Enum
import Data.Maybe
import Data.Function(on)

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
  compare = compare `on` fromEnum

instance enumMonth :: Enum Month where
  cardinality     = Cardinality 12
  firstEnum       = January
  lastEnum        = December

  succ January    = Just February
  succ February   = Just March
  succ March      = Just April
  succ April      = Just May
  succ May        = Just June
  succ June       = Just July
  succ July       = Just August
  succ August     = Just September
  succ September  = Just October
  succ October    = Just November
  succ November   = Just December
  succ December   = Nothing

  pred January    = Nothing
  pred February   = Just January
  pred March      = Just February
  pred April      = Just March
  pred May        = Just April
  pred June       = Just May
  pred July       = Just June
  pred August     = Just July
  pred September  = Just August
  pred October    = Just September
  pred November   = Just October
  pred December   = Just November

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