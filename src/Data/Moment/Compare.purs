module Data.Moment.Compare where

import Data.Moment
import Data.Moment.Manipulate
import Data.Moment.Duration
import Data.Moment.GetSet
import Data.Foreign.OOFFI

foreign import max "var max = moment.max;" :: Moment -> Moment -> Moment
foreign import min "var min = moment.min;" :: Moment -> Moment -> Moment

isSame :: Moment -> Moment -> Boolean
isSame = method1 "isSame"

isSameG :: Granularity -> Moment -> Moment -> Boolean
isSameG g m m' = method2 "isSame" m m' $ show g 

isAfter :: Moment -> Moment -> Boolean
isAfter = method1 "isAfter"

isBefore :: Moment -> Moment -> Boolean
isBefore = method1 "isBefore"

instance eqMoment :: Eq Moment where
  (==) = isSame 
  (/=) m m' = not $ m == m'

instance ordMoment :: Ord Moment where
  compare m m' | m == m'         = EQ 
  compare m m' | m `isAfter` m'  = GT
  compare m m' | m `isBefore` m' = LT

instance showMoment :: Show Moment where
  show m = "Moment " ++ show (milliseconds m)