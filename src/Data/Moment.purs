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

type DayOfMonth   = Number
type DayOfYear    = Number
type WeekOfYear   = Number

foreign import initMoment """
  moment().format()
""" :: Unit

foreign import data Moment :: *
foreign import data Now    :: !

foreign import now """
  function now(){ return function(){ return moment(); }; }
""" :: forall e. Eff (now :: Now | e) Moment

method1' :: forall a. String -> a -> Moment -> Moment
method1' s a m = method1 s (clone m) a

method2' :: forall a b. String -> a -> b -> Moment -> Moment
method2' s a b m = method2 s (clone m) a b

isValid :: Moment -> Boolean
isValid = method0 "isValid"

isValidAt :: Moment -> String
isValidAt = method0 "isValidAt"

clone :: Moment -> Moment
clone = method0 "clone"








