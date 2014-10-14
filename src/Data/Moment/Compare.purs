module Data.Moment.Compare where

import Data.Moment

foreign import max "var max = moment.max;" :: Moment -> Moment -> Moment
foreign import min "var min = moment.min;" :: Moment -> Moment -> Moment