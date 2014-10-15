module Data.Moment.Parse where

import Data.Moment.GetSet(setZone)
import Data.Moment.Duration
import Data.Moment
import Data.Maybe
import Data.Function

type MomentObj = {
    years       :: Duration,
    months      :: Duration,
    days        :: Duration,
    hours       :: Duration,
    minutes     :: Duration,
    seconds     :: Duration,
    milliseconds:: Duration
  }

parseObjImpl  :: { years        :: Number
                 , months       :: Number
                 , days         :: Number
                 , hours        :: Number
                 , minutes      :: Number
                 , seconds      :: Number
                 , milliseconds :: Number } -> Moment

parseObjImpl = unsafeToMoment

parseObj :: MomentObj -> Maybe Moment
parseObj mo = let 
    f   = foldDuration
    mo' = parseObjImpl mo{ years        = f mo.years
                         , months       = f mo.months
                         , days         = f mo.days
                         , hours        = f mo.hours
                         , minutes      = f mo.minutes
                         , seconds      = f mo.seconds
                         , milliseconds = f mo.milliseconds}
  in if isValid mo' then Just mo' else Nothing

foreign import parseUnix """
  function parseUnix(u){ return moment.unix(u); }
""" :: Unix -> Moment

foreign import parseString_ """
  function parseString_(Nothing, Just, strict, fs, s){ 
    var m = moment(s, fs, strict);
    return m.isValid() ? Just(m) : Nothing;
  }
""" :: forall a e. Fn5 (Maybe Moment) (a -> Maybe Moment) Boolean [String] String (Maybe Moment)
parseString = runFn5 parseString_ Nothing Just true
parseStringForgiving = runFn5 parseString_ Nothing Just false

parseEpoch :: Epoch -> Moment
parseEpoch = unsafeToMoment

foreign import unsafeToMoment """ function unsafeToMoment(e){ return moment(e); }
""" :: forall a. a -> Moment

parseStringZ :: [String] -> String -> Maybe Moment
parseStringZ ss s = setZone s <$> parseString ss s