# Module Documentation

## Module Data.Moment

### Types

    type DayOfMonth  = Number

    type DayOfYear  = Number

    type Epoch  = Number

    data Moment :: *

    data Now :: !

    type Unix  = Number

    type WeekOfYear  = Number

    type Zone  = Number


### Values

    clone :: Moment -> Moment

    format :: String -> Moment -> String

    initMoment :: Unit

    invalid :: Moment

    isValid :: Moment -> Boolean

    isValidAt :: Moment -> String

    method1' :: forall a. String -> a -> Moment -> Moment

    method2' :: forall a b. String -> a -> b -> Moment -> Moment

    now :: forall e. Eff (now :: Now | e) Moment


## Module Data.Moment.Compare

### Values

    max :: Moment -> Moment -> Moment

    min :: Moment -> Moment -> Moment


## Module Data.Moment.Duration

### Types

    data Duration where
      Milliseconds :: Number -> Duration
      Seconds :: Number -> Duration
      Minutes :: Number -> Duration
      Hours :: Number -> Duration
      Months :: Number -> Duration
      Weeks :: Number -> Duration
      Years :: Number -> Duration
      Days :: Number -> Duration

    data MDuration :: *


### Type Class Instances

    instance showDuration :: Show Duration


### Values

    asDays :: Duration -> Duration

    asHours :: Duration -> Duration

    asMilliseconds :: Duration -> Duration

    asMinutes :: Duration -> Duration

    asMonths :: Duration -> Duration

    asSeconds :: Duration -> Duration

    asYears :: Duration -> Duration

    durToMDur :: Duration -> MDuration

    durToMDurImpl :: Fn2 Number String MDuration

    foldDuration :: Duration -> Number

    humanize :: Duration -> String

    humanize' :: Duration -> String

    stringDuration :: Duration -> String


## Module Data.Moment.GetSet

### Values

    dayOfMonth :: Moment -> DayOfMonth

    dayOfWeek :: Moment -> Maybe WeekDay

    dayOfWeek' :: Moment -> Maybe WeekDay

    dayOfYear :: Moment -> DayOfYear

    getZone :: Moment -> Zone

    hours :: Moment -> Duration

    milliseconds :: Moment -> Duration

    minutes :: Moment -> Duration

    month :: Moment -> Maybe Month

    quarter :: Moment -> Number

    seconds :: Moment -> Duration

    setDayOfMonth :: DayOfMonth -> Moment -> Moment

    setDayOfWeek :: WeekDay -> Moment -> Moment

    setDayOfWeek' :: WeekDay -> Moment -> Moment

    setDayOfYear :: DayOfYear -> Moment -> Moment

    setHours :: Duration -> Moment -> Moment

    setMilliseconds :: Duration -> Moment -> Moment

    setMinutes :: Duration -> Moment -> Moment

    setMonth :: Month -> Moment -> Moment

    setQuarter :: Number -> Moment -> Moment

    setSeconds :: Duration -> Moment -> Moment

    setWeekOfYear :: WeekOfYear -> Moment -> Moment

    setYear :: Number -> Moment -> Moment

    setZone :: String -> Moment -> Moment

    valueOf :: Moment -> Epoch

    weekOfYear :: Moment -> WeekOfYear

    year :: Moment -> Number


## Module Data.Moment.Manipulate

### Types

    data Granularity where
      Second :: Granularity
      Minute :: Granularity
      Hour :: Granularity
      Day :: Granularity
      Week :: Granularity
      Quarter :: Granularity
      Month :: Granularity
      Year :: Granularity


### Type Class Instances

    instance showGranularity :: Show Granularity


### Values

    add :: Duration -> Moment -> Moment

    endOf :: Granularity -> Moment -> Moment

    startOf :: Granularity -> Moment -> Moment

    subtract :: Duration -> Moment -> Moment


## Module Data.Moment.Month

### Types

    data Month where
      January :: Month
      February :: Month
      March :: Month
      April :: Month
      May :: Month
      June :: Month
      July :: Month
      August :: Month
      September :: Month
      October :: Month
      November :: Month
      December :: Month


### Type Class Instances

    instance enumMonth :: Enum Month

    instance eqMonth :: Eq Month

    instance ordMonth :: Ord Month

    instance showMonth :: Show Month


## Module Data.Moment.Parse

### Types

    type MomentObj  = { milliseconds :: Duration, seconds :: Duration, minutes :: Duration, hours :: Duration, days :: Duration, months :: Duration, years :: Duration }


### Values

    parseEpoch :: Epoch -> Moment

    parseObj :: MomentObj -> Maybe Moment

    parseObjImpl :: { milliseconds :: Number, seconds :: Number, minutes :: Number, hours :: Number, days :: Number, months :: Number, years :: Number } -> Moment

    parseStringZ :: [String] -> String -> Maybe Moment

    parseString_ :: forall a e. Fn4 (Maybe Moment) (a -> Maybe Moment) [String] String (Maybe Moment)

    parseUnix :: Unix -> Moment

    unsafeToMoment :: forall a. a -> Moment


## Module Data.Moment.WeekDay

### Types

    data WeekDay where
      Sunday :: WeekDay
      Monday :: WeekDay
      Tuesday :: WeekDay
      Wednesday :: WeekDay
      Thursday :: WeekDay
      Friday :: WeekDay
      Saturday :: WeekDay


### Type Class Instances

    instance enumWeekDay :: Enum WeekDay

    instance eqWeekDay :: Eq WeekDay

    instance ordWeekDay :: Ord WeekDay

    instance showWeekDay :: Show WeekDay



