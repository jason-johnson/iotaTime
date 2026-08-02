module IotaTime.Time.Component

import public Data.So

%default total

public export
isValidHour : Integer -> Bool
isValidHour value = value >= 0 && value < 24

export
record Hour where
  constructor MkHour
  integerValue : Integer

namespace Hour
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidHour value)} -> Hour
  fromInteger value = MkHour value

public export
hourValue : Hour -> Integer
hourValue (MkHour value) = value

export
hourFromInteger : Integer -> Hour
hourFromInteger = MkHour

public export
Eq Hour where
  left == right = hourValue left == hourValue right

public export
Show Hour where
  show = show . hourValue

public export
data HourError = HourOutOfRange Integer

public export
refineHour : (value : Integer) -> Either HourError Hour
refineHour value = case choose (isValidHour value) of
  Left valid => Right (Hour.fromInteger value @{valid})
  Right _ => Left (HourOutOfRange value)

public export
isValidMinute : Integer -> Bool
isValidMinute value = value >= 0 && value < 60

export
record Minute where
  constructor MkMinute
  integerValue : Integer

namespace Minute
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidMinute value)} -> Minute
  fromInteger value = MkMinute value

public export
minuteValue : Minute -> Integer
minuteValue (MkMinute value) = value

export
minuteFromInteger : Integer -> Minute
minuteFromInteger = MkMinute

public export
Eq Minute where
  left == right = minuteValue left == minuteValue right

public export
Show Minute where
  show = show . minuteValue

public export
data MinuteError = MinuteOutOfRange Integer

public export
refineMinute : (value : Integer) -> Either MinuteError Minute
refineMinute value = case choose (isValidMinute value) of
  Left valid => Right (Minute.fromInteger value @{valid})
  Right _ => Left (MinuteOutOfRange value)

public export
isValidSecond : Integer -> Bool
isValidSecond value = value >= 0 && value < 60

export
record Second where
  constructor MkSecond
  integerValue : Integer

namespace Second
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidSecond value)} -> Second
  fromInteger value = MkSecond value

public export
secondValue : Second -> Integer
secondValue (MkSecond value) = value

export
secondFromInteger : Integer -> Second
secondFromInteger = MkSecond

public export
Eq Second where
  left == right = secondValue left == secondValue right

public export
Show Second where
  show = show . secondValue

public export
data SecondError = SecondOutOfRange Integer

public export
refineSecond : (value : Integer) -> Either SecondError Second
refineSecond value = case choose (isValidSecond value) of
  Left valid => Right (Second.fromInteger value @{valid})
  Right _ => Left (SecondOutOfRange value)

public export
isValidNanosecond : Integer -> Bool
isValidNanosecond value = value >= 0 && value < 1000000000

export
record Nanosecond where
  constructor MkNanosecond
  integerValue : Integer

namespace Nanosecond
  public export
  fromInteger : (value : Integer) ->
                {auto 0 valid : So (isValidNanosecond value)} -> Nanosecond
  fromInteger value = MkNanosecond value

public export
nanosecondValue : Nanosecond -> Integer
nanosecondValue (MkNanosecond value) = value

export
nanosecondFromInteger : Integer -> Nanosecond
nanosecondFromInteger = MkNanosecond

public export
Eq Nanosecond where
  left == right = nanosecondValue left == nanosecondValue right

public export
Show Nanosecond where
  show = show . nanosecondValue

public export
data NanosecondError = NanosecondOutOfRange Integer

public export
refineNanosecond : (value : Integer) -> Either NanosecondError Nanosecond
refineNanosecond value = case choose (isValidNanosecond value) of
  Left valid => Right (Nanosecond.fromInteger value @{valid})
  Right _ => Left (NanosecondOutOfRange value)