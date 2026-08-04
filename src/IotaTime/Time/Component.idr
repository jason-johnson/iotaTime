module IotaTime.Time.Component

import public Data.So

%default total

||| Whether an integer is in the inclusive hour range 0 through 23.
public export
isValidHour : Integer -> Bool
isValidHour value = value >= 0 && value < 24

||| An hour of day constrained to the inclusive range 0 through 23.
export
record Hour where
  constructor MkHour
  integerValue : Integer

namespace Hour
  ||| Construct an hour when its range proof is available statically.
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidHour value)} -> Hour
  fromInteger value = MkHour value

||| Return the integer hour of day.
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

||| A runtime hour value outside the inclusive range 0 through 23.
public export
data HourError = HourOutOfRange Integer

||| Refine an untrusted integer into an hour or return a typed range error.
public export
refineHour : (value : Integer) -> Either HourError Hour
refineHour value = case choose (isValidHour value) of
  Left valid => Right (Hour.fromInteger value @{valid})
  Right _ => Left (HourOutOfRange value)

||| Whether an integer is in the inclusive minute range 0 through 59.
public export
isValidMinute : Integer -> Bool
isValidMinute value = value >= 0 && value < 60

||| A minute of hour constrained to the inclusive range 0 through 59.
export
record Minute where
  constructor MkMinute
  integerValue : Integer

namespace Minute
  ||| Construct a minute when its range proof is available statically.
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidMinute value)} -> Minute
  fromInteger value = MkMinute value

||| Return the integer minute of hour.
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

||| A runtime minute value outside the inclusive range 0 through 59.
public export
data MinuteError = MinuteOutOfRange Integer

||| Refine an untrusted integer into a minute or return a typed range error.
public export
refineMinute : (value : Integer) -> Either MinuteError Minute
refineMinute value = case choose (isValidMinute value) of
  Left valid => Right (Minute.fromInteger value @{valid})
  Right _ => Left (MinuteOutOfRange value)

||| Whether an integer is in the inclusive second range 0 through 59.
public export
isValidSecond : Integer -> Bool
isValidSecond value = value >= 0 && value < 60

||| A second of minute constrained to the inclusive range 0 through 59.
export
record Second where
  constructor MkSecond
  integerValue : Integer

namespace Second
  ||| Construct a second when its range proof is available statically.
  public export
  fromInteger : (value : Integer) -> {auto 0 valid : So (isValidSecond value)} -> Second
  fromInteger value = MkSecond value

||| Return the integer second of minute.
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

||| A runtime second value outside the inclusive range 0 through 59.
public export
data SecondError = SecondOutOfRange Integer

||| Refine an untrusted integer into a second or return a typed range error.
public export
refineSecond : (value : Integer) -> Either SecondError Second
refineSecond value = case choose (isValidSecond value) of
  Left valid => Right (Second.fromInteger value @{valid})
  Right _ => Left (SecondOutOfRange value)

||| Whether an integer is in the nanosecond range 0 through 999,999,999.
public export
isValidNanosecond : Integer -> Bool
isValidNanosecond value = value >= 0 && value < 1000000000

||| A nanosecond of second constrained to 0 through 999,999,999.
export
record Nanosecond where
  constructor MkNanosecond
  integerValue : Integer

namespace Nanosecond
  ||| Construct a nanosecond when its range proof is available statically.
  public export
  fromInteger : (value : Integer) ->
                {auto 0 valid : So (isValidNanosecond value)} -> Nanosecond
  fromInteger value = MkNanosecond value

||| Return the integer nanosecond of second.
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

||| A runtime nanosecond value outside 0 through 999,999,999.
public export
data NanosecondError = NanosecondOutOfRange Integer

||| Refine an untrusted integer into a nanosecond or return a typed range error.
public export
refineNanosecond : (value : Integer) -> Either NanosecondError Nanosecond
refineNanosecond value = case choose (isValidNanosecond value) of
  Left valid => Right (Nanosecond.fromInteger value @{valid})
  Right _ => Left (NanosecondOutOfRange value)