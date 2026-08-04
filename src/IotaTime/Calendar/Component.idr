module IotaTime.Calendar.Component

import public Data.So

||| A calendar year with an unbounded signed integer value.
export
record Year where
  constructor MkYear
  integerValue : Integer

public export
Eq Year where
  left == right = left.integerValue == right.integerValue

public export
Ord Year where
  compare left right = compare left.integerValue right.integerValue

public export
Show Year where
  show value = show value.integerValue

public export
Num Year where
  MkYear left + MkYear right = MkYear (left + right)
  MkYear left * MkYear right = MkYear (left * right)
  fromInteger = MkYear

public export
Neg Year where
  negate (MkYear value) = MkYear (negate value)
  MkYear left - MkYear right = MkYear (left - right)

namespace Year
  ||| Construct a year from any integer.
  public export
  fromInteger : Integer -> Year
  fromInteger = MkYear

||| Return the signed integer represented by a year.
public export
yearValue : Year -> Integer
yearValue (MkYear value) = value

export
yearFromInteger : Integer -> Year
yearFromInteger = MkYear

||| A day number constrained to the inclusive range 1 through 31.
export
record DayOfMonth where
  constructor MkDayOfMonth
  integerValue : Integer

||| Whether an integer is in the representable day-of-month range.
public export
isValidDayOfMonth : Integer -> Bool
isValidDayOfMonth value = value >= 1 && value <= 31

public export
Eq DayOfMonth where
  left == right = left.integerValue == right.integerValue

public export
Ord DayOfMonth where
  compare left right = compare left.integerValue right.integerValue

public export
Show DayOfMonth where
  show value = show value.integerValue

namespace DayOfMonth
  ||| Construct a day of month when its range proof is available statically.
  public export
  fromInteger : (value : Integer) ->
                {auto 0 valid : So (isValidDayOfMonth value)} ->
                DayOfMonth
  fromInteger value = MkDayOfMonth value

||| Return the integer day number.
public export
dayOfMonthValue : DayOfMonth -> Integer
dayOfMonthValue (MkDayOfMonth value) = value

export
dayOfMonthFromInteger : Integer -> DayOfMonth
dayOfMonthFromInteger value = MkDayOfMonth (max 1 (min 31 value))

||| A runtime day-of-month value outside the inclusive range 1 through 31.
public export
data DayOfMonthError = DayOfMonthOutOfRange Integer

||| Refine an untrusted integer into a day of month or return a typed range error.
public export
refineDayOfMonth : (value : Integer) -> Either DayOfMonthError DayOfMonth
refineDayOfMonth value =
  case choose (isValidDayOfMonth value) of
    Left valid => Right (DayOfMonth.fromInteger value @{valid})
    Right _ => Left (DayOfMonthOutOfRange value)

||| An unbounded signed week number used by calendar week calculations.
export
record WeekNumber where
  constructor MkWeekNumber
  integerValue : Integer

public export
Eq WeekNumber where
  left == right = left.integerValue == right.integerValue

public export
Ord WeekNumber where
  compare left right = compare left.integerValue right.integerValue

public export
Show WeekNumber where
  show value = show value.integerValue

public export
Num WeekNumber where
  MkWeekNumber left + MkWeekNumber right = MkWeekNumber (left + right)
  MkWeekNumber left * MkWeekNumber right = MkWeekNumber (left * right)
  fromInteger = MkWeekNumber

public export
Neg WeekNumber where
  negate (MkWeekNumber value) = MkWeekNumber (negate value)
  MkWeekNumber left - MkWeekNumber right = MkWeekNumber (left - right)

namespace WeekNumber
  ||| Construct a week number from any integer.
  public export
  fromInteger : Integer -> WeekNumber
  fromInteger = MkWeekNumber

||| Return the signed integer represented by a week number.
public export
weekNumberValue : WeekNumber -> Integer
weekNumberValue (MkWeekNumber value) = value
