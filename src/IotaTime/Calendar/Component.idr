module IotaTime.Calendar.Component

import public Data.So

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

namespace Year
  public export
  fromInteger : Integer -> Year
  fromInteger = MkYear

public export
yearValue : Year -> Integer
yearValue (MkYear value) = value

export
yearFromInteger : Integer -> Year
yearFromInteger = MkYear

export
record DayOfMonth where
  constructor MkDayOfMonth
  integerValue : Integer

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
  public export
  fromInteger : (value : Integer) ->
                {auto 0 valid : So (isValidDayOfMonth value)} ->
                DayOfMonth
  fromInteger value = MkDayOfMonth value

public export
dayOfMonthValue : DayOfMonth -> Integer
dayOfMonthValue (MkDayOfMonth value) = value

export
dayOfMonthFromInteger : Integer -> DayOfMonth
dayOfMonthFromInteger value = MkDayOfMonth (max 1 (min 31 value))

public export
data DayOfMonthError = DayOfMonthOutOfRange Integer

public export
refineDayOfMonth : (value : Integer) -> Either DayOfMonthError DayOfMonth
refineDayOfMonth value =
  case choose (isValidDayOfMonth value) of
    Left valid => Right (DayOfMonth.fromInteger value @{valid})
    Right _ => Left (DayOfMonthOutOfRange value)

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

namespace WeekNumber
  public export
  fromInteger : Integer -> WeekNumber
  fromInteger = MkWeekNumber

public export
weekNumberValue : WeekNumber -> Integer
weekNumberValue (MkWeekNumber value) = value
