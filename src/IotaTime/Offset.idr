module IotaTime.Offset

import public Data.So

%default total

secondsPerMinute : Integer
secondsPerMinute = 60

secondsPerHour : Integer
secondsPerHour = 3600

maxOffsetHours : Integer
maxOffsetHours = 18

maxOffsetSeconds : Integer
maxOffsetSeconds = maxOffsetHours * secondsPerHour

||| A signed whole-second displacement from UTC, bounded to plus or minus
||| eighteen hours.
export
record OffsetRep where
  constructor MkOffset
  storedSeconds : Integer

public export
Offset : Type
Offset = OffsetRep

public export
isValidOffsetSeconds : Integer -> Bool
isValidOffsetSeconds value =
  value >= -64800 && value <= 64800

public export
isValidOffsetMinutes : Integer -> Bool
isValidOffsetMinutes value =
  value >= -1080 && value <= 1080

public export
isValidOffsetHours : Integer -> Bool
isValidOffsetHours value =
  value >= -18 && value <= 18

public export
fromSeconds : (value : Integer) ->
              {auto 0 valid : So (isValidOffsetSeconds value)} -> Offset
fromSeconds value = MkOffset value

public export
fromMinutes : (value : Integer) ->
              {auto 0 valid : So (isValidOffsetMinutes value)} -> Offset
fromMinutes value = MkOffset (value * secondsPerMinute)

public export
fromHours : (value : Integer) ->
            {auto 0 valid : So (isValidOffsetHours value)} -> Offset
fromHours value = MkOffset (value * secondsPerHour)

public export
data OffsetError = OffsetOutOfRange Integer

public export
refineOffsetSeconds : (value : Integer) -> Either OffsetError Offset
refineOffsetSeconds value =
  case choose (isValidOffsetSeconds value) of
    Left valid => Right (fromSeconds value @{valid})
    Right _ => Left (OffsetOutOfRange value)

||| Internal exact-value observation used by sibling implementation modules.
||| Public callers should normally use `hours`, `minutes`, and `seconds`.
export
totalOffsetSeconds : Offset -> Integer
totalOffsetSeconds (MkOffset value) = value

componentSign : Integer -> Integer
componentSign value = if value < 0 then -1 else 1

||| The signed whole-hour component of an offset.
public export
hours : Offset -> Integer
hours value =
  componentSign seconds * (abs seconds `div` secondsPerHour)
  where
    seconds = totalOffsetSeconds value

||| The signed minute-within-hour component of an offset.
public export
minutes : Offset -> Integer
minutes value =
  componentSign seconds *
    ((abs seconds `mod` secondsPerHour) `div` secondsPerMinute)
  where
    seconds = totalOffsetSeconds value

||| The signed second-within-minute component of an offset.
public export
seconds : Offset -> Integer
seconds value =
  componentSign seconds * (abs seconds `mod` secondsPerMinute)
  where
    seconds = totalOffsetSeconds value

public export
empty : Offset
empty = MkOffset 0

clampOffsetSeconds : Integer -> Integer
clampOffsetSeconds value = max (-64800) (min 64800 value)

||| Add two offsets, clamping the result to the supported bounds.
public export
addClamped : Offset -> Offset -> Offset
addClamped left right = MkOffset (clampOffsetSeconds
  (totalOffsetSeconds left + totalOffsetSeconds right))

||| Subtract the second offset, clamping the result to the supported bounds.
public export
minusClamped : Offset -> Offset -> Offset
minusClamped left right = MkOffset (clampOffsetSeconds
  (totalOffsetSeconds left - totalOffsetSeconds right))

||| Reverse an offset's direction.
public export
negateOffset : Offset -> Offset
negateOffset value = MkOffset (negate (totalOffsetSeconds value))

public export
Eq OffsetRep where
  left == right = totalOffsetSeconds left == totalOffsetSeconds right

public export
Ord OffsetRep where
  compare left right = compare
    (totalOffsetSeconds left) (totalOffsetSeconds right)

public export
Show OffsetRep where
  show value = "fromSeconds " ++ show (totalOffsetSeconds value)