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
offsetFromSeconds : (value : Integer) ->
                    {auto 0 valid : So (isValidOffsetSeconds value)} -> Offset
offsetFromSeconds value = MkOffset value

public export
fromSeconds : (value : Integer) ->
              {auto 0 valid : So (isValidOffsetSeconds value)} -> Offset
fromSeconds = offsetFromSeconds

public export
offsetFromMinutes : (value : Integer) ->
                    {auto 0 valid : So (isValidOffsetMinutes value)} -> Offset
offsetFromMinutes value = MkOffset (value * secondsPerMinute)

public export
fromMinutes : (value : Integer) ->
              {auto 0 valid : So (isValidOffsetMinutes value)} -> Offset
fromMinutes = offsetFromMinutes

public export
offsetFromHours : (value : Integer) ->
                  {auto 0 valid : So (isValidOffsetHours value)} -> Offset
offsetFromHours value = MkOffset (value * secondsPerHour)

public export
fromHours : (value : Integer) ->
            {auto 0 valid : So (isValidOffsetHours value)} -> Offset
fromHours = offsetFromHours

public export
data OffsetError = OffsetOutOfRange Integer

public export
refineOffsetSeconds : (value : Integer) -> Either OffsetError Offset
refineOffsetSeconds value =
  case choose (isValidOffsetSeconds value) of
    Left valid => Right (offsetFromSeconds value @{valid})
    Right _ => Left (OffsetOutOfRange value)

public export
totalOffsetSeconds : Offset -> Integer
totalOffsetSeconds (MkOffset value) = value

componentSign : Integer -> Integer
componentSign value = if value < 0 then -1 else 1

||| The signed whole-hour component of an offset.
public export
offsetHours : Offset -> Integer
offsetHours value =
  componentSign seconds * (abs seconds `div` secondsPerHour)
  where
    seconds = totalOffsetSeconds value

public export
hours : Offset -> Integer
hours = offsetHours

||| The signed minute-within-hour component of an offset.
public export
offsetMinutes : Offset -> Integer
offsetMinutes value =
  componentSign seconds *
    ((abs seconds `mod` secondsPerHour) `div` secondsPerMinute)
  where
    seconds = totalOffsetSeconds value

public export
minutes : Offset -> Integer
minutes = offsetMinutes

||| The signed second-within-minute component of an offset.
public export
offsetSeconds : Offset -> Integer
offsetSeconds value =
  componentSign seconds * (abs seconds `mod` secondsPerMinute)
  where
    seconds = totalOffsetSeconds value

public export
seconds : Offset -> Integer
seconds = offsetSeconds

public export
zeroOffset : Offset
zeroOffset = MkOffset 0

public export
empty : Offset
empty = zeroOffset

clampOffsetSeconds : Integer -> Integer
clampOffsetSeconds value = max (-64800) (min 64800 value)

||| Add two offsets, clamping the result to the supported bounds.
public export
addOffsetClamped : Offset -> Offset -> Offset
addOffsetClamped left right = MkOffset (clampOffsetSeconds
  (totalOffsetSeconds left + totalOffsetSeconds right))

public export
addClamped : Offset -> Offset -> Offset
addClamped = addOffsetClamped

||| Subtract the second offset, clamping the result to the supported bounds.
public export
subtractOffsetClamped : Offset -> Offset -> Offset
subtractOffsetClamped left right = MkOffset (clampOffsetSeconds
  (totalOffsetSeconds left - totalOffsetSeconds right))

public export
minusClamped : Offset -> Offset -> Offset
minusClamped = subtractOffsetClamped

||| Reverse an offset's direction. Both bounds are symmetric, so no clamping
||| is needed.
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
  show value = "offsetFromSeconds " ++ show (totalOffsetSeconds value)