module IotaTime.Pattern.Duration

import Data.String
import Data.String.Parser
import IotaTime.Duration
import IotaTime.Pattern

%default total

isDecimalDigit : Char -> Bool
isDecimalDigit value = value >= '0' && value <= '9'

digitValue : Char -> Integer
digitValue value = cast value - cast '0'

readDayDigits : Integer -> List Char ->
                Maybe (Integer, Nat, List Char)
readDayDigits value (digit :: rest) = if isDecimalDigit digit
  then readMore (value * 10 + digitValue digit) 1 rest
  else Nothing
  where
    readMore : Integer -> Nat -> List Char ->
               Maybe (Integer, Nat, List Char)
    readMore value count (digit :: rest) = if isDecimalDigit digit
      then readMore (value * 10 + digitValue digit) (S count) rest
      else Just (value, count, digit :: rest)
    readMore value count [] = Just (value, count, [])
readDayDigits value [] = Nothing

readTwoDigits : List Char -> Maybe (Integer, List Char)
readTwoDigits (tens :: units :: rest) =
  if isDecimalDigit tens && isDecimalDigit units
    then Just (digitValue tens * 10 + digitValue units, rest)
    else Nothing
readTwoDigits _ = Nothing

readNineDigits : List Char -> Maybe (Integer, List Char)
readNineDigits values = go 9 0 values
  where
    go : Nat -> Integer -> List Char -> Maybe (Integer, List Char)
    go Z value rest = Just (value, rest)
    go (S count) value (digit :: rest) = if isDecimalDigit digit
      then go count (value * 10 + digitValue digit) rest
      else Nothing
    go (S count) value [] = Nothing

record DurationParts where
  constructor MkDurationParts
  consumed : Nat
  negative : Bool
  days : Integer
  hours : Integer
  minutes : Integer
  seconds : Integer
  nanoseconds : Integer

splitSign : List Char -> (Bool, Nat, List Char)
splitSign ('-' :: rest) = (True, 1, rest)
splitSign values = (False, 0, values)

parseDurationParts : Bool -> List Char -> Maybe DurationParts
parseDurationParts withFraction values =
  let (negative, signWidth, unsigned) = splitSign values in
  do
    (days, dayWidth, ':' :: afterDays) <- readDayDigits 0 unsigned
      | _ => Nothing
    (hours, ':' :: afterHours) <- readTwoDigits afterDays
      | _ => Nothing
    (minutes, ':' :: afterMinutes) <- readTwoDigits afterHours
      | _ => Nothing
    (seconds, afterSeconds) <- readTwoDigits afterMinutes
    if minutes > 59 || seconds > 59
      then Nothing
      else if withFraction
        then case afterSeconds of
          '.' :: fraction => do
            (nanoseconds, rest) <- readNineDigits fraction
            let used = signWidth + dayWidth + 1 + 2 + 1 + 2 + 1 + 2 + 1 + 9
            Just (MkDurationParts used negative days hours minutes seconds
              nanoseconds)
          _ => Nothing
        else
          let used = signWidth + dayWidth + 1 + 2 + 1 + 2 + 1 + 2 in
          Just (MkDurationParts used negative days hours minutes seconds 0)

partsDuration : DurationParts -> Duration
partsDuration parts =
  let magnitude =
        ((((parts.days * 24 + parts.hours) * 60 + parts.minutes) * 60 +
          parts.seconds) * 1000000000) + parts.nanoseconds
   in fromNanoseconds (if parts.negative then negate magnitude else magnitude)

durationParser : Bool -> PatternParser (Either PatternError (Duration -> Duration))
durationParser withFraction = Parser.P (\state =>
  let remaining = strSubstr state.pos (state.maxPos - state.pos) state.input in
  case parseDurationParts withFraction (unpack remaining) of
    Nothing => pure (Parser.Fail state.pos "duration")
    Just parts => pure (Parser.OK (Right (const (partsDuration parts)))
      ({ pos := state.pos + cast parts.consumed } state)))

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

pad : Nat -> Integer -> String
pad width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
      else zeros (width `minus` currentWidth) ++ shown

renderDuration : Bool -> Duration -> String
renderDuration withFraction value =
  let totalNanoseconds = toDurationNanoseconds value
      magnitude = abs totalNanoseconds
      totalSeconds = magnitude `div` 1000000000
      nanoseconds = magnitude `mod` 1000000000
      seconds = totalSeconds `mod` 60
      totalMinutes = totalSeconds `div` 60
      minutes = totalMinutes `mod` 60
      totalHours = totalMinutes `div` 60
      hours = totalHours `mod` 24
      days = totalHours `div` 24
      fraction = if withFraction then "." ++ pad 9 nanoseconds else ""
   in (if totalNanoseconds < 0 then "-" else "") ++ show days ++ ":" ++
      pad 2 hours ++ ":" ++ pad 2 minutes ++ ":" ++ pad 2 seconds ++ fraction

durationPattern : Bool -> Pattern Duration Duration
durationPattern withFraction = MkPattern
  zeroDuration
  Right
  (durationParser withFraction)
  (renderDuration withFraction)

||| A signed duration rendered as [-]D:HH:mm:ss.
public export
pDuration : Pattern Duration Duration
pDuration = durationPattern False

||| A signed duration rendered as [-]D:HH:mm:ss.fffffffff.
public export
pDurationNano : Pattern Duration Duration
pDurationNano = durationPattern True