module IotaTime.Pattern.LocalTime

import Data.So
import Data.String.Parser
import IotaTime.Locale
import IotaTime.LocalTime
import IotaTime.Pattern

%default total

||| Intermediate fields accumulated while parsing a local time.
public export
record TimeFields where
  constructor MkTimeFields
  parsedHour : Integer
  parsedMinute : Integer
  parsedSecond : Integer
  parsedNanosecond : Integer

initialTimeFields : TimeFields
initialTimeFields = MkTimeFields 0 0 0 0

finishTime : TimeFields -> Either PatternError LocalTime
finishTime fields = case refineLocalTime
  fields.parsedHour fields.parsedMinute fields.parsedSecond fields.parsedNanosecond of
    Left _ => Left (InvalidValue "invalid local time")
    Right value => Right value

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

spaces : Nat -> String
spaces Z = ""
spaces (S count) = " " ++ spaces count

padNumber : Nat -> Integer -> String
padNumber width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
      else zeros (width `minus` currentWidth) ++ shown

spacePadNumber : Nat -> Integer -> String
spacePadNumber width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
    else spaces (width `minus` currentWidth) ++ shown

setHour : Integer -> TimeFields -> TimeFields
setHour value fields = { parsedHour := value } fields

setMinute : Integer -> TimeFields -> TimeFields
setMinute value fields = { parsedMinute := value } fields

setSecond : Integer -> TimeFields -> TimeFields
setSecond value fields = { parsedSecond := value } fields

setNanosecond : Integer -> TimeFields -> TimeFields
setNanosecond value fields = { parsedNanosecond := value } fields

timeHour : LocalTime -> Integer
timeHour = hourValue . hour

timeMinute : LocalTime -> Integer
timeMinute = minuteValue . minute

timeSecond : LocalTime -> Integer
timeSecond = secondValue . second

timeNanosecond : LocalTime -> Integer
timeNanosecond = nanosecondValue . nanosecond

timeField : (LocalTime -> Integer) -> (Integer -> TimeFields -> TimeFields) ->
            (width : Nat) -> (maximumWidth : Nat) ->
            (minimum : Integer) -> (maximum : Integer) ->
            Pattern TimeFields LocalTime
timeField getter setter width maximumWidth minimum maximum = MkPattern
  initialTimeFields
  finishTime
  (numberUpdatePart setter width maximumWidth minimum maximum)
  (padNumber width . getter)

||| A 24-hour field with the requested output width and up to two input digits.
public export
phour : Nat -> Pattern TimeFields LocalTime
phour width = timeField timeHour setHour width 2 0 23

||| A two-digit 24-hour field in the range 00 through 23.
public export
pHH : Pattern TimeFields LocalTime
pHH = phour 2

setTwelveHour : Integer -> TimeFields -> TimeFields
setTwelveHour value fields =
  { parsedHour := 12 * (fields.parsedHour `div` 12) + value `mod` 12 } fields

formatTwelveHour : LocalTime -> Integer
formatTwelveHour value =
  let folded = timeHour value `mod` 12
   in if folded == 0 then 12 else folded

||| A two-digit 12-hour clock field in the range 01 through 12.
public export
phh : Pattern TimeFields LocalTime
phh = MkPattern
  initialTimeFields
  finishTime
  (numberUpdatePart setTwelveHour 2 2 1 12)
  (padNumber 2 . formatTwelveHour)

||| A space-padded two-character 12-hour clock field.
public export
phhSpace : Pattern TimeFields LocalTime
phhSpace = MkPattern
  initialTimeFields
  finishTime
  (spaceNumberUpdatePart setTwelveHour 2 1 12)
  (spacePadNumber 2 . formatTwelveHour)

||| A minute field with the requested output width and up to two input digits.
public export
pminute : Nat -> Pattern TimeFields LocalTime
pminute width = timeField timeMinute setMinute width 2 0 59

||| A two-digit minute field in the range 00 through 59.
public export
pmm : Pattern TimeFields LocalTime
pmm = pminute 2

||| A second field with the requested output width and up to two input digits.
public export
psecond : Nat -> Pattern TimeFields LocalTime
psecond width = timeField timeSecond setSecond width 2 0 59

||| A two-digit second field in the range 00 through 59.
public export
pss : Pattern TimeFields LocalTime
pss = psecond 2

pow10 : Nat -> Integer
pow10 Z = 1
pow10 (S exponent) = 10 * pow10 exponent

||| Whether a fractional-second width is representable at nanosecond precision.
public export
isValidFractionWidth : Nat -> Bool
isValidFractionWidth width = width >= 1 && width <= 9

||| A fixed-width fractional-second field with one through nine digits.
|||
||| The erased proof rejects unsupported widths at compile time.
public export
pfrac : (width : Nat) -> {auto 0 valid : So (isValidFractionWidth width)} ->
        Pattern TimeFields LocalTime
pfrac width =
  let scale = pow10 (9 `minus` width)
      maximum = pow10 width - 1
   in MkPattern
        initialTimeFields
        finishTime
        (numberUpdatePart (\value => setNanosecond (value * scale))
          width width 0 maximum)
        (padNumber width . (`div` scale) . timeNanosecond)

setPeriod : Bool -> TimeFields -> TimeFields
setPeriod isPm fields =
  { parsedHour := fields.parsedHour `mod` 12 + if isPm then 12 else 0 } fields

periodPattern : String -> String -> Pattern TimeFields LocalTime
periodPattern am pm = MkPattern
  initialTimeFields
  finishTime
  (namedUpdatePart [(pm, True), (am, False)] setPeriod)
  (\value => if timeHour value >= 12 then pm else am)

||| A 12-hour period field using the supplied AM and PM labels.
public export
pPeriod : (String, String) -> Pattern TimeFields LocalTime
pPeriod (am, pm) = periodPattern am pm

||| The single-letter `A` or `P` period field.
public export
pp : Pattern TimeFields LocalTime
pp = periodPattern "A" "P"

||| The English `AM` or `PM` period field.
public export
ppp : Pattern TimeFields LocalTime
ppp = periodPattern "AM" "PM"

||| A period field using a locale's AM and PM labels.
public export
ppp' : Locale -> Pattern TimeFields LocalTime
ppp' locale = periodPattern (amName locale) (pmName locale)

||| The short `HH:mm` local-time pattern.
public export
pt : Pattern TimeFields LocalTime
pt = (pHH <% char ':') <+> pmm

||| The long `HH:mm:ss` local-time pattern.
public export
pT : Pattern TimeFields LocalTime
pT = ((pHH <% char ':') <+> (pmm <% char ':')) <+> pss

||| The round-trip `HH:mm:ss.fffffffff` local-time pattern.
public export
pr : Pattern TimeFields LocalTime
pr = (pT <% char '.') <+> pfrac 9
