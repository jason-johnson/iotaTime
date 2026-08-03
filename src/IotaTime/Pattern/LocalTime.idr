module IotaTime.Pattern.LocalTime

import Data.So
import Data.String.Parser
import IotaTime.Locale
import IotaTime.LocalTime
import IotaTime.Pattern

%default total

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

public export
phour : Nat -> Pattern TimeFields LocalTime
phour width = timeField timeHour setHour width 2 0 23

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

public export
phh : Pattern TimeFields LocalTime
phh = MkPattern
  initialTimeFields
  finishTime
  (numberUpdatePart setTwelveHour 2 2 1 12)
  (padNumber 2 . formatTwelveHour)

public export
phhSpace : Pattern TimeFields LocalTime
phhSpace = MkPattern
  initialTimeFields
  finishTime
  (spaceNumberUpdatePart setTwelveHour 2 1 12)
  (spacePadNumber 2 . formatTwelveHour)

public export
pminute : Nat -> Pattern TimeFields LocalTime
pminute width = timeField timeMinute setMinute width 2 0 59

public export
pmm : Pattern TimeFields LocalTime
pmm = pminute 2

public export
psecond : Nat -> Pattern TimeFields LocalTime
psecond width = timeField timeSecond setSecond width 2 0 59

public export
pss : Pattern TimeFields LocalTime
pss = psecond 2

pow10 : Nat -> Integer
pow10 Z = 1
pow10 (S exponent) = 10 * pow10 exponent

public export
isValidFractionWidth : Nat -> Bool
isValidFractionWidth width = width >= 1 && width <= 9

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

public export
pPeriod : (String, String) -> Pattern TimeFields LocalTime
pPeriod (am, pm) = periodPattern am pm

public export
pp : Pattern TimeFields LocalTime
pp = periodPattern "A" "P"

public export
ppp : Pattern TimeFields LocalTime
ppp = periodPattern "AM" "PM"

public export
ppp' : Locale -> Pattern TimeFields LocalTime
ppp' locale = periodPattern (amName locale) (pmName locale)

public export
pt : Pattern TimeFields LocalTime
pt = (pHH <% char ':') <+> pmm

public export
pT : Pattern TimeFields LocalTime
pT = ((pHH <% char ':') <+> (pmm <% char ':')) <+> pss

public export
pr : Pattern TimeFields LocalTime
pr = (pT <% char '.') <+> pfrac 9
