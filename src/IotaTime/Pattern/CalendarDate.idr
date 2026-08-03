module IotaTime.Pattern.CalendarDate

import Data.String.Parser
import IotaTime.Pattern
import IotaTime.Calendar
import IotaTime.Calendar.Gregorian

%default total

public export
record DateFields where
  constructor MkDateFields
  parsedYear : Integer
  parsedMonth : Integer
  parsedDay : Integer

initialDateFields : DateFields
initialDateFields = MkDateFields 2000 3 1

monthFromInteger : Integer -> Month
monthFromInteger 1 = January
monthFromInteger 2 = February
monthFromInteger 3 = March
monthFromInteger 4 = April
monthFromInteger 5 = May
monthFromInteger 6 = June
monthFromInteger 7 = July
monthFromInteger 8 = August
monthFromInteger 9 = September
monthFromInteger 10 = October
monthFromInteger 11 = November
monthFromInteger _ = December

finishDate : DateFields -> Either PatternError (CalendarDate Gregorian)
finishDate fields = case refineDayOfMonth fields.parsedDay of
  Left _ => Left (InvalidValue "day is outside 1-31")
  Right valueDay => case refineGregorianDate valueDay
    (monthFromInteger fields.parsedMonth)
    (yearFromInteger fields.parsedYear) of
      Left _ => Left (InvalidValue "invalid Gregorian date")
      Right date => Right date

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

padNumber : Nat -> Integer -> String
padNumber width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
      else zeros (width `minus` currentWidth) ++ shown

dateField : (CalendarDate Gregorian -> Integer) ->
            (Integer -> DateFields -> DateFields) ->
            (width : Nat) -> (maximumWidth : Nat) ->
            (minimum : Integer) -> (maximum : Integer) ->
            Pattern DateFields (CalendarDate Gregorian)
dateField getter setter width maximumWidth minimum maximum = MkPattern
  initialDateFields
  finishDate
  (numberUpdatePart setter width maximumWidth minimum maximum)
  (padNumber width . getter)

setYearField : Integer -> DateFields -> DateFields
setYearField value fields = { parsedYear := value } fields

setMonthField : Integer -> DateFields -> DateFields
setMonthField value fields = { parsedMonth := value } fields

setDayField : Integer -> DateFields -> DateFields
setDayField value fields = { parsedDay := value } fields

calendarYear : CalendarDate Gregorian -> Integer
calendarYear date = yearValue (year {calendar = Gregorian} date)

calendarMonth : CalendarDate Gregorian -> Integer
calendarMonth date = monthNumber (month {calendar = Gregorian} date)

calendarDay : CalendarDate Gregorian -> Integer
calendarDay date = dayOfMonthValue (day {calendar = Gregorian} date)

public export
pyear : Nat -> Pattern DateFields (CalendarDate Gregorian)
pyear width = dateField calendarYear setYearField width 4 0 9999

public export
pyyyy : Pattern DateFields (CalendarDate Gregorian)
pyyyy = pyear 4

inferTwoDigitYear : Integer -> Integer -> Integer
inferTwoDigitYear template value =
  let base = (template `div` 100) * 100 + value
      adjustment = (template - base + 50) `div` 100
   in base + adjustment * 100

public export
pyy : Pattern DateFields (CalendarDate Gregorian)
pyy = MkPattern
  initialDateFields
  finishDate
  (numberUpdatePart
    (\value, fields =>
      { parsedYear := inferTwoDigitYear fields.parsedYear value } fields)
    2 2 0 99)
  (padNumber 2 . (`mod` 100) . calendarYear)

public export
pmonthNum : Nat -> Pattern DateFields (CalendarDate Gregorian)
pmonthNum width = dateField calendarMonth setMonthField width 2 1 12

public export
pMM : Pattern DateFields (CalendarDate Gregorian)
pMM = pmonthNum 2

public export
pday : Nat -> Pattern DateFields (CalendarDate Gregorian)
pday width = dateField calendarDay setDayField width 2 1 31

public export
pdd : Pattern DateFields (CalendarDate Gregorian)
pdd = pday 2

public export
pd : Pattern DateFields (CalendarDate Gregorian)
pd = ((pdd <% char '/') <+> (pMM <% char '/')) <+> pyyyy

public export
pR : Pattern DateFields (CalendarDate Gregorian)
pR = ((pyyyy <% char '-') <+> (pMM <% char '-')) <+> pdd
