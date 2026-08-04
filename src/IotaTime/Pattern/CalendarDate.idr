module IotaTime.Pattern.CalendarDate

import Data.String.Parser
import Data.Vect
import IotaTime.Pattern
import IotaTime.Locale
import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.Pattern.Calendar

%default total

||| Intermediate fields accumulated while parsing a calendar date.
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

finishDate : {calendar : Type} ->
             {auto patterned : CalendarPattern calendar} ->
             DateFields -> Either PatternError (CalendarDate calendar)
finishDate {calendar} @{patterned} fields =
  refinePatternDate {calendar} @{patterned} fields.parsedYear fields.parsedMonth
  fields.parsedDay

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

padNumber : Nat -> Integer -> String
padNumber width value =
  let shown = show value
      currentWidth = length (unpack shown)
   in if currentWidth >= width then shown
      else zeros (width `minus` currentWidth) ++ shown

dateField : {calendar : Type} ->
            {auto patterned : CalendarPattern calendar} ->
            (CalendarDate calendar -> Integer) ->
            (Integer -> DateFields -> DateFields) ->
            (width : Nat) -> (maximumWidth : Nat) ->
            (minimum : Integer) -> (maximum : Integer) ->
            Pattern DateFields (CalendarDate calendar)
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

setMonth : Month -> DateFields -> DateFields
setMonth value fields = { parsedMonth := monthNumber value } fields

calendarYear : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
               CalendarDate calendar -> Integer
calendarYear date = yearValue (year {calendar} date)

calendarMonth : {calendar : Type} ->
                {auto patterned : CalendarPattern calendar} ->
                CalendarDate calendar -> Integer
calendarMonth {calendar} @{patterned} =
  patternMonthNumber {calendar} @{patterned}

calendarDay : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
              CalendarDate calendar -> Integer
calendarDay date = dayOfMonthValue (day {calendar} date)

calendarWeekday : CalendarDate Gregorian -> DayOfWeek
calendarWeekday = dayOfWeek {calendar = Gregorian}

gregorianMonths : Vect 12 Month
gregorianMonths =
  [ January, February, March, April, May, June
  , July, August, September, October, November, December
  ]

gregorianWeekdays : Vect 7 DayOfWeek
gregorianWeekdays =
  [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday ]

monthIndex : Month -> Fin 12
monthIndex January = 0
monthIndex February = 1
monthIndex March = 2
monthIndex April = 3
monthIndex May = 4
monthIndex June = 5
monthIndex July = 6
monthIndex August = 7
monthIndex September = 8
monthIndex October = 9
monthIndex November = 10
monthIndex December = 11

weekdayIndex : DayOfWeek -> Fin 7
weekdayIndex Sunday = 0
weekdayIndex Monday = 1
weekdayIndex Tuesday = 2
weekdayIndex Wednesday = 3
weekdayIndex Thursday = 4
weekdayIndex Friday = 5
weekdayIndex Saturday = 6

nameChoices : Vect size String -> Vect size field -> List (String, field)
nameChoices [] [] = []
nameChoices (name :: names) (value :: values) =
  (name, value) :: nameChoices names values

abbreviate : String -> String
abbreviate = substr 0 3

indexedNames : Integer -> List String -> List (String, Integer)
indexedNames _ [] = []
indexedNames index (name :: names) =
  (name, index) :: indexedNames (index + 1) names

nameAt : Integer -> List String -> String
nameAt _ [] = ""
nameAt index (name :: names) = if index <= 1
  then name
  else nameAt (index - 1) names

calendarMonthNamePattern : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  List String -> Pattern DateFields (CalendarDate calendar)
calendarMonthNamePattern {calendar} @{patterned} names = MkPattern
  initialDateFields
  finishDate
  (namedUpdatePart (indexedNames 1 names) setMonthField)
  (\date => nameAt (calendarMonth {calendar} @{patterned} date) names)

weekdayNames : List String
weekdayNames =
  [ "Sunday", "Monday", "Tuesday", "Wednesday"
  , "Thursday", "Friday", "Saturday"
  ]

weekdayAbbreviations : List String
weekdayAbbreviations =
  [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]

calendarDayNamePattern : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  List String -> Pattern DateFields (CalendarDate calendar)
calendarDayNamePattern {calendar} @{patterned} names = MkPattern
  initialDateFields
  finishDate
  (namedConsumePart names)
  (\date => nameAt
    (patternWeekdayNumber {calendar} @{patterned} date + 1) names)

englishMonthNames : Vect 12 String
englishMonthNames = map show gregorianMonths

englishMonthAbbreviations : Vect 12 String
englishMonthAbbreviations = map abbreviate englishMonthNames

englishWeekdayNames : Vect 7 String
englishWeekdayNames = map show gregorianWeekdays

englishWeekdayAbbreviations : Vect 7 String
englishWeekdayAbbreviations = map abbreviate englishWeekdayNames

||| A numeric year field with the requested output width and up to four input digits.
public export
pyear : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Nat -> Pattern DateFields (CalendarDate calendar)
pyear width = dateField calendarYear setYearField width 4 0 9999

||| A four-digit numeric year field.
public export
pyyyy : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pyyyy = pyear 4

inferTwoDigitYear : Integer -> Integer -> Integer
inferTwoDigitYear template value =
  let base = (template `div` 100) * 100 + value
      adjustment = (template - base + 50) `div` 100
   in base + adjustment * 100

||| A two-digit year field resolved to the century nearest the initial year.
public export
pyy : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pyy = MkPattern
  initialDateFields
  finishDate
  (numberUpdatePart
    (\value, fields =>
      { parsedYear := inferTwoDigitYear fields.parsedYear value } fields)
    2 2 0 99)
  (padNumber 2 . (`mod` 100) . calendarYear)

||| A numeric month field bounded by the selected calendar's month count.
public export
pmonthNum : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
    Nat -> Pattern DateFields (CalendarDate calendar)
pmonthNum {calendar} @{patterned} width =
  dateField (calendarMonth {calendar} @{patterned}) setMonthField
    width 2 1 (patternMonthLimit {calendar} @{patterned})

||| A two-digit numeric month field.
public export
pMM : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pMM = pmonthNum 2

||| A Gregorian month field using the supplied twelve full or abbreviated names.
public export
pMonthName : Vect 12 String -> Pattern DateFields (CalendarDate Gregorian)
pMonthName names = MkPattern
  initialDateFields
  (finishDate {calendar = Gregorian})
  (namedUpdatePart (nameChoices names gregorianMonths) setMonth)
  (\date => index (monthIndex (month {calendar = Gregorian} date)) names)

||| A calendar-specific full month-name field.
public export
pMMMM : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pMMMM {calendar} @{patterned} = calendarMonthNamePattern
  (patternMonthNames {calendar} @{patterned})

||| A calendar-specific abbreviated month-name field.
public export
pMMM : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pMMM {calendar} @{patterned} = calendarMonthNamePattern
  (patternMonthAbbreviations {calendar} @{patterned})

||| A numeric day-of-month field with the requested width.
public export
pday : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Nat -> Pattern DateFields (CalendarDate calendar)
pday width = dateField calendarDay setDayField width 2 1 31

||| A two-digit day-of-month field.
public export
pdd : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pdd = pday 2

||| A two-character day-of-month field padded with a leading space.
public export
pdaySpace : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
            Pattern DateFields (CalendarDate calendar)
pdaySpace = MkPattern
  initialDateFields
  finishDate
  (spaceNumberUpdatePart setDayField 2 1 31)
  (\date => let shown = show (calendarDay date) in
    if length (unpack shown) < 2 then " " ++ shown else shown)

||| A Gregorian weekday field using the supplied Sunday-first names.
|||
||| Parsing consumes and validates a name structurally; the date fields determine
||| the resulting date.
public export
pDayName : Vect 7 String -> Pattern DateFields (CalendarDate Gregorian)
pDayName names = MkPattern
  initialDateFields
  (finishDate {calendar = Gregorian})
  (namedConsumePart (toList names))
  (\date => index (weekdayIndex (calendarWeekday date)) names)

||| A full English weekday-name field for the selected calendar.
public export
pdddd : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pdddd = calendarDayNamePattern weekdayNames

||| An abbreviated English weekday-name field for the selected calendar.
public export
pddd : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pddd = calendarDayNamePattern weekdayAbbreviations

||| A full Gregorian month-name field from an operating-system-style locale.
public export
pMMMM' : Locale -> Pattern DateFields (CalendarDate Gregorian)
pMMMM' locale = pMonthName (monthNames locale)

||| An abbreviated Gregorian month-name field from a locale.
public export
pMMM' : Locale -> Pattern DateFields (CalendarDate Gregorian)
pMMM' locale = pMonthName (monthNamesShort locale)

||| A full Gregorian weekday-name field from a locale.
public export
pdddd' : Locale -> Pattern DateFields (CalendarDate Gregorian)
pdddd' locale = pDayName (dayNames locale)

||| An abbreviated Gregorian weekday-name field from a locale.
public export
pddd' : Locale -> Pattern DateFields (CalendarDate Gregorian)
pddd' locale = pDayName (dayNamesShort locale)

||| The numeric `dd/MM/yyyy` date pattern.
public export
pd : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pd = ((pdd <% char '/') <+> (pMM <% char '/')) <+> pyyyy

||| The long `weekday, dd month yyyy` date pattern.
public export
pD : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pD = (((pdddd <% string ", ") <+> (pdd <% char ' ')) <+>
  (pMMMM <% char ' ')) <+> pyyyy

||| The ISO-style `yyyy-MM-dd` date pattern.
public export
pR : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pR = ((pyyyy <% char '-') <+> (pMM <% char '-')) <+> pdd

||| The `month dd` pattern without a year field.
public export
pmonthDay : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pmonthDay = (pMMMM <% char ' ') <+> pdd

||| The `yyyy month` pattern without a day field.
public export
pyearMonth : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern DateFields (CalendarDate calendar)
pyearMonth = (pyyyy <% char ' ') <+> pMMMM
