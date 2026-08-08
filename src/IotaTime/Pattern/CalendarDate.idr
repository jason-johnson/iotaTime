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

gregorianMonths : Vect 12 Month
gregorianMonths =
  [ January, February, March, April, May, June
  , July, August, September, October, November, December
  ]

gregorianWeekdays : Vect 7 DayOfWeek
gregorianWeekdays =
  [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday ]

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

||| A calendar month field using supplied names in calendar order.
public export
pMonthName : {default Gregorian calendar : Type} ->
             {auto patterned : CalendarPattern calendar} ->
             {size : Nat} ->
             (names : Vect size String) ->
             {auto 0 complete : size =
               cast (patternMonthLimit {calendar} @{patterned})} ->
             Pattern DateFields (CalendarDate calendar)
pMonthName names = calendarMonthNamePattern (toList names)

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

||| A calendar weekday field using supplied Sunday-first names.
|||
||| Parsing consumes and validates a name structurally; the date fields determine
||| the resulting date.
public export
pDayName : {default Gregorian calendar : Type} ->
           {auto patterned : CalendarPattern calendar} ->
           Vect 7 String -> Pattern DateFields (CalendarDate calendar)
pDayName names = calendarDayNamePattern (toList names)

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

localeMonthNames : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Locale -> List String
localeMonthNames {calendar} @{patterned} locale =
  case patternMonthNameSource {calendar} @{patterned} of
    GregorianLocaleMonthNames => toList (monthNames locale)
    CanonicalCalendarMonthNames => patternMonthNames {calendar} @{patterned}

localeMonthAbbreviations : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Locale -> List String
localeMonthAbbreviations {calendar} @{patterned} locale =
  case patternMonthNameSource {calendar} @{patterned} of
    GregorianLocaleMonthNames => toList (monthNamesShort locale)
    CanonicalCalendarMonthNames =>
      patternMonthAbbreviations {calendar} @{patterned}

||| A full month-name field using locale names when the selected calendar
||| shares Gregorian month identities, and canonical names otherwise.
public export
pMMMM' : {default Gregorian calendar : Type} ->
         {auto patterned : CalendarPattern calendar} ->
   Locale -> Pattern DateFields (CalendarDate calendar)
pMMMM' {calendar} @{patterned} locale = calendarMonthNamePattern
  (localeMonthNames {calendar} @{patterned} locale)

||| An abbreviated month-name field using locale names when the selected
||| calendar shares Gregorian month identities, and canonical names otherwise.
public export
pMMM' : {default Gregorian calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Locale -> Pattern DateFields (CalendarDate calendar)
pMMM' {calendar} @{patterned} locale = calendarMonthNamePattern
  (localeMonthAbbreviations {calendar} @{patterned} locale)

||| A full locale weekday-name field for the selected calendar.
public export
pdddd' : {default Gregorian calendar : Type} ->
         {auto patterned : CalendarPattern calendar} ->
   Locale -> Pattern DateFields (CalendarDate calendar)
pdddd' {calendar} locale = pDayName {calendar} (dayNames locale)

||| An abbreviated locale weekday-name field for the selected calendar.
public export
pddd' : {default Gregorian calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Locale -> Pattern DateFields (CalendarDate calendar)
pddd' {calendar} locale = pDayName {calendar} (dayNamesShort locale)

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
