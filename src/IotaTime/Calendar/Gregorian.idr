module IotaTime.Calendar.Gregorian

import IotaTime.Calendar
import IotaTime.Period
import Data.So

public export
data Gregorian = GregorianCalendar

public export
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

public export
monthNumber : Month -> Integer
monthNumber January = 1
monthNumber February = 2
monthNumber March = 3
monthNumber April = 4
monthNumber May = 5
monthNumber June = 6
monthNumber July = 7
monthNumber August = 8
monthNumber September = 9
monthNumber October = 10
monthNumber November = 11
monthNumber December = 12

public export
Eq Month where
  left == right = monthNumber left == monthNumber right

public export
Ord Month where
  compare left right = compare (monthNumber left) (monthNumber right)

public export
Show Month where
  show January = "January"
  show February = "February"
  show March = "March"
  show April = "April"
  show May = "May"
  show June = "June"
  show July = "July"
  show August = "August"
  show September = "September"
  show October = "October"
  show November = "November"
  show December = "December"

public export
data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

public export
weekdayNumber : DayOfWeek -> Integer
weekdayNumber Sunday = 0
weekdayNumber Monday = 1
weekdayNumber Tuesday = 2
weekdayNumber Wednesday = 3
weekdayNumber Thursday = 4
weekdayNumber Friday = 5
weekdayNumber Saturday = 6

public export
Eq DayOfWeek where
  left == right = weekdayNumber left == weekdayNumber right

public export
Ord DayOfWeek where
  compare left right = compare (weekdayNumber left) (weekdayNumber right)

public export
Show DayOfWeek where
  show Sunday = "Sunday"
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"

export
record GregorianDate where
  constructor MkGregorianDate
  daysSinceEpoch : Integer

export
Eq GregorianDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

export
Ord GregorianDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

monthFromNumber : Integer -> Month
monthFromNumber 1 = January
monthFromNumber 2 = February
monthFromNumber 3 = March
monthFromNumber 4 = April
monthFromNumber 5 = May
monthFromNumber 6 = June
monthFromNumber 7 = July
monthFromNumber 8 = August
monthFromNumber 9 = September
monthFromNumber 10 = October
monthFromNumber 11 = November
monthFromNumber _ = December

weekdayFromNumber : Integer -> DayOfWeek
weekdayFromNumber value = case value `mod` 7 of
  0 => Sunday
  1 => Monday
  2 => Tuesday
  3 => Wednesday
  4 => Thursday
  5 => Friday
  _ => Saturday

public export
isLeapYear : Year -> Bool
isLeapYear value =
  let number = yearValue value
   in number `mod` 400 == 0 || (number `mod` 4 == 0 && number `mod` 100 /= 0)

public export
maxDaysInMonth : Month -> Year -> DayOfMonth
maxDaysInMonth February value = if isLeapYear value then 29 else 28
maxDaysInMonth April _ = 30
maxDaysInMonth June _ = 30
maxDaysInMonth September _ = 30
maxDaysInMonth November _ = 30
maxDaysInMonth _ _ = 31

public export
isValidGregorianDate : DayOfMonth -> Month -> Year -> Bool
isValidGregorianDate valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
   in dayNumber >= 1 &&
    dayNumber <= dayOfMonthValue (maxDaysInMonth valueMonth valueYear) &&
    (yearNumber > 1582 ||
      (yearNumber == 1582 &&
      (monthNumber valueMonth > monthNumber October ||
        (valueMonth == October && dayNumber >= 15))))

daysFromCivil : Year -> Month -> DayOfMonth -> Integer
daysFromCivil valueYear valueMonth valueDay =
  let number = monthNumber valueMonth
      yearNumber = yearValue valueYear
      dayNumber = dayOfMonthValue valueDay
      shiftedYear = if number <= 2 then yearNumber - 1 else yearNumber
      era = shiftedYear `div` 400
      yearOfEra = shiftedYear - era * 400
      shiftedMonth = number + if number > 2 then -3 else 9
      dayOfYear = (153 * shiftedMonth + 2) `div` 5 + dayNumber - 1
      dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 - yearOfEra `div` 100 + dayOfYear
   in era * 146097 + dayOfEra - 730485

civilFromDays : Integer -> (Year, Month, DayOfMonth)
civilFromDays value =
  let shifted = value + 730485
      era = shifted `div` 146097
      dayOfEra = shifted - era * 146097
      yearOfEra =
        (dayOfEra - dayOfEra `div` 1460 + dayOfEra `div` 36524 - dayOfEra `div` 146096)
          `div` 365
      partialYear = yearOfEra + era * 400
      dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra `div` 4 - yearOfEra `div` 100)
      shiftedMonth = (5 * dayOfYear + 2) `div` 153
      valueDay = dayOfYear - (153 * shiftedMonth + 2) `div` 5 + 1
      monthValue = shiftedMonth + if shiftedMonth < 10 then 3 else -9
      valueYear = partialYear + if monthValue <= 2 then 1 else 0
  in (yearFromInteger valueYear, monthFromNumber monthValue, dayOfMonthFromInteger valueDay)

firstGregorianDay : Integer
firstGregorianDay = -152444

public export
isValidGregorianDays : Integer -> Bool
isValidGregorianDays value = value >= -152444

clampToGregorian : Integer -> Integer
clampToGregorian value = max firstGregorianDay value

makeDate : Year -> Month -> DayOfMonth -> GregorianDate
makeDate valueYear valueMonth valueDay =
  MkGregorianDate (clampToGregorian (daysFromCivil valueYear valueMonth valueDay))

normalizeGregorianDay : Integer -> GregorianDate -> GregorianDate
normalizeGregorianDay targetDay date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      firstOfMonth = daysFromCivil valueYear valueMonth 1
  in MkGregorianDate
        (clampToGregorian (firstOfMonth + targetDay - 1))

shiftGregorianDays : Integer -> GregorianDate -> GregorianDate
shiftGregorianDays amount date =
  MkGregorianDate (clampToGregorian (date.daysSinceEpoch + amount))

normalizeGregorianMonth : Integer -> GregorianDate -> GregorianDate
normalizeGregorianMonth targetMonth date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + targetMonth `div` 12)
      normalizedMonth = monthFromNumber (targetMonth `mod` 12 + 1)
      targetDay = min valueDay (maxDaysInMonth normalizedMonth targetYear)
   in makeDate targetYear normalizedMonth targetDay

shiftGregorianMonths : Integer -> GregorianDate -> GregorianDate
shiftGregorianMonths amount date =
  let (_, valueMonth, _) = civilFromDays date.daysSinceEpoch
   in normalizeGregorianMonth (monthNumber valueMonth - 1 + amount) date

modifyGregorianYear : (Year -> Year) -> GregorianDate -> GregorianDate
modifyGregorianYear transform date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      targetYear = transform valueYear
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
   in makeDate targetYear valueMonth targetDay

applyGregorianPeriod : Period target -> GregorianDate -> GregorianDate
applyGregorianPeriod period =
    shiftGregorianDays (periodDays period)
  . shiftGregorianDays (7 * periodWeeks period)
  . shiftGregorianMonths (periodMonths period)
  . modifyGregorianYear
      (\valueYear => yearFromInteger (yearValue valueYear + periodYears period))

public export
HasCalendar GregorianDate where
  calendarCapability = ()

public export
ApplyPeriod GregorianDate where
  applyPeriod = applyGregorianPeriod

gregorianDayOfWeek : GregorianDate -> DayOfWeek
gregorianDayOfWeek date = weekdayFromNumber (date.daysSinceEpoch + 3)

nextGregorian : Integer -> DayOfWeek -> GregorianDate -> GregorianDate
nextGregorian count target date =
  let current = weekdayNumber (gregorianDayOfWeek date)
      wanted = weekdayNumber target
      weeks = if wanted > current then count - 1 else count
  in MkGregorianDate (clampToGregorian (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousGregorian : Integer -> DayOfWeek -> GregorianDate -> GregorianDate
previousGregorian count target date =
  let current = weekdayNumber (gregorianDayOfWeek date)
      wanted = weekdayNumber target
      weeks = if wanted < current then count - 1 else count
  in MkGregorianDate
      (clampToGregorian (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
Calendar Gregorian where
  DateRep = GregorianDate
  MonthRep _ = Month
  WeekdayRep = DayOfWeek

  isValidDays = isValidGregorianDays
  fromDays days = MkGregorianDate days
  toDays date = date.daysSinceEpoch
  calendarName = "Gregorian"

  year' date = let (value, _, _) = civilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = civilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = civilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyGregorianPeriod
  shiftCalendarDays' = shiftGregorianDays

  dayOfWeek = gregorianDayOfWeek
  next = nextGregorian
  previous = previousGregorian

public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : Month) -> (valueYear : Year) ->
               {auto 0 valid : So (isValidGregorianDate valueDay valueMonth valueYear)} ->
               CalendarDate Gregorian
calendarDate valueDay valueMonth valueYear =
  MkGregorianDate (daysFromCivil valueYear valueMonth valueDay)

public export
data GregorianDateError
  = InvalidGregorianDate DayOfMonth Month Year
  | InvalidGregorianDayCount Integer
  | InvalidGregorianNthDay DayNth DayOfWeek Month Year
  | InvalidGregorianWeekDate WeekNumber DayOfWeek Year

public export
refineGregorianDate : DayOfMonth -> Month -> Year ->
                      Either GregorianDateError (CalendarDate Gregorian)
refineGregorianDate valueDay valueMonth valueYear =
  case choose (isValidGregorianDate valueDay valueMonth valueYear) of
    Left valid => Right (calendarDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidGregorianDate valueDay valueMonth valueYear)

public export
gregorianFromDays : (days : Integer) ->
                    {auto 0 valid : So (isValidGregorianDays days)} ->
                    CalendarDate Gregorian
gregorianFromDays days = MkGregorianDate days

public export
refineGregorianDays : (days : Integer) -> Either GregorianDateError (CalendarDate Gregorian)
refineGregorianDays days =
  case choose (isValidGregorianDays days) of
    Left valid => Right (gregorianFromDays days @{valid})
    Right _ => Left (InvalidGregorianDayCount days)

nthDayOfMonth : DayNth -> DayOfWeek -> Month -> Year -> DayOfMonth
nthDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstDate = MkGregorianDate (daysFromCivil valueYear valueMonth 1)
      firstOffset = (weekdayNumber target - weekdayNumber (gregorianDayOfWeek firstDate)) `mod` 7
      lastDate = MkGregorianDate (daysFromCivil valueYear valueMonth monthLength)
      lastOffset = (weekdayNumber (gregorianDayOfWeek lastDate) - weekdayNumber target) `mod` 7
      dayNumber = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => dayOfMonthValue monthLength - lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidGregorianNthDay : DayNth -> DayOfWeek -> Month -> Year -> Bool
isValidGregorianNthDay nth target valueMonth valueYear =
  if yearValue valueYear > 1582
    then case nth of
      Fifth => nthDayOfMonth nth target valueMonth valueYear <= maxDaysInMonth valueMonth valueYear
      _ => True
    else isValidGregorianDate
      (nthDayOfMonth nth target valueMonth valueYear) valueMonth valueYear

public export
fromNthDay : (nth : DayNth) -> (target : DayOfWeek) ->
             (valueMonth : Month) -> (valueYear : Year) ->
             {auto 0 valid : So (isValidGregorianNthDay nth target valueMonth valueYear)} ->
             CalendarDate Gregorian
fromNthDay nth target valueMonth valueYear =
  MkGregorianDate
    (daysFromCivil valueYear valueMonth (nthDayOfMonth nth target valueMonth valueYear))

public export
refineGregorianNthDay : DayNth -> DayOfWeek -> Month -> Year ->
                        Either GregorianDateError (CalendarDate Gregorian)
refineGregorianNthDay nth target valueMonth valueYear =
  case choose (isValidGregorianNthDay nth target valueMonth valueYear) of
    Left valid => Right (fromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidGregorianNthDay nth target valueMonth valueYear)

weekDateDays : WeekNumber -> DayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = daysFromCivil valueYear January 1
      firstWeekStart = firstDay - weekdayNumber (gregorianDayOfWeek (MkGregorianDate firstDay))
  in firstWeekStart + 7 * (weekNumberValue week - 1) + weekdayNumber target

public export
isValidGregorianWeekDate : WeekNumber -> DayOfWeek -> Year -> Bool
isValidGregorianWeekDate week target valueYear =
  (yearValue valueYear > 1582 && weekNumberValue week >= 0) ||
    isValidGregorianDays (weekDateDays week target valueYear)

public export
fromWeekDate : (week : WeekNumber) -> (target : DayOfWeek) -> (valueYear : Year) ->
               {auto 0 valid : So (isValidGregorianWeekDate week target valueYear)} ->
               CalendarDate Gregorian
fromWeekDate week target valueYear =
  MkGregorianDate (weekDateDays week target valueYear)

public export
refineGregorianWeekDate : WeekNumber -> DayOfWeek -> Year ->
                          Either GregorianDateError (CalendarDate Gregorian)
refineGregorianWeekDate week target valueYear =
  case choose (isValidGregorianWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidGregorianWeekDate week target valueYear)