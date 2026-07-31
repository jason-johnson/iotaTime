module IotaTime.Calendar.Gregorian

import IotaTime.Calendar

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
  value `mod` 400 == 0 || (value `mod` 4 == 0 && value `mod` 100 /= 0)

public export
maxDaysInMonth : Month -> Year -> DayOfMonth
maxDaysInMonth February value = if isLeapYear value then 29 else 28
maxDaysInMonth April _ = 30
maxDaysInMonth June _ = 30
maxDaysInMonth September _ = 30
maxDaysInMonth November _ = 30
maxDaysInMonth _ _ = 31

daysFromCivil : Year -> Month -> DayOfMonth -> Integer
daysFromCivil valueYear valueMonth valueDay =
  let number = monthNumber valueMonth
      shiftedYear = if number <= 2 then valueYear - 1 else valueYear
      era = shiftedYear `div` 400
      yearOfEra = shiftedYear - era * 400
      shiftedMonth = number + if number > 2 then -3 else 9
      dayOfYear = (153 * shiftedMonth + 2) `div` 5 + valueDay - 1
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
   in (valueYear, monthFromNumber monthValue, valueDay)

firstGregorianDay : Integer
firstGregorianDay = daysFromCivil 1582 October 15

clampToGregorian : Integer -> Integer
clampToGregorian value = max firstGregorianDay value

makeDate : Year -> Month -> DayOfMonth -> GregorianDate
makeDate valueYear valueMonth valueDay =
  MkGregorianDate (clampToGregorian (daysFromCivil valueYear valueMonth valueDay))

modifyGregorianDay : (DayOfMonth -> DayOfMonth) -> GregorianDate -> GregorianDate
modifyGregorianDay transform date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      firstOfMonth = daysFromCivil valueYear valueMonth 1
   in MkGregorianDate (clampToGregorian (firstOfMonth + transform valueDay - 1))

modifyGregorianMonth : (Integer -> Integer) -> GregorianDate -> GregorianDate
modifyGregorianMonth transform date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      zeroBased = monthNumber valueMonth - 1
      transformed = transform zeroBased
      targetYear = valueYear + transformed `div` 12
      targetMonth = monthFromNumber (transformed `mod` 12 + 1)
      targetDay = min valueDay (maxDaysInMonth targetMonth targetYear)
   in makeDate targetYear targetMonth targetDay

modifyGregorianYear : (Year -> Year) -> GregorianDate -> GregorianDate
modifyGregorianYear transform date =
  let (valueYear, valueMonth, valueDay) = civilFromDays date.daysSinceEpoch
      targetYear = transform valueYear
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
   in makeDate targetYear valueMonth targetDay

gregorianDayLens : Lens' GregorianDate DayOfMonth
gregorianDayLens = lens
  (\date => let (_, _, valueDay) = civilFromDays date.daysSinceEpoch in valueDay)
  (\date, valueDay => modifyGregorianDay (const valueDay) date)

gregorianMonthLens : Lens' GregorianDate Integer
gregorianMonthLens = lens
  (\date => let (_, valueMonth, _) = civilFromDays date.daysSinceEpoch in monthNumber valueMonth - 1)
  (\date, valueMonth => modifyGregorianMonth (const valueMonth) date)

gregorianYearLens : Lens' GregorianDate Year
gregorianYearLens = lens
  (\date => let (valueYear, _, _) = civilFromDays date.daysSinceEpoch in valueYear)
  (\date, valueYear => modifyGregorianYear (const valueYear) date)

gregorianDayOfWeek : GregorianDate -> DayOfWeek
gregorianDayOfWeek date = weekdayFromNumber (date.daysSinceEpoch + 3)

nextGregorian : Integer -> DayOfWeek -> GregorianDate -> GregorianDate
nextGregorian count target date =
  let current = weekdayNumber (gregorianDayOfWeek date)
      wanted = weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in MkGregorianDate (date.daysSinceEpoch + 7 * weeks + wanted - current)

previousGregorian : Integer -> DayOfWeek -> GregorianDate -> GregorianDate
previousGregorian count target date =
  let current = weekdayNumber (gregorianDayOfWeek date)
      wanted = weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in MkGregorianDate (date.daysSinceEpoch - (7 * weeks + current - wanted))

public export
Calendar Gregorian where
  DateRep = GregorianDate
  MonthRep = Month
  WeekdayRep = DayOfWeek

  fromDays = MkGregorianDate
  toDays date = date.daysSinceEpoch
  toYmd = civilFromDays . daysSinceEpoch
  isValidDate' date = date.daysSinceEpoch >= firstGregorianDay
  calendarName = "Gregorian"

  day' = gregorianDayLens
  month' date = let (_, value, _) = civilFromDays date.daysSinceEpoch in value
  monthl' = gregorianMonthLens
  year' = gregorianYearLens

  dayOfWeek = gregorianDayOfWeek
  next = nextGregorian
  previous = previousGregorian

public export
calendarDate : DayOfMonth -> Month -> Year -> Maybe (CalendarDate Gregorian)
calendarDate valueDay valueMonth valueYear =
  let rawDays = daysFromCivil valueYear valueMonth valueDay
   in if valueDay < 1 || valueDay > maxDaysInMonth valueMonth valueYear || rawDays < firstGregorianDay
        then Nothing
        else Just (MkGregorianDate rawDays)

public export
validatedCalendarDate : DayOfMonth -> Month -> Year -> Maybe (ValidatedDate Gregorian)
validatedCalendarDate valueDay valueMonth valueYear =
  case calendarDate valueDay valueMonth valueYear of
    Nothing => Nothing
    Just date => validateDate date

public export
fromNthDay : DayNth -> DayOfWeek -> Month -> Year -> Maybe (CalendarDate Gregorian)
fromNthDay nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstDate = MkGregorianDate (daysFromCivil valueYear valueMonth 1)
      firstOffset = (weekdayNumber target - weekdayNumber (gregorianDayOfWeek firstDate)) `mod` 7
      lastDate = MkGregorianDate (daysFromCivil valueYear valueMonth monthLength)
      lastOffset = (weekdayNumber (gregorianDayOfWeek lastDate) - weekdayNumber target) `mod` 7
      valueDay = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => monthLength - lastOffset
   in calendarDate valueDay valueMonth valueYear

public export
validatedFromNthDay : DayNth -> DayOfWeek -> Month -> Year -> Maybe (ValidatedDate Gregorian)
validatedFromNthDay nth target valueMonth valueYear =
  case fromNthDay nth target valueMonth valueYear of
    Nothing => Nothing
    Just date => validateDate date

public export
fromWeekDate : WeekNumber -> DayOfWeek -> Year -> Maybe (CalendarDate Gregorian)
fromWeekDate week target valueYear =
  let firstDay = daysFromCivil valueYear January 1
      firstWeekStart = firstDay - weekdayNumber (gregorianDayOfWeek (MkGregorianDate firstDay))
      result = firstWeekStart + 7 * (week - 1) + weekdayNumber target
  in if result < firstGregorianDay
        then Nothing
        else Just (MkGregorianDate result)

public export
validatedFromWeekDate : WeekNumber -> DayOfWeek -> Year -> Maybe (ValidatedDate Gregorian)
validatedFromWeekDate week target valueYear =
  case fromWeekDate week target valueYear of
    Nothing => Nothing
    Just date => validateDate date