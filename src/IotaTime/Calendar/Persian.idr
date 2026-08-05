module IotaTime.Calendar.Persian

import IotaTime.Calendar
import IotaTime.Period
import Data.So
import Derive.Prelude

%language ElabReflection

%default total

||| The astronomical Persian calendar over its vouched year range 1-1500.
public export
data Persian = PersianCalendar

namespace PersianMonths
  public export
  data PersianMonth
    = Farvardin | Ordibehesht | Khordad | Tir | Mordad | Shahrivar
    | Mehr | Aban | Azar | Dey | Bahman | Esfand

  public export
  monthNumber : PersianMonth -> Integer
  monthNumber Farvardin = 1
  monthNumber Ordibehesht = 2
  monthNumber Khordad = 3
  monthNumber Tir = 4
  monthNumber Mordad = 5
  monthNumber Shahrivar = 6
  monthNumber Mehr = 7
  monthNumber Aban = 8
  monthNumber Azar = 9
  monthNumber Dey = 10
  monthNumber Bahman = 11
  monthNumber Esfand = 12

  public export
  Eq PersianMonth where
    left == right = monthNumber left == monthNumber right

  public export
  Ord PersianMonth where
    compare left right = compare (monthNumber left) (monthNumber right)

  %runElab derive `{PersianMonth} [Show]

namespace PersianWeekdays
  public export
  data PersianDayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

  public export
  weekdayNumber : PersianDayOfWeek -> Integer
  weekdayNumber Sunday = 0
  weekdayNumber Monday = 1
  weekdayNumber Tuesday = 2
  weekdayNumber Wednesday = 3
  weekdayNumber Thursday = 4
  weekdayNumber Friday = 5
  weekdayNumber Saturday = 6

  public export
  Eq PersianDayOfWeek where
    left == right = weekdayNumber left == weekdayNumber right

  public export
  Ord PersianDayOfWeek where
    compare left right = compare (weekdayNumber left) (weekdayNumber right)

  %runElab derive `{PersianDayOfWeek} [Show]

monthFromNumber : Integer -> PersianMonth
monthFromNumber 1 = PersianMonths.Farvardin
monthFromNumber 2 = PersianMonths.Ordibehesht
monthFromNumber 3 = PersianMonths.Khordad
monthFromNumber 4 = PersianMonths.Tir
monthFromNumber 5 = PersianMonths.Mordad
monthFromNumber 6 = PersianMonths.Shahrivar
monthFromNumber 7 = PersianMonths.Mehr
monthFromNumber 8 = PersianMonths.Aban
monthFromNumber 9 = PersianMonths.Azar
monthFromNumber 10 = PersianMonths.Dey
monthFromNumber 11 = PersianMonths.Bahman
monthFromNumber _ = PersianMonths.Esfand

public export
persianWeekdayFromDays : Integer -> PersianDayOfWeek
persianWeekdayFromDays value = case (value + 3) `mod` 7 of
  0 => PersianWeekdays.Sunday
  1 => PersianWeekdays.Monday
  2 => PersianWeekdays.Tuesday
  3 => PersianWeekdays.Wednesday
  4 => PersianWeekdays.Thursday
  5 => PersianWeekdays.Friday
  _ => PersianWeekdays.Saturday

export
record PersianDate where
  constructor MkPersianDate
  daysSinceEpoch : Integer

export
Eq PersianDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

export
Ord PersianDate where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

public export
minimumPersianYear : Integer
minimumPersianYear = 1

public export
maximumPersianYear : Integer
maximumPersianYear = 1500

public export
persianEpoch : Integer
persianEpoch = -503284

public export
persianLeapYears : List Integer
persianLeapYears =
  [ 5, 9, 13, 17, 21, 25, 29, 33, 38, 42, 46, 50, 54, 58, 62, 66
  , 71, 75, 79, 83, 87, 91, 95, 99, 104, 108, 112, 116, 120, 124, 128, 132
  , 137, 141, 145, 149, 153, 157, 161, 166, 170, 174, 178, 182, 186, 190, 194
  , 199, 203, 207, 211, 215, 219, 223, 227, 232, 236, 240, 244, 248, 252, 256
  , 260, 265, 269, 273, 277, 281, 285, 289, 293, 298, 302, 306, 310, 314, 318
  , 322, 326, 331, 335, 339, 343, 347, 351, 355, 359, 364, 368, 372, 376, 380
  , 384, 388, 392, 397, 401, 405, 409, 413, 417, 421, 426, 430, 434, 438, 442
  , 446, 450, 454, 459, 463, 467, 471, 475, 479, 483, 487, 492, 496, 500, 504
  , 508, 512, 516, 520, 525, 529, 533, 537, 541, 545, 549, 553, 558, 562, 566
  , 570, 574, 578, 582, 586, 591, 595, 599, 603, 607, 611, 615, 619, 624, 628
  , 632, 636, 640, 644, 648, 652, 657, 661, 665, 669, 673, 677, 681, 686, 690
  , 694, 698, 702, 706, 710, 714, 719, 723, 727, 731, 735, 739, 743, 747, 752
  , 756, 760, 764, 768, 772, 776, 780, 784, 789, 793, 797, 801, 805, 809, 813
  , 818, 822, 826, 830, 834, 838, 842, 846, 851, 855, 859, 863, 867, 871, 875
  , 879, 884, 888, 892, 896, 900, 904, 908, 912, 917, 921, 925, 929, 933, 937
  , 941, 945, 950, 954, 958, 962, 966, 970, 974, 978, 983, 987, 991, 995, 999
  , 1003, 1007, 1011, 1016, 1020, 1024, 1028, 1032, 1036, 1040, 1044, 1049
  , 1053, 1057, 1061, 1065, 1069, 1073, 1077, 1082, 1086, 1090, 1094, 1098
  , 1102, 1106, 1111, 1115, 1119, 1123, 1127, 1131, 1135, 1139, 1144, 1148
  , 1152, 1156, 1160, 1164, 1168, 1172, 1176, 1181, 1185, 1189, 1193, 1197
  , 1201, 1205, 1210, 1214, 1218, 1222, 1226, 1230, 1234, 1238, 1243, 1247
  , 1251, 1255, 1259, 1263, 1267, 1271, 1276, 1280, 1284, 1288, 1292, 1296
  , 1300, 1304, 1309, 1313, 1317, 1321, 1325, 1329, 1333, 1337, 1342, 1346
  , 1350, 1354, 1358, 1362, 1366, 1370, 1375, 1379, 1383, 1387, 1391, 1395
  , 1399, 1403, 1408, 1412, 1416, 1420, 1424, 1428, 1432, 1436, 1441, 1445
  , 1449, 1453, 1457, 1461, 1465, 1469, 1474, 1478, 1482, 1486, 1490, 1494
  , 1498
  ]

||| Whether a supported Persian year contains Esfand 30.
public export
isPersianLeapYear : Year -> Bool
isPersianLeapYear value = elem (yearValue value) persianLeapYears

public export
countLeapsBefore : Integer -> List Integer -> Integer
countLeapsBefore _ [] = 0
countLeapsBefore year (leapYear :: rest) =
  if leapYear < year then 1 + countLeapsBefore year rest else 0

public export
persianNewYearDay : Year -> Integer
persianNewYearDay value =
  persianEpoch + (yearValue value - 1) * 365 +
    countLeapsBefore (yearValue value) persianLeapYears

public export
lastPersianDay : Integer
lastPersianDay = persianNewYearDay 1501 - 1

public export
maxPersianDaysInMonth : PersianMonth -> Year -> DayOfMonth
maxPersianDaysInMonth PersianMonths.Esfand value =
  if isPersianLeapYear value then 30 else 29
maxPersianDaysInMonth valueMonth _ =
  if PersianMonths.monthNumber valueMonth <= 6 then 31 else 30

public export
isValidPersianDate : DayOfMonth -> PersianMonth -> Year -> Bool
isValidPersianDate valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
      maxDay = dayOfMonthValue (maxPersianDaysInMonth valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay &&
      yearNumber >= minimumPersianYear && yearNumber <= maximumPersianYear

public export
monthOffset : PersianMonth -> Integer
monthOffset value =
  let number = PersianMonths.monthNumber value
   in if number <= 6 then (number - 1) * 31
      else 186 + (number - 7) * 30

public export
persianDaysFromCivil : Year -> PersianMonth -> DayOfMonth -> Integer
persianDaysFromCivil valueYear valueMonth valueDay =
  persianNewYearDay valueYear + monthOffset valueMonth +
    dayOfMonthValue valueDay - 1

findPersianYear : Nat -> Integer -> Integer -> Integer
findPersianYear Z estimate days = estimate
findPersianYear (S fuel) estimate days =
  if days < persianNewYearDay (yearFromInteger estimate)
    then findPersianYear fuel (estimate - 1) days
    else if days >= persianNewYearDay (yearFromInteger (estimate + 1))
      then findPersianYear fuel (estimate + 1) days
      else estimate

persianCivilFromDays : Integer -> (Year, PersianMonth, DayOfMonth)
persianCivilFromDays value =
  let estimate = max minimumPersianYear (min maximumPersianYear
        ((value - persianEpoch) `div` 365 + 1))
      yearNumber = findPersianYear 1500 estimate value
      valueYear = yearFromInteger yearNumber
      dayOfYear = value - persianNewYearDay valueYear
      monthNumber = if dayOfYear == 365 then 12
        else if dayOfYear < 186 then dayOfYear `div` 31 + 1
        else (dayOfYear - 186) `div` 30 + 7
      offset = if monthNumber <= 6 then (monthNumber - 1) * 31
        else 186 + (monthNumber - 7) * 30
      dayNumber = dayOfYear - offset + 1
   in (valueYear, monthFromNumber monthNumber,
       dayOfMonthFromInteger dayNumber)

public export
isValidPersianDays : Integer -> Bool
isValidPersianDays value = value >= persianEpoch && value <= lastPersianDay

public export
HasCalendarDate PersianDate where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = isValidPersianDays
  calendarDateFromDays days = MkPersianDate days
  calendarDateName = "Persian"

clampToPersian : Integer -> Integer
clampToPersian = max persianEpoch . min lastPersianDay

shiftPersianDays : Integer -> PersianDate -> PersianDate
shiftPersianDays amount date =
  MkPersianDate (clampToPersian (date.daysSinceEpoch + amount))

shiftPersianMonths : Integer -> PersianDate -> PersianDate
shiftPersianMonths amount date =
  let (valueYear, valueMonth, valueDay) =
        persianCivilFromDays date.daysSinceEpoch
      monthOrdinal = PersianMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay (maxPersianDaysInMonth targetMonth targetYear)
   in MkPersianDate (clampToPersian
        (persianDaysFromCivil targetYear targetMonth targetDay))

shiftPersianYears : Integer -> PersianDate -> PersianDate
shiftPersianYears amount date =
  let (valueYear, valueMonth, valueDay) =
        persianCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxPersianDaysInMonth valueMonth targetYear)
   in MkPersianDate (clampToPersian
        (persianDaysFromCivil targetYear valueMonth targetDay))

applyPersianPeriod : Period target -> PersianDate -> PersianDate
applyPersianPeriod period =
    shiftPersianDays (periodDays period)
  . shiftPersianDays (7 * periodWeeks period)
  . shiftPersianMonths (periodMonths period)
  . shiftPersianYears (periodYears period)

persianDayOfWeek : PersianDate -> PersianDayOfWeek
persianDayOfWeek date = persianWeekdayFromDays date.daysSinceEpoch

nextPersian : Integer -> PersianDayOfWeek -> PersianDate -> PersianDate
nextPersian count target date =
  let current = PersianWeekdays.weekdayNumber (persianDayOfWeek date)
      wanted = PersianWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in MkPersianDate
        (clampToPersian (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousPersian : Integer -> PersianDayOfWeek -> PersianDate -> PersianDate
previousPersian count target date =
  let current = PersianWeekdays.weekdayNumber (persianDayOfWeek date)
      wanted = PersianWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in MkPersianDate
        (clampToPersian (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
Calendar Persian where
  DateRep = PersianDate
  MonthRep _ = PersianMonth
  WeekdayRep = PersianDayOfWeek

  isValidDays = isValidPersianDays
  fromDays days = MkPersianDate days
  toDays date = date.daysSinceEpoch
  calendarName = "Persian"

  year' date = let (value, _, _) = persianCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    persianCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = persianCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = persianCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyPersianPeriod
  shiftCalendarDays' = shiftPersianDays

  dayOfWeek = persianDayOfWeek
  next = nextPersian
  previous = previousPersian

public export
HasCalendar PersianDate where
  calendarCapability = ()

public export
ApplyPeriod PersianDate where
  applyPeriod = applyPersianPeriod

||| Construct a statically validated Persian date in years 1-1500.
public export
persianDate : (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
              (valueYear : Year) ->
              {auto 0 valid : So
                (isValidPersianDate valueDay valueMonth valueYear)} ->
              CalendarDate Persian
persianDate valueDay valueMonth valueYear =
  MkPersianDate (persianDaysFromCivil valueYear valueMonth valueDay)

||| Failures produced while refining untrusted Persian date data.
public export
data PersianDateError
  = InvalidPersianDate DayOfMonth PersianMonth Year
  | InvalidPersianDayCount Integer
  | InvalidPersianNthDay DayNth PersianDayOfWeek PersianMonth Year
  | InvalidPersianWeekDate WeekNumber PersianDayOfWeek Year

||| Validate runtime day, month, and year components as a Persian date.
public export
refinePersianDate : DayOfMonth -> PersianMonth -> Year ->
                    Either PersianDateError (CalendarDate Persian)
refinePersianDate valueDay valueMonth valueYear =
  case choose (isValidPersianDate valueDay valueMonth valueYear) of
    Left valid => Right (persianDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidPersianDate valueDay valueMonth valueYear)

||| Construct a Persian date from a statically valid calendar-relative day count.
public export
persianFromDays : (days : Integer) ->
                  {auto 0 valid : So (isValidPersianDays days)} ->
                  CalendarDate Persian
persianFromDays days = MkPersianDate days

||| Validate a runtime Persian day count within the supported year range.
public export
refinePersianDays : Integer -> Either PersianDateError (CalendarDate Persian)
refinePersianDays days = case choose (isValidPersianDays days) of
  Left valid => Right (persianFromDays days @{valid})
  Right _ => Left (InvalidPersianDayCount days)

public export
nthPersianDayOfMonth : DayNth -> PersianDayOfWeek -> PersianMonth -> Year ->
                       DayOfMonth
nthPersianDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxPersianDaysInMonth valueMonth valueYear
      firstOffset = (PersianWeekdays.weekdayNumber target -
        PersianWeekdays.weekdayNumber (persianWeekdayFromDays
          (persianDaysFromCivil valueYear valueMonth 1))) `mod` 7
      lastOffset = (PersianWeekdays.weekdayNumber (persianWeekdayFromDays
        (persianDaysFromCivil valueYear valueMonth monthLength)) -
        PersianWeekdays.weekdayNumber target) `mod` 7
      dayNumber = case nth of
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
        Last => dayOfMonthValue monthLength - lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidPersianNthDay : DayNth -> PersianDayOfWeek -> PersianMonth -> Year -> Bool
isValidPersianNthDay nth target valueMonth valueYear =
  yearValue valueYear >= minimumPersianYear &&
  yearValue valueYear <= maximumPersianYear && case nth of
    Fifth => nthPersianDayOfMonth nth target valueMonth valueYear <=
      maxPersianDaysInMonth valueMonth valueYear
    _ => True

||| Construct the nth requested weekday in a Persian month.
public export
persianFromNthDay : (nth : DayNth) -> (target : PersianDayOfWeek) ->
                    (valueMonth : PersianMonth) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidPersianNthDay nth target valueMonth valueYear)} ->
                    CalendarDate Persian
persianFromNthDay nth target valueMonth valueYear =
  MkPersianDate (persianDaysFromCivil valueYear valueMonth
    (nthPersianDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Persian month.
public export
refinePersianNthDay : DayNth -> PersianDayOfWeek -> PersianMonth -> Year ->
                      Either PersianDateError (CalendarDate Persian)
refinePersianNthDay nth target valueMonth valueYear =
  case choose (isValidPersianNthDay nth target valueMonth valueYear) of
    Left valid => Right
      (persianFromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidPersianNthDay nth target valueMonth valueYear)

public export
persianWeekDateDays : WeekNumber -> PersianDayOfWeek -> Year -> Integer
persianWeekDateDays week target valueYear =
  let firstDay = persianDaysFromCivil valueYear PersianMonths.Farvardin 1
      firstWeekStart = firstDay -
        ((PersianWeekdays.weekdayNumber (persianWeekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (PersianWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

public export
isValidPersianWeekDate : WeekNumber -> PersianDayOfWeek -> Year -> Bool
isValidPersianWeekDate week target valueYear =
  let days = persianWeekDateDays week target valueYear
   in yearValue valueYear >= minimumPersianYear &&
      yearValue valueYear <= maximumPersianYear && isValidPersianDays days

||| Construct a Persian Saturday-based week date under static validity evidence.
public export
persianFromWeekDate : (week : WeekNumber) -> (target : PersianDayOfWeek) ->
                      (valueYear : Year) ->
                      {auto 0 valid : So
                        (isValidPersianWeekDate week target valueYear)} ->
                      CalendarDate Persian
persianFromWeekDate week target valueYear =
  MkPersianDate (persianWeekDateDays week target valueYear)

||| Validate a runtime Persian Saturday-based week date.
public export
refinePersianWeekDate : WeekNumber -> PersianDayOfWeek -> Year ->
                        Either PersianDateError (CalendarDate Persian)
refinePersianWeekDate week target valueYear =
  case choose (isValidPersianWeekDate week target valueYear) of
    Left valid => Right (persianFromWeekDate week target valueYear @{valid})
    Right _ => Left (InvalidPersianWeekDate week target valueYear)
