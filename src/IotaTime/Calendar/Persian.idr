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

public export
Eq PersianDate where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

public export
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
Show PersianDate where
  show date = case persianCivilFromDays date.daysSinceEpoch of
    (valueYear, valueMonth, valueDay) =>
      "persianDate " ++ show valueDay ++ " " ++
      show valueMonth ++ " " ++ show valueYear

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
        FourthToLast => dayOfMonthValue monthLength - lastOffset - 21
        ThirdToLast => dayOfMonthValue monthLength - lastOffset - 14
        SecondToLast => dayOfMonthValue monthLength - lastOffset - 7
        Last => dayOfMonthValue monthLength - lastOffset
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
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

||| Exact arithmetic rules available for the Solar Hijri calendar.
public export
data PersianArithmeticRule = Simple | Birashk

||| A Persian calendar whose leap years are fixed by an arithmetic rule.
public export
data ArithmeticPersian : PersianArithmeticRule -> Type where
  MkArithmeticPersian : ArithmeticPersian rule

||| The legacy 33-year Persian cycle used by the BCL before .NET 4.6.
public export
PersianSimple : Type
PersianSimple = ArithmeticPersian Simple

||| Ahmad Birashk's nested 2820-year arithmetic Persian cycle.
public export
PersianArithmetic : Type
PersianArithmetic = ArithmeticPersian Birashk

public export
interface KnownPersianArithmeticRule (rule : PersianArithmeticRule) where
  arithmeticPersianName : String
  arithmeticPersianConstructorName : String
  arithmeticPersianEpoch : Integer
  arithmeticPersianLeapYear : Integer -> Bool
  arithmeticPersianNewYear : Integer -> Integer

simpleLeapPositions : List Integer
simpleLeapPositions = [1, 5, 9, 13, 17, 22, 26, 30]

countAtMost : Integer -> List Integer -> Integer
countAtMost _ [] = 0
countAtMost limit (value :: rest) =
  if value <= limit then 1 + countAtMost limit rest else 0

simpleLeapsBefore : Integer -> Integer
simpleLeapsBefore year =
  let elapsed = year - 1
   in (elapsed `div` 33) * 8 +
      countAtMost (elapsed `mod` 33) simpleLeapPositions

simpleNewYear : Integer -> Integer
simpleNewYear year = -503285 + (year - 1) * 365 + simpleLeapsBefore year

arithmeticNewYear : Integer -> Integer
arithmeticNewYear year =
  let base = year - 474
      cycleYear = 474 + base `mod` 2820
   in -503284 + ((cycleYear * 682 - 110) `div` 2816) +
      (cycleYear - 1) * 365 + (base `div` 2820) * 1029983

public export
KnownPersianArithmeticRule Simple where
  arithmeticPersianName = "Persian Simple"
  arithmeticPersianConstructorName = "simplePersianDate"
  arithmeticPersianEpoch = -503285
  arithmeticPersianLeapYear year = simpleNewYear (year + 1) - simpleNewYear year == 366
  arithmeticPersianNewYear = simpleNewYear

public export
KnownPersianArithmeticRule Birashk where
  arithmeticPersianName = "Persian Arithmetic"
  arithmeticPersianConstructorName = "arithmeticPersianDate"
  arithmeticPersianEpoch = -503284
  arithmeticPersianLeapYear year =
    let cycleYear = (year - 474) `mod` 2820 + 474
     in ((cycleYear + 38) * 31) `mod` 128 < 31
  arithmeticPersianNewYear = arithmeticNewYear

export
record ArithmeticPersianDate (rule : PersianArithmeticRule) where
  constructor MkArithmeticPersianDate
  arithmeticDaysSinceEpoch : Integer

public export
Eq (ArithmeticPersianDate rule) where
  left == right = left.arithmeticDaysSinceEpoch == right.arithmeticDaysSinceEpoch

public export
Ord (ArithmeticPersianDate rule) where
  compare left right = compare left.arithmeticDaysSinceEpoch right.arithmeticDaysSinceEpoch

public export
minimumArithmeticPersianYear : Integer
minimumArithmeticPersianYear = 1

public export
maximumArithmeticPersianYear : Integer
maximumArithmeticPersianYear = 9377

public export
isArithmeticPersianLeapYear : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> Bool
isArithmeticPersianLeapYear {rule} value =
  arithmeticPersianLeapYear {rule} (yearValue value)

public export
arithmeticPersianNewYearDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> Integer
arithmeticPersianNewYearDay {rule} value =
  arithmeticPersianNewYear {rule} (yearValue value)

arithmeticPersianLastDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer
arithmeticPersianLastDay {rule} =
  arithmeticPersianNewYear {rule} (maximumArithmeticPersianYear + 1) - 1

public export
maxArithmeticPersianDaysInMonth : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => PersianMonth -> Year -> DayOfMonth
maxArithmeticPersianDaysInMonth {rule} PersianMonths.Esfand value =
  if isArithmeticPersianLeapYear {rule} value then 30 else 29
maxArithmeticPersianDaysInMonth valueMonth _ =
  if PersianMonths.monthNumber valueMonth <= 6 then 31 else 30

public export
isValidArithmeticPersianDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayOfMonth -> PersianMonth -> Year -> Bool
isValidArithmeticPersianDate {rule} valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
      maxDay = dayOfMonthValue
        (maxArithmeticPersianDaysInMonth {rule} valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay &&
      yearNumber >= minimumArithmeticPersianYear &&
      yearNumber <= maximumArithmeticPersianYear

public export
arithmeticPersianDaysFromCivil : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> PersianMonth -> DayOfMonth -> Integer
arithmeticPersianDaysFromCivil {rule} valueYear valueMonth valueDay =
  arithmeticPersianNewYearDay {rule} valueYear + monthOffset valueMonth +
    dayOfMonthValue valueDay - 1

findArithmeticPersianYear : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Nat -> Integer -> Integer -> Integer
findArithmeticPersianYear {rule} Z estimate _ = estimate
findArithmeticPersianYear {rule} (S fuel) estimate days =
  if days < arithmeticPersianNewYear {rule} estimate
    then findArithmeticPersianYear {rule} fuel (estimate - 1) days
    else if days >= arithmeticPersianNewYear {rule} (estimate + 1)
      then findArithmeticPersianYear {rule} fuel (estimate + 1) days
      else estimate

arithmeticPersianCivilFromDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> (Year, PersianMonth, DayOfMonth)
arithmeticPersianCivilFromDays {rule} value =
  let epoch = arithmeticPersianEpoch {rule}
      estimate = max minimumArithmeticPersianYear
        (min maximumArithmeticPersianYear ((value - epoch) `div` 365 + 1))
      yearNumber = findArithmeticPersianYear {rule} 9377 estimate value
      valueYear = yearFromInteger yearNumber
      dayOfYear = value - arithmeticPersianNewYearDay {rule} valueYear
      monthNumber = if dayOfYear == 365 then 12
        else if dayOfYear < 186 then dayOfYear `div` 31 + 1
        else (dayOfYear - 186) `div` 30 + 7
      offset = if monthNumber <= 6 then (monthNumber - 1) * 31
        else 186 + (monthNumber - 7) * 30
   in (valueYear, monthFromNumber monthNumber,
       dayOfMonthFromInteger (dayOfYear - offset + 1))

public export
isValidArithmeticPersianDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> Bool
isValidArithmeticPersianDays {rule} value =
  value >= arithmeticPersianEpoch {rule} &&
  value <= arithmeticPersianLastDay {rule}

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  HasCalendarDate (ArithmeticPersianDate rule) where
  calendarDays = arithmeticDaysSinceEpoch
  acceptsCalendarDays = isValidArithmeticPersianDays {rule}
  calendarDateFromDays days = MkArithmeticPersianDate days
  calendarDateName = arithmeticPersianName {rule}

clampToArithmeticPersian : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> Integer
clampToArithmeticPersian {rule} =
  max (arithmeticPersianEpoch {rule}) . min (arithmeticPersianLastDay {rule})

shiftArithmeticPersianDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> ArithmeticPersianDate rule ->
  ArithmeticPersianDate rule
shiftArithmeticPersianDays {rule} amount date = MkArithmeticPersianDate
  (clampToArithmeticPersian {rule} (date.arithmeticDaysSinceEpoch + amount))

shiftArithmeticPersianMonths : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> ArithmeticPersianDate rule ->
  ArithmeticPersianDate rule
shiftArithmeticPersianMonths {rule} amount date =
  let (valueYear, valueMonth, valueDay) =
        arithmeticPersianCivilFromDays {rule} date.arithmeticDaysSinceEpoch
      monthOrdinal = PersianMonths.monthNumber valueMonth - 1 + amount
      targetYear = yearFromInteger (yearValue valueYear + monthOrdinal `div` 12)
      targetMonth = monthFromNumber (monthOrdinal `mod` 12 + 1)
      targetDay = min valueDay
        (maxArithmeticPersianDaysInMonth {rule} targetMonth targetYear)
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (arithmeticPersianDaysFromCivil {rule} targetYear targetMonth targetDay))

shiftArithmeticPersianYears : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> ArithmeticPersianDate rule ->
  ArithmeticPersianDate rule
shiftArithmeticPersianYears {rule} amount date =
  let (valueYear, valueMonth, valueDay) =
        arithmeticPersianCivilFromDays {rule} date.arithmeticDaysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay
        (maxArithmeticPersianDaysInMonth {rule} valueMonth targetYear)
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (arithmeticPersianDaysFromCivil {rule} targetYear valueMonth targetDay))

applyArithmeticPersianPeriod : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Period target -> ArithmeticPersianDate rule ->
  ArithmeticPersianDate rule
applyArithmeticPersianPeriod {rule} period =
    shiftArithmeticPersianDays {rule} (periodDays period)
  . shiftArithmeticPersianDays {rule} (7 * periodWeeks period)
  . shiftArithmeticPersianMonths {rule} (periodMonths period)
  . shiftArithmeticPersianYears {rule} (periodYears period)

arithmeticPersianDayOfWeek : ArithmeticPersianDate rule -> PersianDayOfWeek
arithmeticPersianDayOfWeek date =
  persianWeekdayFromDays date.arithmeticDaysSinceEpoch

nextArithmeticPersian : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> PersianDayOfWeek ->
  ArithmeticPersianDate rule -> ArithmeticPersianDate rule
nextArithmeticPersian {rule} count target date =
  let current = PersianWeekdays.weekdayNumber (arithmeticPersianDayOfWeek date)
      wanted = PersianWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (date.arithmeticDaysSinceEpoch + 7 * weeks + wanted - current))

previousArithmeticPersian : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> PersianDayOfWeek ->
  ArithmeticPersianDate rule -> ArithmeticPersianDate rule
previousArithmeticPersian {rule} count target date =
  let current = PersianWeekdays.weekdayNumber (arithmeticPersianDayOfWeek date)
      wanted = PersianWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (date.arithmeticDaysSinceEpoch - (7 * weeks + current - wanted)))

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  Calendar (ArithmeticPersian rule) where
  DateRep = ArithmeticPersianDate rule
  MonthRep _ = PersianMonth
  WeekdayRep = PersianDayOfWeek

  isValidDays = isValidArithmeticPersianDays {rule}
  fromDays days = MkArithmeticPersianDate days
  toDays date = date.arithmeticDaysSinceEpoch
  calendarName = arithmeticPersianName {rule}

  year' date = let (value, _, _) = arithmeticPersianCivilFromDays {rule}
                    date.arithmeticDaysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    arithmeticPersianCivilFromDays {rule}
                      date.arithmeticDaysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = arithmeticPersianCivilFromDays {rule}
                   date.arithmeticDaysSinceEpoch in value
  month' date = let (_, value, _) = arithmeticPersianCivilFromDays {rule}
                     date.arithmeticDaysSinceEpoch in value

  applyCalendarPeriod' = applyArithmeticPersianPeriod {rule}
  shiftCalendarDays' = shiftArithmeticPersianDays {rule}
  dayOfWeek = arithmeticPersianDayOfWeek
  next = nextArithmeticPersian {rule}
  previous = previousArithmeticPersian {rule}

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  Show (ArithmeticPersianDate rule) where
  show date = case arithmeticPersianCivilFromDays {rule}
    date.arithmeticDaysSinceEpoch of
      (valueYear, valueMonth, valueDay) =>
        arithmeticPersianConstructorName {rule} ++ " " ++ show valueDay ++
        " " ++ show valueMonth ++ " " ++ show valueYear

public export
HasCalendar (ArithmeticPersianDate rule) where
  calendarCapability = ()

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  ApplyPeriod (ArithmeticPersianDate rule) where
  applyPeriod = applyArithmeticPersianPeriod {rule}

||| Construct a statically validated Persian date under an arithmetic rule.
public export
arithmeticRulePersianDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
  (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticPersianDate {rule} valueDay valueMonth valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticRulePersianDate {rule} valueDay valueMonth valueYear =
  MkArithmeticPersianDate
    (arithmeticPersianDaysFromCivil {rule} valueYear valueMonth valueDay)

||| Construct a date in the legacy 33-year Persian cycle.
public export
simplePersianDate : (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
  (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticPersianDate {rule = Simple}
      valueDay valueMonth valueYear)} -> CalendarDate PersianSimple
simplePersianDate = arithmeticRulePersianDate {rule = Simple}

||| Construct a date in Birashk's 2820-year arithmetic Persian cycle.
public export
arithmeticPersianDate : (valueDay : DayOfMonth) ->
  (valueMonth : PersianMonth) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticPersianDate {rule = Birashk}
      valueDay valueMonth valueYear)} -> CalendarDate PersianArithmetic
arithmeticPersianDate = arithmeticRulePersianDate {rule = Birashk}

||| Validate runtime components under a selected arithmetic Persian rule.
public export
refineArithmeticRulePersianDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticRulePersianDate {rule} valueDay valueMonth valueYear =
  case choose (isValidArithmeticPersianDate {rule}
    valueDay valueMonth valueYear) of
      Left valid => Right (arithmeticRulePersianDate {rule}
        valueDay valueMonth valueYear {valid = valid})
      Right _ => Left (InvalidPersianDate valueDay valueMonth valueYear)

public export
refineSimplePersianDate : DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate PersianSimple)
refineSimplePersianDate = refineArithmeticRulePersianDate {rule = Simple}

public export
refineArithmeticPersianDate : DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate PersianArithmetic)
refineArithmeticPersianDate = refineArithmeticRulePersianDate {rule = Birashk}

||| Validate a day count under a selected arithmetic Persian rule.
public export
refineArithmeticPersianDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticPersianDays {rule} days =
  case choose (isValidArithmeticPersianDays {rule} days) of
    Left valid => Right (fromDays {calendar = ArithmeticPersian rule}
      days {valid = valid})
    Right _ => Left (InvalidPersianDayCount days)

arithmeticPersianNthDayOfMonth : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year -> DayOfMonth
arithmeticPersianNthDayOfMonth {rule} nth target valueMonth valueYear =
  let monthLength = maxArithmeticPersianDaysInMonth {rule} valueMonth valueYear
      firstOffset = (PersianWeekdays.weekdayNumber target -
        PersianWeekdays.weekdayNumber (persianWeekdayFromDays
          (arithmeticPersianDaysFromCivil {rule} valueYear valueMonth 1))) `mod` 7
      lastOffset = (PersianWeekdays.weekdayNumber (persianWeekdayFromDays
        (arithmeticPersianDaysFromCivil {rule}
          valueYear valueMonth monthLength)) -
        PersianWeekdays.weekdayNumber target) `mod` 7
      dayNumber = case nth of
        FourthToLast => dayOfMonthValue monthLength - lastOffset - 21
        ThirdToLast => dayOfMonthValue monthLength - lastOffset - 14
        SecondToLast => dayOfMonthValue monthLength - lastOffset - 7
        Last => dayOfMonthValue monthLength - lastOffset
        First => 1 + firstOffset
        Second => 8 + firstOffset
        Third => 15 + firstOffset
        Fourth => 22 + firstOffset
        Fifth => 29 + firstOffset
   in dayOfMonthFromInteger dayNumber

isValidArithmeticPersianNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year -> Bool
isValidArithmeticPersianNthDay {rule} nth target valueMonth valueYear =
  yearValue valueYear >= minimumArithmeticPersianYear &&
  yearValue valueYear <= maximumArithmeticPersianYear && case nth of
    Fifth => arithmeticPersianNthDayOfMonth {rule}
      nth target valueMonth valueYear <=
      maxArithmeticPersianDaysInMonth {rule} valueMonth valueYear
    _ => True

||| Construct an nth weekday under a selected arithmetic Persian rule.
public export
arithmeticPersianFromNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (nth : DayNth) -> (target : PersianDayOfWeek) ->
  (valueMonth : PersianMonth) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticPersianNthDay {rule}
      nth target valueMonth valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticPersianFromNthDay {rule} nth target valueMonth valueYear =
  MkArithmeticPersianDate (arithmeticPersianDaysFromCivil {rule}
    valueYear valueMonth
    (arithmeticPersianNthDayOfMonth {rule} nth target valueMonth valueYear))

||| Validate an nth-weekday request under an arithmetic Persian rule.
public export
refineArithmeticPersianNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticPersianNthDay {rule} nth target valueMonth valueYear =
  case choose (isValidArithmeticPersianNthDay {rule}
    nth target valueMonth valueYear) of
      Left valid => Right (arithmeticPersianFromNthDay {rule}
        nth target valueMonth valueYear {valid = valid})
      Right _ => Left (InvalidPersianNthDay nth target valueMonth valueYear)

arithmeticPersianWeekDateDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek ->
  Year -> Integer
arithmeticPersianWeekDateDays {rule} week target valueYear =
  let firstDay = arithmeticPersianDaysFromCivil {rule}
        valueYear PersianMonths.Farvardin 1
      firstWeekStart = firstDay -
        ((PersianWeekdays.weekdayNumber (persianWeekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (PersianWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

isValidArithmeticPersianWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek ->
  Year -> Bool
isValidArithmeticPersianWeekDate {rule} week target valueYear =
  let days = arithmeticPersianWeekDateDays {rule} week target valueYear
   in yearValue valueYear >= minimumArithmeticPersianYear &&
      yearValue valueYear <= maximumArithmeticPersianYear &&
      isValidArithmeticPersianDays {rule} days

||| Construct a Saturday-based week date under an arithmetic Persian rule.
public export
arithmeticPersianFromWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (week : WeekNumber) -> (target : PersianDayOfWeek) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticPersianWeekDate {rule} week target valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticPersianFromWeekDate {rule} week target valueYear =
  MkArithmeticPersianDate
    (arithmeticPersianWeekDateDays {rule} week target valueYear)

||| Validate a Saturday-based week date under an arithmetic Persian rule.
public export
refineArithmeticPersianWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticPersianWeekDate {rule} week target valueYear =
  case choose (isValidArithmeticPersianWeekDate {rule}
    week target valueYear) of
      Left valid => Right (arithmeticPersianFromWeekDate {rule}
        week target valueYear {valid = valid})
      Right _ => Left (InvalidPersianWeekDate week target valueYear)
