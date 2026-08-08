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
weekdayFromDays : Integer -> PersianDayOfWeek
weekdayFromDays value = case (value + 3) `mod` 7 of
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
minimumYear : Integer
minimumYear = 1

public export
maximumYear : Integer
maximumYear = 1500

public export
epoch : Integer
epoch = -503284

public export
leapYears : List Integer
leapYears =
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
isLeapYear : Year -> Bool
isLeapYear value = elem (yearValue value) leapYears

public export
countLeapsBefore : Integer -> List Integer -> Integer
countLeapsBefore _ [] = 0
countLeapsBefore year (leapYear :: rest) =
  if leapYear < year then 1 + countLeapsBefore year rest else 0

public export
newYearDay : Year -> Integer
newYearDay value =
  epoch + (yearValue value - 1) * 365 +
    countLeapsBefore (yearValue value) leapYears

public export
lastDay : Integer
lastDay = newYearDay 1501 - 1

public export
maxDaysInMonth : PersianMonth -> Year -> DayOfMonth
maxDaysInMonth PersianMonths.Esfand value =
  if isLeapYear value then 30 else 29
maxDaysInMonth valueMonth _ =
  if PersianMonths.monthNumber valueMonth <= 6 then 31 else 30

public export
isValidDate : DayOfMonth -> PersianMonth -> Year -> Bool
isValidDate valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
      maxDay = dayOfMonthValue (maxDaysInMonth valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay &&
      yearNumber >= minimumYear && yearNumber <= maximumYear

public export
monthOffset : PersianMonth -> Integer
monthOffset value =
  let number = PersianMonths.monthNumber value
   in if number <= 6 then (number - 1) * 31
      else 186 + (number - 7) * 30

public export
daysFromCivil : Year -> PersianMonth -> DayOfMonth -> Integer
daysFromCivil valueYear valueMonth valueDay =
  newYearDay valueYear + monthOffset valueMonth +
    dayOfMonthValue valueDay - 1

findPersianYear : Nat -> Integer -> Integer -> Integer
findPersianYear Z estimate days = estimate
findPersianYear (S fuel) estimate days =
  if days < newYearDay (yearFromInteger estimate)
    then findPersianYear fuel (estimate - 1) days
    else if days >= newYearDay (yearFromInteger (estimate + 1))
      then findPersianYear fuel (estimate + 1) days
      else estimate

persianCivilFromDays : Integer -> (Year, PersianMonth, DayOfMonth)
persianCivilFromDays value =
  let estimate = max minimumYear (min maximumYear
        ((value - epoch) `div` 365 + 1))
      yearNumber = findPersianYear 1500 estimate value
      valueYear = yearFromInteger yearNumber
      dayOfYear = value - newYearDay valueYear
      monthNumber = if dayOfYear == 365 then 12
        else if dayOfYear < 186 then dayOfYear `div` 31 + 1
        else (dayOfYear - 186) `div` 30 + 7
      offset = if monthNumber <= 6 then (monthNumber - 1) * 31
        else 186 + (monthNumber - 7) * 30
      dayNumber = dayOfYear - offset + 1
   in (valueYear, monthFromNumber monthNumber,
       dayOfMonthFromInteger dayNumber)

public export
HasCalendarDate PersianDate where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays value = value >= epoch && value <= lastDay
  calendarDateFromDays days = MkPersianDate days
  calendarDateName = "Persian"

clampToPersian : Integer -> Integer
clampToPersian = max epoch . min lastDay

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
      targetDay = min valueDay (maxDaysInMonth targetMonth targetYear)
   in MkPersianDate (clampToPersian
        (daysFromCivil targetYear targetMonth targetDay))

shiftPersianYears : Integer -> PersianDate -> PersianDate
shiftPersianYears amount date =
  let (valueYear, valueMonth, valueDay) =
        persianCivilFromDays date.daysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay (maxDaysInMonth valueMonth targetYear)
   in MkPersianDate (clampToPersian
        (daysFromCivil targetYear valueMonth targetDay))

applyPersianPeriod : Period target -> PersianDate -> PersianDate
applyPersianPeriod period =
    shiftPersianDays (periodDays period)
  . shiftPersianDays (7 * periodWeeks period)
  . shiftPersianMonths (periodMonths period)
  . shiftPersianYears (periodYears period)

persianDayOfWeek : PersianDate -> PersianDayOfWeek
persianDayOfWeek date = weekdayFromDays date.daysSinceEpoch

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

  isValidDays value = value >= epoch && value <= lastDay
  fromDays days = MkPersianDate days
  toDaysFor date = date.daysSinceEpoch
  calendarName = "Persian"

  year' date = let (value, _, _) = persianCivilFromDays date.daysSinceEpoch in value
  toYmd date = let (_, valueMonth, valueDay) =
                    persianCivilFromDays date.daysSinceEpoch
                in (valueMonth, valueDay)
  day' date = let (_, _, value) = persianCivilFromDays date.daysSinceEpoch in value
  month' date = let (_, value, _) = persianCivilFromDays date.daysSinceEpoch in value

  applyCalendarPeriod' = applyPersianPeriod
  shiftCalendarDays' = shiftPersianDays

  dayOfWeekFor = persianDayOfWeek
  next = nextPersian
  previous = previousPersian

public export
Show PersianDate where
  show date = case persianCivilFromDays date.daysSinceEpoch of
    (valueYear, valueMonth, valueDay) =>
      "calendarDate " ++ show valueDay ++ " " ++
      show valueMonth ++ " " ++ show valueYear

public export
HasCalendar PersianDate where
  calendarCapability = ()

public export
ApplyPeriod PersianDate where
  applyPeriod = applyPersianPeriod

public export
CalendarValue PersianDate where
  CalendarMonth _ = PersianMonth
  CalendarWeekday = PersianDayOfWeek
  calendarValueToDays = toDaysFor {calendar = Persian}
  calendarValueYear = yearFor {calendar = Persian}
  calendarValueMonth = monthFor {calendar = Persian}
  calendarValueDay = dayFor {calendar = Persian}
  calendarValueDayOfWeek = dayOfWeekFor {calendar = Persian}
  calendarValueBetweenWith = betweenWithFor {calendar = Persian}

||| Construct a statically validated Persian date in years 1-1500.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
              (valueYear : Year) ->
              {auto 0 valid : So
                (isValidDate valueDay valueMonth valueYear)} ->
              CalendarDate Persian
calendarDate valueDay valueMonth valueYear =
  MkPersianDate (daysFromCivil valueYear valueMonth valueDay)

||| Failures produced while refining untrusted Persian date data.
public export
data PersianDateError
  = InvalidPersianDate DayOfMonth PersianMonth Year
  | InvalidPersianDayCount Integer
  | InvalidPersianNthDay DayNth PersianDayOfWeek PersianMonth Year
  | InvalidPersianWeekDate WeekNumber PersianDayOfWeek Year

||| Validate runtime day, month, and year components as a Persian date.
public export
refineDate : DayOfMonth -> PersianMonth -> Year ->
                    Either PersianDateError (CalendarDate Persian)
refineDate valueDay valueMonth valueYear =
  case choose (isValidDate valueDay valueMonth valueYear) of
    Left valid => Right (calendarDate valueDay valueMonth valueYear @{valid})
    Right _ => Left (InvalidPersianDate valueDay valueMonth valueYear)

||| Construct a Persian date from a statically valid calendar-relative day count.
public export
fromDays : (days : Integer) ->
                  {auto 0 valid : So
                    (IotaTime.Calendar.isValidDays {calendar = Persian} days)} ->
                  CalendarDate Persian
fromDays days = MkPersianDate days

||| Validate a runtime Persian day count within the supported year range.
public export
refineDays : Integer -> Either PersianDateError (CalendarDate Persian)
refineDays days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Persian} days) of
  Left valid => Right (fromDays days @{valid})
  Right _ => Left (InvalidPersianDayCount days)

public export
nthDayOfMonth : DayNth -> PersianDayOfWeek -> PersianMonth -> Year ->
                       DayOfMonth
nthDayOfMonth nth target valueMonth valueYear =
  let monthLength = maxDaysInMonth valueMonth valueYear
      firstOffset = (PersianWeekdays.weekdayNumber target -
        PersianWeekdays.weekdayNumber (weekdayFromDays
          (daysFromCivil valueYear valueMonth 1))) `mod` daysPerWeek
      lastOffset = (PersianWeekdays.weekdayNumber (weekdayFromDays
        (daysFromCivil valueYear valueMonth monthLength)) -
        PersianWeekdays.weekdayNumber target) `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

public export
isValidNthDay : DayNth -> PersianDayOfWeek -> PersianMonth -> Year -> Bool
isValidNthDay nth target valueMonth valueYear =
  yearValue valueYear >= minimumYear &&
  yearValue valueYear <= maximumYear && case nth of
    Fifth => nthDayOfMonth nth target valueMonth valueYear <=
      maxDaysInMonth valueMonth valueYear
    _ => True

||| Construct the nth requested weekday in a Persian month.
public export
fromNthDay : (nth : DayNth) -> (target : PersianDayOfWeek) ->
                    (valueMonth : PersianMonth) -> (valueYear : Year) ->
                    {auto 0 valid : So
                      (isValidNthDay nth target valueMonth valueYear)} ->
                    CalendarDate Persian
fromNthDay nth target valueMonth valueYear =
  MkPersianDate (daysFromCivil valueYear valueMonth
    (nthDayOfMonth nth target valueMonth valueYear))

||| Validate an nth-weekday request for a Persian month.
public export
refineNthDay : DayNth -> PersianDayOfWeek -> PersianMonth -> Year ->
                      Either PersianDateError (CalendarDate Persian)
refineNthDay nth target valueMonth valueYear =
  case choose (isValidNthDay nth target valueMonth valueYear) of
    Left valid => Right
      (fromNthDay nth target valueMonth valueYear @{valid})
    Right _ => Left (InvalidPersianNthDay nth target valueMonth valueYear)

public export
weekDateDays : WeekNumber -> PersianDayOfWeek -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = daysFromCivil valueYear PersianMonths.Farvardin 1
      firstWeekStart = firstDay -
        ((PersianWeekdays.weekdayNumber (weekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (PersianWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

public export
isValidWeekDate : WeekNumber -> PersianDayOfWeek -> Year -> Bool
isValidWeekDate week target valueYear =
  let days = weekDateDays week target valueYear
   in yearValue valueYear >= minimumYear &&
      yearValue valueYear <= maximumYear &&
      IotaTime.Calendar.isValidDays {calendar = Persian} days

||| Construct a Persian Saturday-based week date under static validity evidence.
public export
fromWeekDate : (week : WeekNumber) -> (target : PersianDayOfWeek) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
               CalendarDate Persian
fromWeekDate week target valueYear =
  MkPersianDate (weekDateDays week target valueYear)

||| Validate a runtime Persian Saturday-based week date.
public export
refineWeekDate : WeekNumber -> PersianDayOfWeek -> Year ->
                 Either PersianDateError (CalendarDate Persian)
refineWeekDate week target valueYear =
  case choose (isValidWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate week target valueYear @{valid})
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
  ruleName : String
  ruleConstructorName : String
  ruleEpoch : Integer
  ruleIsLeapYear : Integer -> Bool
  ruleNewYearDay : Integer -> Integer

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
  ruleName = "Persian Simple"
  ruleConstructorName = "simpleCalendarDate"
  ruleEpoch = -503285
  ruleIsLeapYear year = simpleNewYear (year + 1) - simpleNewYear year == 366
  ruleNewYearDay = simpleNewYear

public export
KnownPersianArithmeticRule Birashk where
  ruleName = "Persian Arithmetic"
  ruleConstructorName = "arithmeticCalendarDate"
  ruleEpoch = -503284
  ruleIsLeapYear year =
    let cycleYear = (year - 474) `mod` 2820 + 474
     in ((cycleYear + 38) * 31) `mod` 128 < 31
  ruleNewYearDay = arithmeticNewYear

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
minimumArithmeticYear : Integer
minimumArithmeticYear = 1

public export
maximumArithmeticYear : Integer
maximumArithmeticYear = 9377

public export
isArithmeticLeapYear : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> Bool
isArithmeticLeapYear {rule} value =
  ruleIsLeapYear {rule} (yearValue value)

public export
arithmeticNewYearDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> Integer
arithmeticNewYearDay {rule} value =
  ruleNewYearDay {rule} (yearValue value)

||| The final supported day under the selected arithmetic Persian rule.
public export
arithmeticLastDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer
arithmeticLastDay {rule} =
  ruleNewYearDay {rule} (maximumArithmeticYear + 1) - 1

public export
maxArithmeticDaysInMonth : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => PersianMonth -> Year -> DayOfMonth
maxArithmeticDaysInMonth {rule} PersianMonths.Esfand value =
  if isArithmeticLeapYear {rule} value then 30 else 29
maxArithmeticDaysInMonth valueMonth _ =
  if PersianMonths.monthNumber valueMonth <= 6 then 31 else 30

public export
isValidArithmeticDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayOfMonth -> PersianMonth -> Year -> Bool
isValidArithmeticDate {rule} valueDay valueMonth valueYear =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
      maxDay = dayOfMonthValue
        (maxArithmeticDaysInMonth {rule} valueMonth valueYear)
   in dayNumber >= 1 && dayNumber <= maxDay &&
      yearNumber >= minimumArithmeticYear &&
      yearNumber <= maximumArithmeticYear

public export
arithmeticDaysFromCivil : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Year -> PersianMonth -> DayOfMonth -> Integer
arithmeticDaysFromCivil {rule} valueYear valueMonth valueDay =
  arithmeticNewYearDay {rule} valueYear + monthOffset valueMonth +
    dayOfMonthValue valueDay - 1

findArithmeticPersianYear : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Nat -> Integer -> Integer -> Integer
findArithmeticPersianYear {rule} Z estimate _ = estimate
findArithmeticPersianYear {rule} (S fuel) estimate days =
  if days < ruleNewYearDay {rule} estimate
    then findArithmeticPersianYear {rule} fuel (estimate - 1) days
    else if days >= ruleNewYearDay {rule} (estimate + 1)
      then findArithmeticPersianYear {rule} fuel (estimate + 1) days
      else estimate

arithmeticPersianCivilFromDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> (Year, PersianMonth, DayOfMonth)
arithmeticPersianCivilFromDays {rule} value =
  let epoch = ruleEpoch {rule}
      estimate = max minimumArithmeticYear
        (min maximumArithmeticYear ((value - epoch) `div` 365 + 1))
      yearNumber = findArithmeticPersianYear {rule} 9377 estimate value
      valueYear = yearFromInteger yearNumber
      dayOfYear = value - arithmeticNewYearDay {rule} valueYear
      monthNumber = if dayOfYear == 365 then 12
        else if dayOfYear < 186 then dayOfYear `div` 31 + 1
        else (dayOfYear - 186) `div` 30 + 7
      offset = if monthNumber <= 6 then (monthNumber - 1) * 31
        else 186 + (monthNumber - 7) * 30
   in (valueYear, monthFromNumber monthNumber,
       dayOfMonthFromInteger (dayOfYear - offset + 1))

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  HasCalendarDate (ArithmeticPersianDate rule) where
  calendarDays = arithmeticDaysSinceEpoch
  acceptsCalendarDays value = value >= ruleEpoch {rule} &&
    value <= arithmeticLastDay {rule}
  calendarDateFromDays days = MkArithmeticPersianDate days
  calendarDateName = ruleName {rule}

clampToArithmeticPersian : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> Integer
clampToArithmeticPersian {rule} =
  max (ruleEpoch {rule}) . min (arithmeticLastDay {rule})

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
        (maxArithmeticDaysInMonth {rule} targetMonth targetYear)
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (arithmeticDaysFromCivil {rule} targetYear targetMonth targetDay))

shiftArithmeticPersianYears : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer -> ArithmeticPersianDate rule ->
  ArithmeticPersianDate rule
shiftArithmeticPersianYears {rule} amount date =
  let (valueYear, valueMonth, valueDay) =
        arithmeticPersianCivilFromDays {rule} date.arithmeticDaysSinceEpoch
      targetYear = yearFromInteger (yearValue valueYear + amount)
      targetDay = min valueDay
        (maxArithmeticDaysInMonth {rule} valueMonth targetYear)
   in MkArithmeticPersianDate (clampToArithmeticPersian {rule}
        (arithmeticDaysFromCivil {rule} targetYear valueMonth targetDay))

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
  weekdayFromDays date.arithmeticDaysSinceEpoch

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

  isValidDays value = value >= ruleEpoch {rule} &&
    value <= arithmeticLastDay {rule}
  fromDays days = MkArithmeticPersianDate days
  toDaysFor date = date.arithmeticDaysSinceEpoch
  calendarName = ruleName {rule}

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
  dayOfWeekFor = arithmeticPersianDayOfWeek
  next = nextArithmeticPersian {rule}
  previous = previousArithmeticPersian {rule}

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  Show (ArithmeticPersianDate rule) where
  show date = case arithmeticPersianCivilFromDays {rule}
    date.arithmeticDaysSinceEpoch of
      (valueYear, valueMonth, valueDay) =>
        ruleConstructorName {rule} ++ " " ++ show valueDay ++
        " " ++ show valueMonth ++ " " ++ show valueYear

public export
HasCalendar (ArithmeticPersianDate rule) where
  calendarCapability = ()

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  ApplyPeriod (ArithmeticPersianDate rule) where
  applyPeriod = applyArithmeticPersianPeriod {rule}

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  CalendarValue (ArithmeticPersianDate rule) where
  CalendarMonth _ = PersianMonth
  CalendarWeekday = PersianDayOfWeek
  calendarValueToDays = toDaysFor {calendar = ArithmeticPersian rule}
  calendarValueYear = yearFor {calendar = ArithmeticPersian rule}
  calendarValueMonth = monthFor {calendar = ArithmeticPersian rule}
  calendarValueDay = dayFor {calendar = ArithmeticPersian rule}
  calendarValueDayOfWeek = dayOfWeekFor {calendar = ArithmeticPersian rule}
  calendarValueBetweenWith =
    betweenWithFor {calendar = ArithmeticPersian rule}

||| Construct a statically validated Persian date under an arithmetic rule.
public export
arithmeticRuleCalendarDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
  (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticDate {rule} valueDay valueMonth valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticRuleCalendarDate {rule} valueDay valueMonth valueYear =
  MkArithmeticPersianDate
    (arithmeticDaysFromCivil {rule} valueYear valueMonth valueDay)

||| Construct a date in the legacy 33-year Persian cycle.
public export
simpleCalendarDate : (valueDay : DayOfMonth) -> (valueMonth : PersianMonth) ->
  (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticDate {rule = Simple}
      valueDay valueMonth valueYear)} -> CalendarDate PersianSimple
simpleCalendarDate = arithmeticRuleCalendarDate {rule = Simple}

||| Construct a date in Birashk's 2820-year arithmetic Persian cycle.
public export
arithmeticCalendarDate : (valueDay : DayOfMonth) ->
  (valueMonth : PersianMonth) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticDate {rule = Birashk}
      valueDay valueMonth valueYear)} -> CalendarDate PersianArithmetic
arithmeticCalendarDate = arithmeticRuleCalendarDate {rule = Birashk}

||| Validate runtime components under a selected arithmetic Persian rule.
public export
refineArithmeticRuleDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticRuleDate {rule} valueDay valueMonth valueYear =
  case choose (isValidArithmeticDate {rule}
    valueDay valueMonth valueYear) of
      Left valid => Right (arithmeticRuleCalendarDate {rule}
        valueDay valueMonth valueYear {valid = valid})
      Right _ => Left (InvalidPersianDate valueDay valueMonth valueYear)

public export
refineSimpleDate : DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate PersianSimple)
refineSimpleDate = refineArithmeticRuleDate {rule = Simple}

public export
refineArithmeticDate : DayOfMonth -> PersianMonth -> Year ->
  Either PersianDateError (CalendarDate PersianArithmetic)
refineArithmeticDate = refineArithmeticRuleDate {rule = Birashk}

||| Validate a day count under a selected arithmetic Persian rule.
public export
refineArithmeticDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => Integer ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticDays {rule} days =
  case choose
    (IotaTime.Calendar.isValidDays
      {calendar = ArithmeticPersian rule} days) of
    Left valid => Right (fromDays {calendar = ArithmeticPersian rule}
      days {valid = valid})
    Right _ => Left (InvalidPersianDayCount days)

arithmeticNthDayOfMonth : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year -> DayOfMonth
arithmeticNthDayOfMonth {rule} nth target valueMonth valueYear =
  let monthLength = maxArithmeticDaysInMonth {rule} valueMonth valueYear
      firstOffset = (PersianWeekdays.weekdayNumber target -
        PersianWeekdays.weekdayNumber (weekdayFromDays
          (arithmeticDaysFromCivil {rule} valueYear valueMonth 1)))
            `mod` daysPerWeek
      lastOffset = (PersianWeekdays.weekdayNumber (weekdayFromDays
        (arithmeticDaysFromCivil {rule}
          valueYear valueMonth monthLength)) -
        PersianWeekdays.weekdayNumber target) `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

isValidArithmeticNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year -> Bool
isValidArithmeticNthDay {rule} nth target valueMonth valueYear =
  yearValue valueYear >= minimumArithmeticYear &&
  yearValue valueYear <= maximumArithmeticYear && case nth of
    Fifth => arithmeticNthDayOfMonth {rule}
      nth target valueMonth valueYear <=
      maxArithmeticDaysInMonth {rule} valueMonth valueYear
    _ => True

||| Construct an nth weekday under a selected arithmetic Persian rule.
public export
arithmeticFromNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (nth : DayNth) -> (target : PersianDayOfWeek) ->
  (valueMonth : PersianMonth) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticNthDay {rule}
      nth target valueMonth valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticFromNthDay {rule} nth target valueMonth valueYear =
  MkArithmeticPersianDate (arithmeticDaysFromCivil {rule}
    valueYear valueMonth
    (arithmeticNthDayOfMonth {rule} nth target valueMonth valueYear))

||| Validate an nth-weekday request under an arithmetic Persian rule.
public export
refineArithmeticNthDay : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => DayNth -> PersianDayOfWeek ->
  PersianMonth -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticNthDay {rule} nth target valueMonth valueYear =
  case choose (isValidArithmeticNthDay {rule}
    nth target valueMonth valueYear) of
      Left valid => Right (arithmeticFromNthDay {rule}
        nth target valueMonth valueYear {valid = valid})
      Right _ => Left (InvalidPersianNthDay nth target valueMonth valueYear)

arithmeticWeekDateDays : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek ->
  Year -> Integer
arithmeticWeekDateDays {rule} week target valueYear =
  let firstDay = arithmeticDaysFromCivil {rule}
        valueYear PersianMonths.Farvardin 1
      firstWeekStart = firstDay -
        ((PersianWeekdays.weekdayNumber (weekdayFromDays firstDay) - 6)
          `mod` 7)
      targetOffset = (PersianWeekdays.weekdayNumber target - 6) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) + targetOffset

isValidArithmeticWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek ->
  Year -> Bool
isValidArithmeticWeekDate {rule} week target valueYear =
  let days = arithmeticWeekDateDays {rule} week target valueYear
   in yearValue valueYear >= minimumArithmeticYear &&
      yearValue valueYear <= maximumArithmeticYear &&
      IotaTime.Calendar.isValidDays
        {calendar = ArithmeticPersian rule} days

||| Construct a Saturday-based week date under an arithmetic Persian rule.
public export
arithmeticFromWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule =>
  (week : WeekNumber) -> (target : PersianDayOfWeek) -> (valueYear : Year) ->
  {auto 0 valid : So
    (isValidArithmeticWeekDate {rule} week target valueYear)} ->
  CalendarDate (ArithmeticPersian rule)
arithmeticFromWeekDate {rule} week target valueYear =
  MkArithmeticPersianDate
    (arithmeticWeekDateDays {rule} week target valueYear)

||| Validate a Saturday-based week date under an arithmetic Persian rule.
public export
refineArithmeticWeekDate : {rule : PersianArithmeticRule} ->
  KnownPersianArithmeticRule rule => WeekNumber -> PersianDayOfWeek -> Year ->
  Either PersianDateError (CalendarDate (ArithmeticPersian rule))
refineArithmeticWeekDate {rule} week target valueYear =
  case choose (isValidArithmeticWeekDate {rule}
    week target valueYear) of
      Left valid => Right (arithmeticFromWeekDate {rule}
        week target valueYear {valid = valid})
      Right _ => Left (InvalidPersianWeekDate week target valueYear)
