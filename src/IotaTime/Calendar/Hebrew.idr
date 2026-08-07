module IotaTime.Calendar.Hebrew

import IotaTime.Calendar
import IotaTime.Period
import Data.So

%default total

||| Hebrew month numbering: civil begins at Tishri, scriptural at Nisan.
public export
data HebrewNumbering = Civil | Scriptural

||| Evidence exposing the starting month for a Hebrew numbering system.
public export
interface KnownHebrewNumbering (numbering : HebrewNumbering) where
  numberingStart : Integer

public export
KnownHebrewNumbering Civil where
  numberingStart = 0

public export
KnownHebrewNumbering Scriptural where
  numberingStart = 7

||| The Hebrew calendar indexed by its month-numbering convention.
public export
data Hebrew : HebrewNumbering -> Type where
  HebrewCalendar : Hebrew numbering

public export
HebrewCivil : Type
HebrewCivil = Hebrew Civil

public export
HebrewScriptural : Type
HebrewScriptural = Hebrew Scriptural

public export
total
isHebrewLeapYear : Year -> Bool
isHebrewLeapYear value = (7 * yearValue value + 1) `mod` 19 < 7

namespace HebrewMonths
  public export
  data HebrewMonth : HebrewNumbering -> Year -> Type where
    Tishri : {numbering : HebrewNumbering} -> {valueYear : Year} ->
       HebrewMonth numbering valueYear
    Cheshvan : {numbering : HebrewNumbering} -> {valueYear : Year} ->
         HebrewMonth numbering valueYear
    Kislev : {numbering : HebrewNumbering} -> {valueYear : Year} ->
       HebrewMonth numbering valueYear
    Tevet : {numbering : HebrewNumbering} -> {valueYear : Year} ->
      HebrewMonth numbering valueYear
    Shevat : {numbering : HebrewNumbering} -> {valueYear : Year} ->
       HebrewMonth numbering valueYear
    AdarI : {numbering : HebrewNumbering} -> {valueYear : Year} ->
      {auto 0 leap : So (isHebrewLeapYear valueYear)} ->
      HebrewMonth numbering valueYear
    Adar : {numbering : HebrewNumbering} -> {valueYear : Year} ->
     HebrewMonth numbering valueYear
    Nisan : {numbering : HebrewNumbering} -> {valueYear : Year} ->
      HebrewMonth numbering valueYear
    Iyar : {numbering : HebrewNumbering} -> {valueYear : Year} ->
     HebrewMonth numbering valueYear
    Sivan : {numbering : HebrewNumbering} -> {valueYear : Year} ->
      HebrewMonth numbering valueYear
    Tammuz : {numbering : HebrewNumbering} -> {valueYear : Year} ->
       HebrewMonth numbering valueYear
    Av : {numbering : HebrewNumbering} -> {valueYear : Year} ->
   HebrewMonth numbering valueYear
    Elul : {numbering : HebrewNumbering} -> {valueYear : Year} ->
     HebrewMonth numbering valueYear

  public export
  calendarIndex : {numbering : HebrewNumbering} -> {valueYear : Year} ->
      HebrewMonth numbering valueYear -> Integer
  calendarIndex Tishri = 0
  calendarIndex Cheshvan = 1
  calendarIndex Kislev = 2
  calendarIndex Tevet = 3
  calendarIndex Shevat = 4
  calendarIndex AdarI = 5
  calendarIndex Adar = 6
  calendarIndex Nisan = 7
  calendarIndex Iyar = 8
  calendarIndex Sivan = 9
  calendarIndex Tammuz = 10
  calendarIndex Av = 11
  calendarIndex Elul = 12

  public export
  {numbering : HebrewNumbering} -> {valueYear : Year} ->
    Eq (HebrewMonth numbering valueYear) where
    left == right = calendarIndex left == calendarIndex right

  public export
  {numbering : HebrewNumbering} -> {valueYear : Year} ->
    Ord (HebrewMonth numbering valueYear) where
    compare left right = compare (calendarIndex left) (calendarIndex right)

  export
  showMonth : HebrewMonth numbering valueYear -> String
  showMonth Tishri = "Tishri"
  showMonth Cheshvan = "Cheshvan"
  showMonth Kislev = "Kislev"
  showMonth Tevet = "Tevet"
  showMonth Shevat = "Shevat"
  showMonth AdarI = "AdarI"
  showMonth Adar = "Adar"
  showMonth Nisan = "Nisan"
  showMonth Iyar = "Iyar"
  showMonth Sivan = "Sivan"
  showMonth Tammuz = "Tammuz"
  showMonth Av = "Av"
  showMonth Elul = "Elul"

  public export
  {numbering : HebrewNumbering} -> {valueYear : Year} ->
    Show (HebrewMonth numbering valueYear) where
    show = showMonth

||| A non-dependent Hebrew month name used at runtime refinement boundaries.
public export
data HebrewMonthName
  = TishriName | CheshvanName | KislevName | TevetName | ShevatName
  | AdarIName | AdarName | NisanName | IyarName | SivanName
  | TammuzName | AvName | ElulName

public export
Eq HebrewMonthName where
  TishriName == TishriName = True
  CheshvanName == CheshvanName = True
  KislevName == KislevName = True
  TevetName == TevetName = True
  ShevatName == ShevatName = True
  AdarIName == AdarIName = True
  AdarName == AdarName = True
  NisanName == NisanName = True
  IyarName == IyarName = True
  SivanName == SivanName = True
  TammuzName == TammuzName = True
  AvName == AvName = True
  ElulName == ElulName = True
  _ == _ = False

public export
monthName : {numbering : HebrewNumbering} -> {valueYear : Year} ->
            HebrewMonth numbering valueYear -> HebrewMonthName
monthName HebrewMonths.Tishri = TishriName
monthName HebrewMonths.Cheshvan = CheshvanName
monthName HebrewMonths.Kislev = KislevName
monthName HebrewMonths.Tevet = TevetName
monthName HebrewMonths.Shevat = ShevatName
monthName HebrewMonths.AdarI = AdarIName
monthName HebrewMonths.Adar = AdarName
monthName HebrewMonths.Nisan = NisanName
monthName HebrewMonths.Iyar = IyarName
monthName HebrewMonths.Sivan = SivanName
monthName HebrewMonths.Tammuz = TammuzName
monthName HebrewMonths.Av = AvName
monthName HebrewMonths.Elul = ElulName

namespace HebrewWeekdays
  public export
  data HebrewDayOfWeek : HebrewNumbering -> Type where
    Sunday : HebrewDayOfWeek numbering
    Monday : HebrewDayOfWeek numbering
    Tuesday : HebrewDayOfWeek numbering
    Wednesday : HebrewDayOfWeek numbering
    Thursday : HebrewDayOfWeek numbering
    Friday : HebrewDayOfWeek numbering
    Saturday : HebrewDayOfWeek numbering

  public export
  weekdayNumber : HebrewDayOfWeek numbering -> Integer
  weekdayNumber Sunday = 0
  weekdayNumber Monday = 1
  weekdayNumber Tuesday = 2
  weekdayNumber Wednesday = 3
  weekdayNumber Thursday = 4
  weekdayNumber Friday = 5
  weekdayNumber Saturday = 6

  public export
  Eq (HebrewDayOfWeek numbering) where
    left == right = weekdayNumber left == weekdayNumber right

  public export
  Ord (HebrewDayOfWeek numbering) where
    compare left right = compare (weekdayNumber left) (weekdayNumber right)

  public export
  Show (HebrewDayOfWeek numbering) where
    show Sunday = "Sunday"
    show Monday = "Monday"
    show Tuesday = "Tuesday"
    show Wednesday = "Wednesday"
    show Thursday = "Thursday"
    show Friday = "Friday"
    show Saturday = "Saturday"

weekdayFromNumber : Integer -> HebrewDayOfWeek numbering
weekdayFromNumber value = case value `mod` 7 of
  0 => HebrewWeekdays.Sunday
  1 => HebrewWeekdays.Monday
  2 => HebrewWeekdays.Tuesday
  3 => HebrewWeekdays.Wednesday
  4 => HebrewWeekdays.Thursday
  5 => HebrewWeekdays.Friday
  _ => HebrewWeekdays.Saturday

public export
hebrewMonthNumber : {numbering : HebrewNumbering} -> {year : Year} ->
                    KnownHebrewNumbering numbering =>
                    HebrewMonth numbering year -> Integer
hebrewMonthNumber value =
  (HebrewMonths.calendarIndex value - numberingStart {numbering} + 13) `mod` 13 + 1

monthsInHebrewYear : Year -> Integer
monthsInHebrewYear value = if isHebrewLeapYear value then 13 else 12

public export
total
monthsElapsed : Year -> Integer
monthsElapsed value =
  let number = yearValue value
      cycles = (number - 1) `div` 19
      inCycle = (number - 1) `mod` 19
   in 235 * cycles + 12 * inCycle + (7 * inCycle + 1) `div` 19

public export
total
elapsedHebrewDays : Year -> Integer
elapsedHebrewDays value =
  let elapsedMonths = monthsElapsed value
      partsElapsed = 204 + 793 * (elapsedMonths `mod` 1080)
      hoursElapsed = 5 + 12 * elapsedMonths + 793 * (elapsedMonths `div` 1080) +
        partsElapsed `div` 1080
      moladDay = 1 + 29 * elapsedMonths + hoursElapsed `div` 24
      moladParts = 1080 * (hoursElapsed `mod` 24) + partsElapsed `mod` 1080
      postponed = if moladParts >= 19440
        then moladDay + 1
        else if moladDay `mod` 7 == 2 && moladParts >= 9924 && not (isHebrewLeapYear value)
          then moladDay + 1
          else if moladDay `mod` 7 == 1 && moladParts >= 16789 &&
              isHebrewLeapYear (Year.fromInteger (yearValue value - 1))
            then moladDay + 1
            else moladDay
   in if postponed `mod` 7 == 0 || postponed `mod` 7 == 3 || postponed `mod` 7 == 5
        then postponed + 1
        else postponed

public export
total
firstHebrewDayOfYear : Year -> Integer
firstHebrewDayOfYear value = -2103608 + elapsedHebrewDays value

public export
total
daysInHebrewYear : Year -> Integer
daysInHebrewYear value =
  firstHebrewDayOfYear (Year.fromInteger (yearValue value + 1)) -
  firstHebrewDayOfYear value

public export
total
isCheshvanLong : Year -> Bool
isCheshvanLong value = daysInHebrewYear value `mod` 10 == 5

public export
total
isKislevShort : Year -> Bool
isKislevShort value = daysInHebrewYear value `mod` 10 == 3

public export
total
hebrewMonthLengthByIndex : Year -> Integer -> Integer
hebrewMonthLengthByIndex _ 0 = 30
hebrewMonthLengthByIndex value 1 = if isCheshvanLong value then 30 else 29
hebrewMonthLengthByIndex value 2 = if isKislevShort value then 29 else 30
hebrewMonthLengthByIndex _ 3 = 29
hebrewMonthLengthByIndex _ 4 = 30
hebrewMonthLengthByIndex value 5 = if isHebrewLeapYear value then 30 else 0
hebrewMonthLengthByIndex _ 6 = 29
hebrewMonthLengthByIndex _ 7 = 30
hebrewMonthLengthByIndex _ 8 = 29
hebrewMonthLengthByIndex _ 9 = 30
hebrewMonthLengthByIndex _ 10 = 29
hebrewMonthLengthByIndex _ 11 = 30
hebrewMonthLengthByIndex _ 12 = 29
hebrewMonthLengthByIndex _ _ = 0

public export
total
maxHebrewDaysInMonth : {numbering : HebrewNumbering} -> {year : Year} ->
                       (value : HebrewMonth numbering year) -> DayOfMonth
maxHebrewDaysInMonth {year} value =
  dayOfMonthFromInteger (hebrewMonthLengthByIndex year (HebrewMonths.calendarIndex value))

public export
total
isValidHebrewDay : DayOfMonth -> (valueYear : Year) ->
                   HebrewMonth numbering valueYear -> Bool
isValidHebrewDay valueDay valueYear HebrewMonths.Cheshvan =
  valueDay <= if isCheshvanLong valueYear then 30 else 29
isValidHebrewDay valueDay valueYear HebrewMonths.Kislev =
  valueDay <= if isKislevShort valueYear then 29 else 30
isValidHebrewDay valueDay _ HebrewMonths.Tevet = valueDay <= 29
isValidHebrewDay valueDay _ HebrewMonths.Adar = valueDay <= 29
isValidHebrewDay valueDay _ HebrewMonths.Iyar = valueDay <= 29
isValidHebrewDay valueDay _ HebrewMonths.Tammuz = valueDay <= 29
isValidHebrewDay valueDay _ HebrewMonths.Elul = valueDay <= 29
isValidHebrewDay valueDay _ _ = valueDay <= 30

total
daysBeforeHebrewMonth : Year -> Integer -> Integer
daysBeforeHebrewMonth _ 0 = 0
daysBeforeHebrewMonth _ 1 = 30
daysBeforeHebrewMonth value 2 = 30 + hebrewMonthLengthByIndex value 1
daysBeforeHebrewMonth value target =
  let variableDays = hebrewMonthLengthByIndex value 1 +
        hebrewMonthLengthByIndex value 2
      adarIDays = hebrewMonthLengthByIndex value 5
   in case target of
        3 => 30 + variableDays
        4 => 59 + variableDays
        5 => 89 + variableDays
        6 => 89 + variableDays + adarIDays
        7 => 118 + variableDays + adarIDays
        8 => 148 + variableDays + adarIDays
        9 => 177 + variableDays + adarIDays
        10 => 207 + variableDays + adarIDays
        11 => 236 + variableDays + adarIDays
        12 => 266 + variableDays + adarIDays
        _ => 295 + variableDays + adarIDays

total
hebrewYearMonthDayToDays : {numbering : HebrewNumbering} ->
                           (valueYear : Year) -> HebrewMonth numbering valueYear ->
                           DayOfMonth -> Integer
hebrewYearMonthDayToDays valueYear valueMonth valueDay =
  firstHebrewDayOfYear valueYear +
  daysBeforeHebrewMonth valueYear (HebrewMonths.calendarIndex valueMonth) +
  dayOfMonthValue valueDay - 1

monthFromCalendarIndex : {numbering : HebrewNumbering} ->
                         (valueYear : Year) -> Integer -> HebrewMonth numbering valueYear
monthFromCalendarIndex _ 0 = HebrewMonths.Tishri
monthFromCalendarIndex _ 1 = HebrewMonths.Cheshvan
monthFromCalendarIndex _ 2 = HebrewMonths.Kislev
monthFromCalendarIndex _ 3 = HebrewMonths.Tevet
monthFromCalendarIndex _ 4 = HebrewMonths.Shevat
monthFromCalendarIndex valueYear 5 = case choose (isHebrewLeapYear valueYear) of
  Left leap => HebrewMonths.AdarI @{leap}
  Right _ => HebrewMonths.Adar
monthFromCalendarIndex _ 6 = HebrewMonths.Adar
monthFromCalendarIndex _ 7 = HebrewMonths.Nisan
monthFromCalendarIndex _ 8 = HebrewMonths.Iyar
monthFromCalendarIndex _ 9 = HebrewMonths.Sivan
monthFromCalendarIndex _ 10 = HebrewMonths.Tammuz
monthFromCalendarIndex _ 11 = HebrewMonths.Av
monthFromCalendarIndex _ _ = HebrewMonths.Elul

findHebrewYear : Nat -> Integer -> Year -> Year
findHebrewYear Z days candidate = candidate
findHebrewYear (S fuel) days candidate =
  if firstHebrewDayOfYear candidate > days
    then findHebrewYear fuel days (yearFromInteger (yearValue candidate - 1))
    else let following = yearFromInteger (yearValue candidate + 1)
          in if firstHebrewDayOfYear following <= days
               then findHebrewYear fuel days following
               else candidate

findHebrewMonth : Nat -> Year -> Integer -> Integer -> (Integer, DayOfMonth)
findHebrewMonth Z valueYear index remaining =
  (index, dayOfMonthFromInteger (remaining + 1))
findHebrewMonth (S fuel) valueYear index remaining =
  let monthLength = hebrewMonthLengthByIndex valueYear index
   in if remaining < monthLength
        then (index, dayOfMonthFromInteger (remaining + 1))
        else findHebrewMonth fuel valueYear (index + 1) (remaining - monthLength)

hebrewCivilFromDays : {numbering : HebrewNumbering} -> Integer ->
  (valueYear : Year ** (HebrewMonth numbering valueYear, DayOfMonth))
hebrewCivilFromDays days =
  let firstYearDay = firstHebrewDayOfYear 1
      estimate = max 1 ((days - firstYearDay) `div` 366 + 1)
      valueYear = findHebrewYear (cast (abs estimate + 2)) days
        (yearFromInteger estimate)
      (monthIndex, valueDay) = findHebrewMonth 13 valueYear 0
        (days - firstHebrewDayOfYear valueYear)
  in (valueYear ** (monthFromCalendarIndex {numbering} valueYear monthIndex, valueDay))

export
record HebrewDate (numbering : HebrewNumbering) where
  constructor MkHebrewDate
  daysSinceEpoch : Integer
  dateYear : Year
  dateMonth : HebrewMonth numbering dateYear
  dateDay : DayOfMonth

public export
Eq (HebrewDate numbering) where
  left == right = left.daysSinceEpoch == right.daysSinceEpoch

public export
Ord (HebrewDate numbering) where
  compare left right = compare left.daysSinceEpoch right.daysSinceEpoch

public export
Show (HebrewDate numbering) where
  show (MkHebrewDate _ valueYear valueMonth valueDay) =
    "hebrewDate' " ++ show valueDay ++ " " ++
    show valueYear ++ " " ++ HebrewMonths.showMonth valueMonth

makeHebrewDate : {numbering : HebrewNumbering} -> Integer -> HebrewDate numbering
makeHebrewDate days = case hebrewCivilFromDays {numbering} days of
  (valueYear ** (valueMonth, valueDay)) =>
    MkHebrewDate days valueYear valueMonth valueDay

firstHebrewDay : Integer
firstHebrewDay = -2103607

public export
isValidHebrewDays : Integer -> Bool
isValidHebrewDays value = value >= -2103607

public export
{numbering : HebrewNumbering} -> HasCalendarDate (HebrewDate numbering) where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = isValidHebrewDays
  calendarDateFromDays days = makeHebrewDate days
  calendarDateName = "Hebrew"

public export
total
isValidHebrewDate : (valueDay : DayOfMonth) -> (valueYear : Year) ->
                    HebrewMonth numbering valueYear -> Bool
isValidHebrewDate valueDay valueYear valueMonth =
  let dayNumber = dayOfMonthValue valueDay
      yearNumber = yearValue valueYear
      maxDay = case valueMonth of
        HebrewMonths.Cheshvan => if isCheshvanLong valueYear then 30 else 29
        HebrewMonths.Kislev => if isKislevShort valueYear then 29 else 30
        HebrewMonths.Tevet => 29
        HebrewMonths.Adar => 29
        HebrewMonths.Iyar => 29
        HebrewMonths.Tammuz => 29
        HebrewMonths.Elul => 29
        _ => 30
   in yearNumber >= 1 && dayNumber <= maxDay

clampToHebrew : Integer -> Integer
clampToHebrew = max firstHebrewDay

shiftHebrewDays : {numbering : HebrewNumbering} ->
                  Integer -> HebrewDate numbering -> HebrewDate numbering
shiftHebrewDays amount date = makeHebrewDate (clampToHebrew (date.daysSinceEpoch + amount))

calendarIndexToPosition : Year -> Integer -> Integer
calendarIndexToPosition valueYear index =
  if isHebrewLeapYear valueYear || index < 5 then index else index - 1

positionToCalendarIndex : Year -> Integer -> Integer
positionToCalendarIndex valueYear position =
  if isHebrewLeapYear valueYear || position < 5 then position else position + 1

addHebrewMonths : Year -> Integer -> Integer -> (Year, Integer)
addHebrewMonths valueYear index amount =
  go (cast (abs amount + 2)) valueYear
    (calendarIndexToPosition valueYear index + amount)
  where
    go : Nat -> Year -> Integer -> (Year, Integer)
    go Z currentYear position =
      (currentYear, positionToCalendarIndex currentYear
        (max 0 (min (monthsInHebrewYear currentYear - 1) position)))
    go (S fuel) currentYear position =
      if position < 0
        then if yearValue currentYear <= 1
          then (1, 0)
          else let previousYear = yearFromInteger (yearValue currentYear - 1)
                in go fuel previousYear
                  (position + monthsInHebrewYear previousYear)
        else if position >= monthsInHebrewYear currentYear
          then go fuel (yearFromInteger (yearValue currentYear + 1))
            (position - monthsInHebrewYear currentYear)
          else (currentYear, positionToCalendarIndex currentYear position)

shiftHebrewMonths : {numbering : HebrewNumbering} ->
                    Integer -> HebrewDate numbering -> HebrewDate numbering
shiftHebrewMonths amount date =
  let (targetYear, targetIndex) = addHebrewMonths date.dateYear
        (HebrewMonths.calendarIndex date.dateMonth) amount
      targetMonth = monthFromCalendarIndex {numbering} targetYear targetIndex
      targetDay = min date.dateDay (maxHebrewDaysInMonth targetMonth)
   in makeHebrewDate (hebrewYearMonthDayToDays targetYear targetMonth targetDay)

shiftHebrewYears : {numbering : HebrewNumbering} ->
                   Integer -> HebrewDate numbering -> HebrewDate numbering
shiftHebrewYears amount date =
  let targetYear = yearFromInteger (max 1 (yearValue date.dateYear + amount))
      sourceIndex = HebrewMonths.calendarIndex date.dateMonth
      targetIndex = if sourceIndex == 5 && not (isHebrewLeapYear targetYear) then 6 else sourceIndex
      targetMonth = monthFromCalendarIndex {numbering} targetYear targetIndex
      targetDay = min date.dateDay (maxHebrewDaysInMonth targetMonth)
   in makeHebrewDate (hebrewYearMonthDayToDays targetYear targetMonth targetDay)

applyHebrewPeriod : {numbering : HebrewNumbering} ->
                    Period target -> HebrewDate numbering -> HebrewDate numbering
applyHebrewPeriod period =
    shiftHebrewDays (periodDays period)
  . shiftHebrewDays (7 * periodWeeks period)
  . shiftHebrewMonths (periodMonths period)
  . shiftHebrewYears (periodYears period)

hebrewDayOfWeek : HebrewDate numbering -> HebrewDayOfWeek numbering
hebrewDayOfWeek date = weekdayFromNumber (date.daysSinceEpoch + 3)

nextHebrew : {numbering : HebrewNumbering} ->
             Integer -> HebrewDayOfWeek numbering ->
             HebrewDate numbering -> HebrewDate numbering
nextHebrew count target date =
  let current = HebrewWeekdays.weekdayNumber (hebrewDayOfWeek date)
      wanted = HebrewWeekdays.weekdayNumber target
      weeks = if wanted > current then count - 1 else count
   in makeHebrewDate
        (clampToHebrew (date.daysSinceEpoch + 7 * weeks + wanted - current))

previousHebrew : {numbering : HebrewNumbering} ->
                 Integer -> HebrewDayOfWeek numbering ->
                 HebrewDate numbering -> HebrewDate numbering
previousHebrew count target date =
  let current = HebrewWeekdays.weekdayNumber (hebrewDayOfWeek date)
      wanted = HebrewWeekdays.weekdayNumber target
      weeks = if wanted < current then count - 1 else count
   in makeHebrewDate
        (clampToHebrew (date.daysSinceEpoch - (7 * weeks + current - wanted)))

public export
{numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
  Calendar (Hebrew numbering) where
  DateRep = HebrewDate numbering
  MonthRep valueYear = HebrewMonth numbering valueYear
  WeekdayRep = HebrewDayOfWeek numbering

  isValidDays = isValidHebrewDays
  fromDays days = makeHebrewDate days
  toDays date = date.daysSinceEpoch
  calendarName = "Hebrew"

  year' = dateYear
  toYmd date = (date.dateMonth, date.dateDay)
  day' = dateDay
  month' = dateMonth

  applyCalendarPeriod' = applyHebrewPeriod
  shiftCalendarDays' = shiftHebrewDays

  dayOfWeek = hebrewDayOfWeek
  next = nextHebrew
  previous = previousHebrew

public export
{numbering : HebrewNumbering} -> HasCalendar (HebrewDate numbering) where
  calendarCapability = ()

public export
{numbering : HebrewNumbering} -> ApplyPeriod (HebrewDate numbering) where
  applyPeriod = applyHebrewPeriod

||| Construct a statically validated Hebrew date in the selected numbering.
||| The month is indexed by the year, making Adar I unavailable in common years.
public export
hebrewDate' : {numbering : HebrewNumbering} ->
              {auto known : KnownHebrewNumbering numbering} ->
              (valueDay : DayOfMonth) -> (valueYear : Year) ->
              (valueMonth : HebrewMonth numbering valueYear) ->
              {auto 0 valid : So (isValidHebrewDate valueDay valueYear valueMonth)} ->
              CalendarDate (Hebrew numbering)
hebrewDate' valueDay valueYear valueMonth =
  makeHebrewDate (hebrewYearMonthDayToDays valueYear valueMonth valueDay)

||| Construct a statically validated civil-numbered Hebrew date.
public export
hebrewDate : (valueDay : DayOfMonth) -> (valueYear : Year) ->
             (valueMonth : HebrewMonth Civil valueYear) ->
             {auto 0 valid : So (isValidHebrewDate valueDay valueYear valueMonth)} ->
             CalendarDate HebrewCivil
hebrewDate = hebrewDate'

||| Failures produced while refining untrusted Hebrew date data.
public export
data HebrewDateError
  = InvalidHebrewMonth HebrewMonthName Year
  | InvalidHebrewDate DayOfMonth HebrewMonthName Year
  | InvalidHebrewDayCount Integer
  | InvalidHebrewNthDay DayNth HebrewMonthName Year
  | InvalidHebrewWeekDate WeekNumber Year

||| Refine a runtime month name into a year-indexed Hebrew month.
||| Adar I is rejected when the supplied year is not leap.
public export
refineHebrewMonth : {numbering : HebrewNumbering} -> (valueYear : Year) -> HebrewMonthName ->
                    Either HebrewDateError (HebrewMonth numbering valueYear)
refineHebrewMonth _ TishriName = Right HebrewMonths.Tishri
refineHebrewMonth _ CheshvanName = Right HebrewMonths.Cheshvan
refineHebrewMonth _ KislevName = Right HebrewMonths.Kislev
refineHebrewMonth _ TevetName = Right HebrewMonths.Tevet
refineHebrewMonth _ ShevatName = Right HebrewMonths.Shevat
refineHebrewMonth valueYear AdarIName = case choose (isHebrewLeapYear valueYear) of
  Left leap => Right (HebrewMonths.AdarI @{leap})
  Right _ => Left (InvalidHebrewMonth AdarIName valueYear)
refineHebrewMonth _ AdarName = Right HebrewMonths.Adar
refineHebrewMonth _ NisanName = Right HebrewMonths.Nisan
refineHebrewMonth _ IyarName = Right HebrewMonths.Iyar
refineHebrewMonth _ SivanName = Right HebrewMonths.Sivan
refineHebrewMonth _ TammuzName = Right HebrewMonths.Tammuz
refineHebrewMonth _ AvName = Right HebrewMonths.Av
refineHebrewMonth _ ElulName = Right HebrewMonths.Elul

||| Validate runtime date components in the selected Hebrew numbering.
public export
refineHebrewDate' : {numbering : HebrewNumbering} ->
                    {auto known : KnownHebrewNumbering numbering} ->
                    DayOfMonth -> HebrewMonthName -> Year ->
                    Either HebrewDateError (CalendarDate (Hebrew numbering))
refineHebrewDate' @{known} valueDay valueMonthName valueYear =
  case refineHebrewMonth {numbering} valueYear valueMonthName of
    Left error => Left error
    Right valueMonth => case choose (isValidHebrewDate valueDay valueYear valueMonth) of
      Left valid => Right (hebrewDate' @{known} valueDay valueYear valueMonth @{valid})
      Right _ => Left (InvalidHebrewDate valueDay valueMonthName valueYear)

||| Validate runtime date components using civil Hebrew numbering.
public export
refineHebrewDate : DayOfMonth -> HebrewMonthName -> Year ->
                   Either HebrewDateError (CalendarDate HebrewCivil)
refineHebrewDate = refineHebrewDate'

||| Construct a Hebrew date in the selected numbering from a statically valid
||| calendar-relative day count.
public export
hebrewFromDays' : {numbering : HebrewNumbering} ->
                  {auto known : KnownHebrewNumbering numbering} ->
                  (days : Integer) -> {auto 0 valid : So (isValidHebrewDays days)} ->
                  CalendarDate (Hebrew numbering)
hebrewFromDays' days = makeHebrewDate days

public export
hebrewFromDays : (days : Integer) -> {auto 0 valid : So (isValidHebrewDays days)} ->
                 CalendarDate HebrewCivil
hebrewFromDays = hebrewFromDays'

||| Validate a runtime Hebrew day count in the selected numbering.
public export
refineHebrewDays' : {numbering : HebrewNumbering} ->
                    {auto known : KnownHebrewNumbering numbering} ->
                    Integer -> Either HebrewDateError (CalendarDate (Hebrew numbering))
refineHebrewDays' @{known} days = case choose (isValidHebrewDays days) of
  Left valid => Right (hebrewFromDays' @{known} days @{valid})
  Right _ => Left (InvalidHebrewDayCount days)

public export
refineHebrewDays : Integer -> Either HebrewDateError (CalendarDate HebrewCivil)
refineHebrewDays = refineHebrewDays'

public export
total
nthHebrewDayOfMonth : {numbering : HebrewNumbering} -> DayNth -> HebrewDayOfWeek numbering ->
                      (valueYear : Year) -> HebrewMonth numbering valueYear -> DayOfMonth
nthHebrewDayOfMonth nth target valueYear valueMonth =
  let monthLength = maxHebrewDaysInMonth valueMonth
      firstDays = hebrewYearMonthDayToDays valueYear valueMonth 1
      firstOffset =
        (HebrewWeekdays.weekdayNumber target -
         (firstDays + 3) `mod` 7) `mod` 7
      lastDays = hebrewYearMonthDayToDays valueYear valueMonth monthLength
      lastOffset =
        ((lastDays + 3) `mod` 7 -
         HebrewWeekdays.weekdayNumber target) `mod` 7
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
total
isValidHebrewNthDay : {numbering : HebrewNumbering} -> DayNth -> HebrewDayOfWeek numbering ->
                      (valueYear : Year) -> HebrewMonth numbering valueYear -> Bool
isValidHebrewNthDay First _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay Second _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay Third _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay Fourth _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay Last _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay SecondToLast _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay ThirdToLast _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay FourthToLast _ valueYear _ = yearValue valueYear >= 1
isValidHebrewNthDay Fifth target valueYear valueMonth =
  yearValue valueYear >= 1 &&
  dayOfMonthValue (nthHebrewDayOfMonth Fifth target valueYear valueMonth) <=
    dayOfMonthValue (maxHebrewDaysInMonth valueMonth)

||| Construct the nth requested weekday in a Hebrew month using the selected
||| numbering convention.
public export
hebrewFromNthDay' : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
                     (nth : DayNth) -> (target : HebrewDayOfWeek numbering) ->
                     (valueYear : Year) -> (valueMonth : HebrewMonth numbering valueYear) ->
                     {auto 0 valid : So
                       (isValidHebrewNthDay nth target valueYear valueMonth)} ->
                     CalendarDate (Hebrew numbering)
hebrewFromNthDay' nth target valueYear valueMonth =
  makeHebrewDate
    (hebrewYearMonthDayToDays valueYear valueMonth
      (nthHebrewDayOfMonth nth target valueYear valueMonth))

public export
hebrewFromNthDay : (nth : DayNth) -> (target : HebrewDayOfWeek Civil) ->
                   (valueYear : Year) -> (valueMonth : HebrewMonth Civil valueYear) ->
                   {auto 0 valid : So
                     (isValidHebrewNthDay nth target valueYear valueMonth)} ->
                   CalendarDate HebrewCivil
hebrewFromNthDay = hebrewFromNthDay'

||| Validate an nth-weekday request in the selected Hebrew numbering.
public export
refineHebrewNthDay' : {numbering : HebrewNumbering} ->
                      {auto known : KnownHebrewNumbering numbering} ->
                      DayNth -> HebrewDayOfWeek numbering -> Year -> HebrewMonthName ->
                      Either HebrewDateError (CalendarDate (Hebrew numbering))
refineHebrewNthDay' @{known} nth target valueYear valueMonthName =
  case refineHebrewMonth {numbering} valueYear valueMonthName of
    Left error => Left error
    Right valueMonth =>
      case choose (isValidHebrewNthDay nth target valueYear valueMonth) of
        Left valid => Right
          (hebrewFromNthDay' @{known} nth target valueYear valueMonth @{valid})
        Right _ => Left (InvalidHebrewNthDay nth valueMonthName valueYear)

public export
refineHebrewNthDay : DayNth -> HebrewDayOfWeek Civil -> Year -> HebrewMonthName ->
                     Either HebrewDateError (CalendarDate HebrewCivil)
refineHebrewNthDay = refineHebrewNthDay'

public export
total
hebrewWeekDateDays : {numbering : HebrewNumbering} ->
                     WeekNumber -> HebrewDayOfWeek numbering -> Year -> Integer
hebrewWeekDateDays week target valueYear =
  let firstDay = firstHebrewDayOfYear valueYear
      firstWeekStart = firstDay - (firstDay + 3) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      HebrewWeekdays.weekdayNumber target

public export
total
isValidHebrewWeekDate : {numbering : HebrewNumbering} ->
                        WeekNumber -> HebrewDayOfWeek numbering -> Year -> Bool
isValidHebrewWeekDate week target valueYear =
  if yearValue valueYear > 1
    then True
    else isValidHebrewDays (hebrewWeekDateDays week target valueYear)

||| Construct a Sunday-based Hebrew week date in the selected numbering.
public export
hebrewFromWeekDate' : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
                      (week : WeekNumber) -> (target : HebrewDayOfWeek numbering) ->
                      (valueYear : Year) ->
                      {auto 0 valid : So (isValidHebrewWeekDate week target valueYear)} ->
                      CalendarDate (Hebrew numbering)
hebrewFromWeekDate' week target valueYear =
  makeHebrewDate (hebrewWeekDateDays week target valueYear)

public export
hebrewFromWeekDate : (week : WeekNumber) -> (target : HebrewDayOfWeek Civil) ->
                     (valueYear : Year) ->
                     {auto 0 valid : So (isValidHebrewWeekDate week target valueYear)} ->
                     CalendarDate HebrewCivil
hebrewFromWeekDate = hebrewFromWeekDate'

||| Validate a runtime Hebrew week date in the selected numbering.
public export
refineHebrewWeekDate' : {numbering : HebrewNumbering} ->
                        {auto known : KnownHebrewNumbering numbering} ->
                        WeekNumber -> HebrewDayOfWeek numbering -> Year ->
                        Either HebrewDateError (CalendarDate (Hebrew numbering))
refineHebrewWeekDate' @{known} week target valueYear =
  case choose (isValidHebrewWeekDate week target valueYear) of
    Left valid => Right (hebrewFromWeekDate' @{known} week target valueYear @{valid})
    Right _ => Left (InvalidHebrewWeekDate week valueYear)

public export
refineHebrewWeekDate : WeekNumber -> HebrewDayOfWeek Civil -> Year ->
                       Either HebrewDateError (CalendarDate HebrewCivil)
refineHebrewWeekDate = refineHebrewWeekDate'