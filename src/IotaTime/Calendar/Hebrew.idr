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
isLeapYear : Year -> Bool
isLeapYear value = (7 * yearValue value + 1) `mod` 19 < 7

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
      {auto 0 leap : So (isLeapYear valueYear)} ->
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
monthNumber : {numbering : HebrewNumbering} -> {year : Year} ->
                    KnownHebrewNumbering numbering =>
                    HebrewMonth numbering year -> Integer
monthNumber value =
  (HebrewMonths.calendarIndex value - numberingStart {numbering} + 13) `mod` 13 + 1

monthsInHebrewYear : Year -> Integer
monthsInHebrewYear value = if isLeapYear value then 13 else 12

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
elapsedDays : Year -> Integer
elapsedDays value =
  let elapsedMonths = monthsElapsed value
      partsElapsed = 204 + 793 * (elapsedMonths `mod` 1080)
      hoursElapsed = 5 + 12 * elapsedMonths + 793 * (elapsedMonths `div` 1080) +
        partsElapsed `div` 1080
      moladDay = 1 + 29 * elapsedMonths + hoursElapsed `div` 24
      moladParts = 1080 * (hoursElapsed `mod` 24) + partsElapsed `mod` 1080
      postponed = if moladParts >= 19440
        then moladDay + 1
        else if moladDay `mod` 7 == 2 && moladParts >= 9924 && not (isLeapYear value)
          then moladDay + 1
          else if moladDay `mod` 7 == 1 && moladParts >= 16789 &&
              isLeapYear (Year.fromInteger (yearValue value - 1))
            then moladDay + 1
            else moladDay
   in if postponed `mod` 7 == 0 || postponed `mod` 7 == 3 || postponed `mod` 7 == 5
        then postponed + 1
        else postponed

public export
total
firstDayOfYear : Year -> Integer
firstDayOfYear value = -2103608 + elapsedDays value

public export
total
daysInYear : Year -> Integer
daysInYear value =
  firstDayOfYear (Year.fromInteger (yearValue value + 1)) -
  firstDayOfYear value

public export
total
isCheshvanLong : Year -> Bool
isCheshvanLong value = daysInYear value `mod` 10 == 5

public export
total
isKislevShort : Year -> Bool
isKislevShort value = daysInYear value `mod` 10 == 3

public export
total
monthLengthByIndex : Year -> Integer -> Integer
monthLengthByIndex _ 0 = 30
monthLengthByIndex value 1 = if isCheshvanLong value then 30 else 29
monthLengthByIndex value 2 = if isKislevShort value then 29 else 30
monthLengthByIndex _ 3 = 29
monthLengthByIndex _ 4 = 30
monthLengthByIndex value 5 = if isLeapYear value then 30 else 0
monthLengthByIndex _ 6 = 29
monthLengthByIndex _ 7 = 30
monthLengthByIndex _ 8 = 29
monthLengthByIndex _ 9 = 30
monthLengthByIndex _ 10 = 29
monthLengthByIndex _ 11 = 30
monthLengthByIndex _ 12 = 29
monthLengthByIndex _ _ = 0

public export
total
maxDaysInMonth : {numbering : HebrewNumbering} -> {year : Year} ->
                       (value : HebrewMonth numbering year) -> DayOfMonth
maxDaysInMonth {year} value =
  dayOfMonthFromInteger (monthLengthByIndex year (HebrewMonths.calendarIndex value))

public export
total
isValidDay : DayOfMonth -> (valueYear : Year) ->
                   HebrewMonth numbering valueYear -> Bool
isValidDay valueDay valueYear HebrewMonths.Cheshvan =
  valueDay <= if isCheshvanLong valueYear then 30 else 29
isValidDay valueDay valueYear HebrewMonths.Kislev =
  valueDay <= if isKislevShort valueYear then 29 else 30
isValidDay valueDay _ HebrewMonths.Tevet = valueDay <= 29
isValidDay valueDay _ HebrewMonths.Adar = valueDay <= 29
isValidDay valueDay _ HebrewMonths.Iyar = valueDay <= 29
isValidDay valueDay _ HebrewMonths.Tammuz = valueDay <= 29
isValidDay valueDay _ HebrewMonths.Elul = valueDay <= 29
isValidDay valueDay _ _ = valueDay <= 30

total
daysBeforeHebrewMonth : Year -> Integer -> Integer
daysBeforeHebrewMonth _ 0 = 0
daysBeforeHebrewMonth _ 1 = 30
daysBeforeHebrewMonth value 2 = 30 + monthLengthByIndex value 1
daysBeforeHebrewMonth value target =
  let variableDays = monthLengthByIndex value 1 +
        monthLengthByIndex value 2
      adarIDays = monthLengthByIndex value 5
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
  firstDayOfYear valueYear +
  daysBeforeHebrewMonth valueYear (HebrewMonths.calendarIndex valueMonth) +
  dayOfMonthValue valueDay - 1

monthFromCalendarIndex : {numbering : HebrewNumbering} ->
                         (valueYear : Year) -> Integer -> HebrewMonth numbering valueYear
monthFromCalendarIndex _ 0 = HebrewMonths.Tishri
monthFromCalendarIndex _ 1 = HebrewMonths.Cheshvan
monthFromCalendarIndex _ 2 = HebrewMonths.Kislev
monthFromCalendarIndex _ 3 = HebrewMonths.Tevet
monthFromCalendarIndex _ 4 = HebrewMonths.Shevat
monthFromCalendarIndex valueYear 5 = case choose (isLeapYear valueYear) of
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
  if firstDayOfYear candidate > days
    then findHebrewYear fuel days (yearFromInteger (yearValue candidate - 1))
    else let following = yearFromInteger (yearValue candidate + 1)
          in if firstDayOfYear following <= days
               then findHebrewYear fuel days following
               else candidate

findHebrewMonth : Nat -> Year -> Integer -> Integer -> (Integer, DayOfMonth)
findHebrewMonth Z valueYear index remaining =
  (index, dayOfMonthFromInteger (remaining + 1))
findHebrewMonth (S fuel) valueYear index remaining =
  let monthLength = monthLengthByIndex valueYear index
   in if remaining < monthLength
        then (index, dayOfMonthFromInteger (remaining + 1))
        else findHebrewMonth fuel valueYear (index + 1) (remaining - monthLength)

hebrewCivilFromDays : {numbering : HebrewNumbering} -> Integer ->
  (valueYear : Year ** (HebrewMonth numbering valueYear, DayOfMonth))
hebrewCivilFromDays days =
  let firstYearDay = firstDayOfYear 1
      estimate = max 1 ((days - firstYearDay) `div` 366 + 1)
      valueYear = findHebrewYear (cast (abs estimate + 2)) days
        (yearFromInteger estimate)
      (monthIndex, valueDay) = findHebrewMonth 13 valueYear 0
        (days - firstDayOfYear valueYear)
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
    "calendarDate' " ++ show valueDay ++ " " ++
    show valueYear ++ " " ++ HebrewMonths.showMonth valueMonth

makeHebrewDate : {numbering : HebrewNumbering} -> Integer -> HebrewDate numbering
makeHebrewDate days = case hebrewCivilFromDays {numbering} days of
  (valueYear ** (valueMonth, valueDay)) =>
    MkHebrewDate days valueYear valueMonth valueDay

||| The Hebrew calendar epoch day, representing 1 Tishri 1.
public export
epochDay : Integer
epochDay = -2103607

public export
{numbering : HebrewNumbering} -> HasCalendarDate (HebrewDate numbering) where
  calendarDays = daysSinceEpoch
  acceptsCalendarDays = (>= epochDay)
  calendarDateFromDays days = makeHebrewDate days
  calendarDateName = "Hebrew"

public export
total
isValidDate : (valueDay : DayOfMonth) -> (valueYear : Year) ->
                    HebrewMonth numbering valueYear -> Bool
isValidDate valueDay valueYear valueMonth =
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
clampToHebrew = max epochDay

shiftHebrewDays : {numbering : HebrewNumbering} ->
                  Integer -> HebrewDate numbering -> HebrewDate numbering
shiftHebrewDays amount date = makeHebrewDate (clampToHebrew (date.daysSinceEpoch + amount))

calendarIndexToPosition : Year -> Integer -> Integer
calendarIndexToPosition valueYear index =
  if isLeapYear valueYear || index < 5 then index else index - 1

positionToCalendarIndex : Year -> Integer -> Integer
positionToCalendarIndex valueYear position =
  if isLeapYear valueYear || position < 5 then position else position + 1

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
      targetDay = min date.dateDay (maxDaysInMonth targetMonth)
   in makeHebrewDate (hebrewYearMonthDayToDays targetYear targetMonth targetDay)

shiftHebrewYears : {numbering : HebrewNumbering} ->
                   Integer -> HebrewDate numbering -> HebrewDate numbering
shiftHebrewYears amount date =
  let targetYear = yearFromInteger (max 1 (yearValue date.dateYear + amount))
      sourceIndex = HebrewMonths.calendarIndex date.dateMonth
      targetIndex = if sourceIndex == 5 && not (isLeapYear targetYear) then 6 else sourceIndex
      targetMonth = monthFromCalendarIndex {numbering} targetYear targetIndex
      targetDay = min date.dateDay (maxDaysInMonth targetMonth)
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

  isValidDays = (>= epochDay)
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

public export
{numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
  CalendarValue (HebrewDate numbering) where
  CalendarMonth valueYear = HebrewMonth numbering valueYear
  calendarValueYearMonthDay = yearMonthDayFor {calendar = Hebrew numbering}
  calendarValueBetweenWith = betweenWithFor {calendar = Hebrew numbering}

||| Construct a statically validated Hebrew date in the selected numbering.
||| The month is indexed by the year, making Adar I unavailable in common years.
public export
calendarDate' : {numbering : HebrewNumbering} ->
              {auto known : KnownHebrewNumbering numbering} ->
              (valueDay : DayOfMonth) -> (valueYear : Year) ->
              (valueMonth : HebrewMonth numbering valueYear) ->
              {auto 0 valid : So (isValidDate valueDay valueYear valueMonth)} ->
              CalendarDate (Hebrew numbering)
calendarDate' valueDay valueYear valueMonth =
  makeHebrewDate (hebrewYearMonthDayToDays valueYear valueMonth valueDay)

||| Construct a statically validated civil-numbered Hebrew date.
public export
calendarDate : (valueDay : DayOfMonth) -> (valueYear : Year) ->
             (valueMonth : HebrewMonth Civil valueYear) ->
             {auto 0 valid : So (isValidDate valueDay valueYear valueMonth)} ->
             CalendarDate HebrewCivil
calendarDate = calendarDate'

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
refineMonth : {numbering : HebrewNumbering} -> (valueYear : Year) -> HebrewMonthName ->
                    Either HebrewDateError (HebrewMonth numbering valueYear)
refineMonth _ TishriName = Right HebrewMonths.Tishri
refineMonth _ CheshvanName = Right HebrewMonths.Cheshvan
refineMonth _ KislevName = Right HebrewMonths.Kislev
refineMonth _ TevetName = Right HebrewMonths.Tevet
refineMonth _ ShevatName = Right HebrewMonths.Shevat
refineMonth valueYear AdarIName = case choose (isLeapYear valueYear) of
  Left leap => Right (HebrewMonths.AdarI @{leap})
  Right _ => Left (InvalidHebrewMonth AdarIName valueYear)
refineMonth _ AdarName = Right HebrewMonths.Adar
refineMonth _ NisanName = Right HebrewMonths.Nisan
refineMonth _ IyarName = Right HebrewMonths.Iyar
refineMonth _ SivanName = Right HebrewMonths.Sivan
refineMonth _ TammuzName = Right HebrewMonths.Tammuz
refineMonth _ AvName = Right HebrewMonths.Av
refineMonth _ ElulName = Right HebrewMonths.Elul

||| Validate runtime date components in the selected Hebrew numbering.
public export
refineDate' : {numbering : HebrewNumbering} ->
                    {auto known : KnownHebrewNumbering numbering} ->
                    DayOfMonth -> HebrewMonthName -> Year ->
                    Either HebrewDateError (CalendarDate (Hebrew numbering))
refineDate' @{known} valueDay valueMonthName valueYear =
  case refineMonth {numbering} valueYear valueMonthName of
    Left error => Left error
    Right valueMonth => case choose (isValidDate valueDay valueYear valueMonth) of
      Left valid => Right
        (calendarDate' @{known} valueDay valueYear valueMonth @{valid})
      Right _ => Left (InvalidHebrewDate valueDay valueMonthName valueYear)

||| Validate runtime date components using civil Hebrew numbering.
public export
refineDate : DayOfMonth -> HebrewMonthName -> Year ->
                   Either HebrewDateError (CalendarDate HebrewCivil)
refineDate = refineDate'

||| Construct a Hebrew date in the selected numbering from a statically valid
||| calendar-relative day count.
public export
fromDays' : {numbering : HebrewNumbering} ->
                  {auto known : KnownHebrewNumbering numbering} ->
                  (days : Integer) -> {auto 0 valid : So
                    (IotaTime.Calendar.isValidDays
                      {calendar = Hebrew numbering} days)} ->
                  CalendarDate (Hebrew numbering)
fromDays' days = makeHebrewDate days

public export
fromDays : (days : Integer) -> {auto 0 valid : So
  (IotaTime.Calendar.isValidDays {calendar = HebrewCivil} days)} ->
                 CalendarDate HebrewCivil
fromDays = fromDays'

||| Validate a runtime Hebrew day count in the selected numbering.
public export
refineDays' : {numbering : HebrewNumbering} ->
                    {auto known : KnownHebrewNumbering numbering} ->
                    Integer -> Either HebrewDateError (CalendarDate (Hebrew numbering))
refineDays' @{known} days = case choose
  (IotaTime.Calendar.isValidDays {calendar = Hebrew numbering} days) of
  Left valid => Right (fromDays' @{known} days @{valid})
  Right _ => Left (InvalidHebrewDayCount days)

public export
refineDays : Integer -> Either HebrewDateError (CalendarDate HebrewCivil)
refineDays = refineDays'

public export
total
nthDayOfMonth : {numbering : HebrewNumbering} -> DayNth -> HebrewDayOfWeek numbering ->
                      (valueYear : Year) -> HebrewMonth numbering valueYear -> DayOfMonth
nthDayOfMonth nth target valueYear valueMonth =
  let monthLength = maxDaysInMonth valueMonth
      firstDays = hebrewYearMonthDayToDays valueYear valueMonth 1
      firstOffset =
        (HebrewWeekdays.weekdayNumber target -
         (firstDays + 3) `mod` daysPerWeek) `mod` daysPerWeek
      lastDays = hebrewYearMonthDayToDays valueYear valueMonth monthLength
      lastOffset =
        ((lastDays + 3) `mod` daysPerWeek -
         HebrewWeekdays.weekdayNumber target) `mod` daysPerWeek
      dayNumber = nthWeekdayDayNumber nth (dayOfMonthValue monthLength)
        firstOffset lastOffset
   in dayOfMonthFromInteger dayNumber

public export
total
isValidNthDay : {numbering : HebrewNumbering} -> DayNth -> HebrewDayOfWeek numbering ->
                      (valueYear : Year) -> HebrewMonth numbering valueYear -> Bool
isValidNthDay First _ valueYear _ = yearValue valueYear >= 1
isValidNthDay Second _ valueYear _ = yearValue valueYear >= 1
isValidNthDay Third _ valueYear _ = yearValue valueYear >= 1
isValidNthDay Fourth _ valueYear _ = yearValue valueYear >= 1
isValidNthDay Last _ valueYear _ = yearValue valueYear >= 1
isValidNthDay SecondToLast _ valueYear _ = yearValue valueYear >= 1
isValidNthDay ThirdToLast _ valueYear _ = yearValue valueYear >= 1
isValidNthDay FourthToLast _ valueYear _ = yearValue valueYear >= 1
isValidNthDay Fifth target valueYear valueMonth =
  yearValue valueYear >= 1 &&
  dayOfMonthValue (nthDayOfMonth Fifth target valueYear valueMonth) <=
    dayOfMonthValue (maxDaysInMonth valueMonth)

||| Construct the nth requested weekday in a Hebrew month using the selected
||| numbering convention.
public export
fromNthDay' : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
                     (nth : DayNth) -> (target : HebrewDayOfWeek numbering) ->
                     (valueYear : Year) -> (valueMonth : HebrewMonth numbering valueYear) ->
                     {auto 0 valid : So
                       (isValidNthDay nth target valueYear valueMonth)} ->
                     CalendarDate (Hebrew numbering)
fromNthDay' nth target valueYear valueMonth =
  makeHebrewDate
    (hebrewYearMonthDayToDays valueYear valueMonth
      (nthDayOfMonth nth target valueYear valueMonth))

public export
fromNthDay : (nth : DayNth) -> (target : HebrewDayOfWeek Civil) ->
                   (valueYear : Year) -> (valueMonth : HebrewMonth Civil valueYear) ->
                   {auto 0 valid : So
                     (isValidNthDay nth target valueYear valueMonth)} ->
                   CalendarDate HebrewCivil
fromNthDay = fromNthDay'

||| Validate an nth-weekday request in the selected Hebrew numbering.
public export
refineNthDay' : {numbering : HebrewNumbering} ->
                      {auto known : KnownHebrewNumbering numbering} ->
                      DayNth -> HebrewDayOfWeek numbering -> Year -> HebrewMonthName ->
                      Either HebrewDateError (CalendarDate (Hebrew numbering))
refineNthDay' @{known} nth target valueYear valueMonthName =
  case refineMonth {numbering} valueYear valueMonthName of
    Left error => Left error
    Right valueMonth =>
      case choose (isValidNthDay nth target valueYear valueMonth) of
        Left valid => Right
          (fromNthDay' @{known} nth target valueYear valueMonth @{valid})
        Right _ => Left (InvalidHebrewNthDay nth valueMonthName valueYear)

public export
refineNthDay : DayNth -> HebrewDayOfWeek Civil -> Year -> HebrewMonthName ->
                     Either HebrewDateError (CalendarDate HebrewCivil)
refineNthDay = refineNthDay'

public export
total
weekDateDays : {numbering : HebrewNumbering} ->
                     WeekNumber -> HebrewDayOfWeek numbering -> Year -> Integer
weekDateDays week target valueYear =
  let firstDay = firstDayOfYear valueYear
      firstWeekStart = firstDay - (firstDay + 3) `mod` 7
   in firstWeekStart + 7 * (weekNumberValue week - 1) +
      HebrewWeekdays.weekdayNumber target

public export
total
isValidWeekDate : {numbering : HebrewNumbering} ->
                  KnownHebrewNumbering numbering =>
                  WeekNumber -> HebrewDayOfWeek numbering -> Year -> Bool
isValidWeekDate week target valueYear =
  (yearValue valueYear > 1 && weekNumberValue week >= 0) ||
    IotaTime.Calendar.isValidDays {calendar = Hebrew numbering}
      (weekDateDays week target valueYear)

||| Construct a Sunday-based Hebrew week date in the selected numbering.
public export
fromWeekDate' : {numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
                (week : WeekNumber) -> (target : HebrewDayOfWeek numbering) ->
                (valueYear : Year) ->
                {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
                CalendarDate (Hebrew numbering)
fromWeekDate' week target valueYear =
  makeHebrewDate (weekDateDays week target valueYear)

public export
fromWeekDate : (week : WeekNumber) -> (target : HebrewDayOfWeek Civil) ->
               (valueYear : Year) ->
               {auto 0 valid : So (isValidWeekDate week target valueYear)} ->
               CalendarDate HebrewCivil
fromWeekDate = fromWeekDate'

||| Validate a runtime Hebrew week date in the selected numbering.
public export
refineWeekDate' : {numbering : HebrewNumbering} ->
                  {auto known : KnownHebrewNumbering numbering} ->
                  WeekNumber -> HebrewDayOfWeek numbering -> Year ->
                  Either HebrewDateError (CalendarDate (Hebrew numbering))
refineWeekDate' @{known} week target valueYear =
  case choose (isValidWeekDate week target valueYear) of
    Left valid => Right (fromWeekDate' @{known} week target valueYear @{valid})
    Right _ => Left (InvalidHebrewWeekDate week valueYear)

public export
refineWeekDate : WeekNumber -> HebrewDayOfWeek Civil -> Year ->
                 Either HebrewDateError (CalendarDate HebrewCivil)
refineWeekDate = refineWeekDate'