module IotaTime.Calendar

import public IotaTime.Calendar.Component
import IotaTime.Period
import Derive.Prelude

%language ElabReflection

%default total

||| Selects an occurrence of a weekday within a month.
public export
data DayNth
  = FourthToLast
  | ThirdToLast
  | SecondToLast
  | Last
  | First
  | Second
  | Third
  | Fourth
  | Fifth

%runElab derive `{DayNth} [Eq, Show]

||| Number of days in the standard civil week modeled by iotaTime calendars.
public export
daysPerWeek : Integer
daysPerWeek = 7

||| Compute the raw day-of-month candidate for a weekday occurrence.
||| Calendar implementations remain responsible for validating the candidate
||| against their supported year and month ranges before constructing a date.
export
nthWeekdayDayNumber : DayNth -> (monthLength : Integer) ->
                      (firstOffset : Integer) -> (lastOffset : Integer) ->
                      Integer
nthWeekdayDayNumber FourthToLast monthLength _ lastOffset =
  monthLength - lastOffset - 3 * daysPerWeek
nthWeekdayDayNumber ThirdToLast monthLength _ lastOffset =
  monthLength - lastOffset - 2 * daysPerWeek
nthWeekdayDayNumber SecondToLast monthLength _ lastOffset =
  monthLength - lastOffset - daysPerWeek
nthWeekdayDayNumber Last monthLength _ lastOffset =
  monthLength - lastOffset
nthWeekdayDayNumber First _ firstOffset _ = 1 + firstOffset
nthWeekdayDayNumber Second _ firstOffset _ =
  1 + daysPerWeek + firstOffset
nthWeekdayDayNumber Third _ firstOffset _ =
  1 + 2 * daysPerWeek + firstOffset
nthWeekdayDayNumber Fourth _ firstOffset _ =
  1 + 3 * daysPerWeek + firstOffset
nthWeekdayDayNumber Fifth _ firstOffset _ =
  1 + 4 * daysPerWeek + firstOffset

||| A calendar conversion failed because the target calendar cannot represent
||| the source date's absolute day count.
public export
data CalendarConversionError = TargetCalendarOutOfRange String Integer

||| Calendar units used when decomposing the difference between two dates.
public export
data DateDifferenceUnits = DaysOnly | YearsMonthsDays

||| Month arithmetic used while decomposing a calendar difference.
public export
data MonthArithmeticPolicy = ClampToMonth

||| Controls how a difference between calendar dates is decomposed.
public export
record DateDifferencePolicy where
  constructor MkDateDifferencePolicy
  units : DateDifferenceUnits
  monthArithmetic : MonthArithmeticPolicy

||| The standard largest-first, non-overshooting calendar decomposition.
public export
nodaTimePolicy : DateDifferencePolicy
nodaTimePolicy = MkDateDifferencePolicy YearsMonthsDays ClampToMonth

||| Capabilities and dependent representations required of a calendar.
||| `MonthRep` may depend on the year, allowing calendars such as Hebrew to
||| make leap-only months unrepresentable in common years.
public export
interface Calendar calendar where
  DateRep : Type
  MonthRep : Year -> Type
  WeekdayRep : Type

  isValidDays : Integer -> Bool
  fromDays : (days : Integer) -> {auto 0 valid : So (isValidDays days)} -> DateRep
  toDaysFor : DateRep -> Integer
  calendarName : String

  year' : DateRep -> Year
  toYmd : (date : DateRep) -> (MonthRep (year' date), DayOfMonth)
  day' : DateRep -> DayOfMonth
  month' : (date : DateRep) -> MonthRep (year' date)

  applyCalendarPeriod' : Period target -> DateRep -> DateRep
  shiftCalendarDays' : Integer -> DateRep -> DateRep

  dayOfWeekFor : DateRep -> WeekdayRep
  nextFor : Integer -> WeekdayRep -> DateRep -> DateRep
  previousFor : Integer -> WeekdayRep -> DateRep -> DateRep

||| The opaque date representation selected by a calendar implementation.
public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

||| A date-like value that can participate in absolute-day calendar conversion.
public export
interface HasCalendarDate date where
  calendarDays : date -> Integer
  acceptsCalendarDays : Integer -> Bool
  calendarDateFromDays : (days : Integer) ->
                         {auto 0 valid : So (acceptsCalendarDays days)} -> date
  calendarDateName : String

||| Calendar operations determined by a concrete date representation.
||| This lets value-oriented APIs infer the calendar from their first date
||| argument instead of requiring a repeated `{calendar = ...}` annotation.
public export
interface CalendarValue date where
  CalendarMonth : Year -> Type
  CalendarWeekday : Type
  calendarValueToDays : date -> Integer
  calendarValueYear : date -> Year
  calendarValueMonth : (value : date) ->
                       CalendarMonth (calendarValueYear value)
  calendarValueDay : date -> DayOfMonth
  calendarValueDayOfWeek : date -> CalendarWeekday
  calendarValueBetweenWith :
    DateDifferencePolicy -> date -> date -> Period date

||| Weekday navigation determined by concrete weekday and date representations.
||| Both ordinary arguments are available before Idris resolves this interface,
||| so callers do not need to select the calendar explicitly.
public export
interface CalendarNavigation weekday date where
  calendarValueNext : Integer -> weekday -> date -> date
  calendarValuePrevious : Integer -> weekday -> date -> date

||| Extract the calendar year from a date.
public export
yearFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> Year
yearFor @{cal} = year' @{cal}

||| Extract the year-indexed calendar month from a date.
public export
monthFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
  (date : CalendarDate calendar @{cal}) ->
  MonthRep @{cal} (yearFor {calendar} @{cal} date)
monthFor @{cal} = month' @{cal}

||| Extract the day of month from a date.
public export
dayFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> DayOfMonth
dayFor @{cal} = day' @{cal}

export
applyCalendarPeriod : {calendar : Type} -> {auto cal : Calendar calendar} ->
                      Period target -> CalendarDate calendar @{cal} ->
                      CalendarDate calendar @{cal}
applyCalendarPeriod @{cal} = applyCalendarPeriod' @{cal}

export
shiftCalendarDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
                    Integer -> CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}
shiftCalendarDays @{cal} = shiftCalendarDays' @{cal}

||| Compute the exact signed day period from `start` to `end`.
public export
betweenDaysFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
              {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
              (start : CalendarDate calendar @{cal}) ->
              (end : CalendarDate calendar @{cal}) ->
              Period (CalendarDate calendar @{cal})
betweenDaysFor @{cal} start end = days (toDaysFor @{cal} end - toDaysFor @{cal} start)

yearsBetween : {calendar : Type} -> {auto cal : Calendar calendar} ->
               {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
               CalendarDate calendar @{cal} -> CalendarDate calendar @{cal} -> Integer
yearsBetween @{cal} start end =
  let estimate = yearValue (yearFor @{cal} end) - yearValue (yearFor @{cal} start)
      estimatedDate = applyCalendarPeriod @{cal}
        (years {target = CalendarDate calendar @{cal}} estimate) start
      estimatedDays = toDaysFor @{cal} estimatedDate
      startDays = toDaysFor @{cal} start
      endDays = toDaysFor @{cal} end
   in if startDays <= endDays
        then if estimatedDays <= endDays then estimate else estimate - 1
        else if estimatedDays >= endDays then estimate else estimate + 1

monthsBetween : {calendar : Type} -> {auto cal : Calendar calendar} ->
                {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
                CalendarDate calendar @{cal} -> CalendarDate calendar @{cal} -> Integer
monthsBetween @{cal} start end =
  let startDays = toDaysFor @{cal} start
      endDays = toDaysFor @{cal} end
      fuel = cast (abs (endDays - startDays) + 1)
   in if startDays <= endDays
        then forward fuel 0
        else backward fuel 0
  where
    forward : Nat -> Integer -> Integer
    forward Z count = count
    forward (S fuel) count =
      let candidate = count + 1
          candidateDays = toDaysFor @{cal}
            (applyCalendarPeriod @{cal}
              (months {target = CalendarDate calendar @{cal}} candidate) start)
       in if candidateDays <= toDaysFor @{cal} end
            then if candidateDays == toDaysFor @{cal} end
              then candidate
              else forward fuel candidate
            else count

    backward : Nat -> Integer -> Integer
    backward Z count = count
    backward (S fuel) count =
      let candidate = count - 1
          candidateDays = toDaysFor @{cal}
            (applyCalendarPeriod @{cal}
              (months {target = CalendarDate calendar @{cal}} candidate) start)
       in if candidateDays >= toDaysFor @{cal} end
            then if candidateDays == toDaysFor @{cal} end
              then candidate
              else backward fuel candidate
            else count

||| Decompose the signed difference from `start` to `end` according to `policy`.
||| Calendar units are selected largest-first without passing the endpoint.
public export
betweenWithFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
              {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
              DateDifferencePolicy ->
              (start : CalendarDate calendar @{cal}) ->
              (end : CalendarDate calendar @{cal}) ->
              Period (CalendarDate calendar @{cal})
betweenWithFor @{cal} (MkDateDifferencePolicy DaysOnly _) start end =
  betweenDaysFor @{cal} start end
betweenWithFor @{cal} (MkDateDifferencePolicy YearsMonthsDays ClampToMonth) start end =
  let yearCount = yearsBetween @{cal} start end
      afterYears = applyCalendarPeriod @{cal}
        (years {target = CalendarDate calendar @{cal}} yearCount) start
      monthCount = monthsBetween @{cal} afterYears end
      afterMonths = applyCalendarPeriod @{cal}
        (months {target = CalendarDate calendar @{cal}} monthCount) afterYears
      dayCount = toDaysFor @{cal} end - toDaysFor @{cal} afterMonths
   in years {target = CalendarDate calendar @{cal}} yearCount <+>
      months {target = CalendarDate calendar @{cal}} monthCount <+>
      days {target = CalendarDate calendar @{cal}} dayCount

||| Decompose the signed calendar difference using `nodaTimePolicy`.
public export
betweenFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
          {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
          (start : CalendarDate calendar @{cal}) ->
          (end : CalendarDate calendar @{cal}) ->
          Period (CalendarDate calendar @{cal})
betweenFor @{cal} = betweenWithFor @{cal} nodaTimePolicy

||| Decompose a date while preserving the dependency between its year and month.
public export
yearMonthDayFor : {calendar : Type} -> {auto cal : Calendar calendar} ->
               (date : CalendarDate calendar @{cal}) ->
               (valueYear : Year ** (MonthRep @{cal} valueYear, DayOfMonth))
yearMonthDayFor @{cal} date =
  (year' @{cal} date ** toYmd @{cal} date)

||| Compute the exact signed day period from `start` to `end`.
public export
betweenDays : (start : date) -> {auto value : CalendarValue date} ->
              date -> Period date
betweenDays start @{value} =
  calendarValueBetweenWith @{value}
    (MkDateDifferencePolicy DaysOnly ClampToMonth) start

||| Decompose a difference according to `policy`, inferring the calendar from
||| the first date argument.
public export
betweenWith : DateDifferencePolicy ->
              (start : date) -> {auto value : CalendarValue date} ->
              date -> Period date
betweenWith policy start @{value} =
  calendarValueBetweenWith @{value} policy start

||| Decompose a signed date difference using `nodaTimePolicy`.
public export
between : (start : date) -> {auto value : CalendarValue date} ->
          date -> Period date
between = betweenWith nodaTimePolicy

||| Decompose a date while preserving its year-indexed month type.
public export
yearMonthDay : (value : date) -> {auto rep : CalendarValue date} ->
               (valueYear : Year **
                 (CalendarMonth @{rep} valueYear, DayOfMonth))
yearMonthDay value @{rep} =
  (calendarValueYear @{rep} value **
    (calendarValueMonth @{rep} value, calendarValueDay @{rep} value))

||| Return the calendar-relative day count for a concrete date value.
public export
toDays : (value : date) -> {auto rep : CalendarValue date} -> Integer
toDays value @{rep} = calendarValueToDays @{rep} value

||| Extract the calendar year from a concrete date value.
public export
year : (value : date) -> {auto rep : CalendarValue date} -> Year
year value @{rep} = calendarValueYear @{rep} value

||| Extract the year-indexed calendar month from a concrete date value.
public export
month : (value : date) -> {auto rep : CalendarValue date} ->
        CalendarMonth @{rep} (calendarValueYear @{rep} value)
month value @{rep} = calendarValueMonth @{rep} value

||| Extract the day of month from a concrete date value.
public export
day : (value : date) -> {auto rep : CalendarValue date} -> DayOfMonth
day value @{rep} = calendarValueDay @{rep} value

||| Extract the calendar-specific weekday from a concrete date value.
public export
dayOfWeek : (value : date) -> {auto rep : CalendarValue date} ->
            CalendarWeekday @{rep}
dayOfWeek value @{rep} = calendarValueDayOfWeek @{rep} value

||| Find a matching weekday relative to a concrete date value.
public export
next : Integer -> weekday -> (value : date) ->
       {auto navigation : CalendarNavigation weekday date} -> date
next count weekday value @{navigation} =
  calendarValueNext @{navigation} count weekday value

||| Find a preceding matching weekday relative to a concrete date value.
public export
previous : Integer -> weekday -> (value : date) ->
           {auto navigation : CalendarNavigation weekday date} -> date
previous count weekday value @{navigation} =
  calendarValuePrevious @{navigation} count weekday value

||| Convert a date to another calendar while preserving its absolute day.
||| Returns `TargetCalendarOutOfRange` when the target cannot represent it.
public export
withCalendar : {sourceDate : Type} -> {targetDate : Type} ->
               {auto sourceRep : HasCalendarDate sourceDate} ->
               {auto targetRep : HasCalendarDate targetDate} ->
               sourceDate ->
               Either CalendarConversionError targetDate
withCalendar @{sourceRep} @{targetRep} date =
  let valueDays = calendarDays @{sourceRep} date
   in case choose (acceptsCalendarDays @{targetRep} valueDays) of
        Left valid => Right (calendarDateFromDays @{targetRep} valueDays @{valid})
        Right _ => Left
          (TargetCalendarOutOfRange (calendarDateName @{targetRep}) valueDays)