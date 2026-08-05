module IotaTime.Calendar

import public IotaTime.Calendar.Component
import IotaTime.Period
import Derive.Prelude

%language ElabReflection

||| Selects an occurrence of a weekday within a month.
public export
data DayNth = First | Second | Third | Fourth | Fifth | Last

%runElab derive `{DayNth} [Eq, Show]

||| A calendar conversion failed because the target calendar cannot represent
||| the source date's absolute day count.
public export
data CalendarConversionError = TargetCalendarOutOfRange String Integer

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
  toDays : DateRep -> Integer
  calendarName : String

  year' : DateRep -> Year
  toYmd : (date : DateRep) -> (MonthRep (year' date), DayOfMonth)
  day' : DateRep -> DayOfMonth
  month' : (date : DateRep) -> MonthRep (year' date)

  applyCalendarPeriod' : Period target -> DateRep -> DateRep
  shiftCalendarDays' : Integer -> DateRep -> DateRep

  dayOfWeek : DateRep -> WeekdayRep
  next : Integer -> WeekdayRep -> DateRep -> DateRep
  previous : Integer -> WeekdayRep -> DateRep -> DateRep

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

||| Extract the calendar year from a date.
public export
year : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> Year
year @{cal} = year' @{cal}

||| Extract the year-indexed calendar month from a date.
public export
month : {calendar : Type} -> {auto cal : Calendar calendar} ->
  (date : CalendarDate calendar @{cal}) ->
  MonthRep @{cal} (year {calendar} @{cal} date)
month @{cal} = month' @{cal}

||| Extract the day of month from a date.
public export
day : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> DayOfMonth
day @{cal} = day' @{cal}

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
between : {calendar : Type} -> {auto cal : Calendar calendar} ->
          {auto target : HasCalendar (CalendarDate calendar @{cal})} ->
          (start : CalendarDate calendar @{cal}) ->
          (end : CalendarDate calendar @{cal}) ->
          Period (CalendarDate calendar @{cal})
between @{cal} start end = days (toDays @{cal} end - toDays @{cal} start)

||| Decompose a date while preserving the dependency between its year and month.
public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               (date : CalendarDate calendar @{cal}) ->
               (valueYear : Year ** (MonthRep @{cal} valueYear, DayOfMonth))
yearMonthDay @{cal} date =
  (year' @{cal} date ** toYmd @{cal} date)

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