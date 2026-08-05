module IotaTime.CalendarDateTime

import IotaTime.Calendar
import IotaTime.LocalTime
import IotaTime.Period

%default total

nanosecondsPerDay : Integer
nanosecondsPerDay = 86400 * 1000000000

||| A calendar date paired with a local time of day.
public export
record CalendarDateTimeRep (calendar : Type) (cal : Calendar calendar) where
  constructor MkCalendarDateTime
  date : CalendarDate calendar @{cal}
  time : LocalTime

||| The date-time representation selected by a calendar implementation.
public export
CalendarDateTime : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDateTime calendar @{cal} = CalendarDateTimeRep calendar cal

||| Associate a local time with a date, using time-first argument order.
public export
on : {calendar : Type} -> {auto cal : Calendar calendar} ->
     LocalTime -> CalendarDate calendar @{cal} -> CalendarDateTime calendar @{cal}
on valueTime valueDate = MkCalendarDateTime valueDate valueTime

||| Associate a date with a local time, using date-first argument order.
public export
at : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> LocalTime -> CalendarDateTime calendar @{cal}
at valueDate valueTime = on valueTime valueDate

||| Associate a date with midnight at the start of that day.
public export
atStartOfDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
      CalendarDate calendar @{cal} -> CalendarDateTime calendar @{cal}
atStartOfDay valueDate = at valueDate (localTime 0 0 0 0)

||| Extract the calendar date component.
public export
datePart : {calendar : Type} -> {auto cal : Calendar calendar} ->
           CalendarDateTime calendar @{cal} -> CalendarDate calendar @{cal}
datePart = date

||| Extract the local time-of-day component.
public export
localTimeOfDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
                 CalendarDateTime calendar @{cal} -> LocalTime
localTimeOfDay = time

||| Convert the date component to another calendar while preserving the local
||| time of day and absolute day.
public export
withCalendar : {source : Type} -> {target : Type} ->
               {auto sourceCal : Calendar source} ->
               {auto targetCal : Calendar target} ->
               {auto sourceRep : HasCalendarDate (CalendarDate source @{sourceCal})} ->
               {auto targetRep : HasCalendarDate (CalendarDate target @{targetCal})} ->
               CalendarDateTime source @{sourceCal} ->
               Either CalendarConversionError (CalendarDateTime target @{targetCal})
withCalendar @{sourceCal} @{targetCal} @{sourceRep} @{targetRep} value =
  case IotaTime.Calendar.withCalendar @{sourceRep} @{targetRep} value.date of
    Left error => Left error
    Right convertedDate => Right (MkCalendarDateTime convertedDate value.time)

public export
implementation {calendar : Type} -> {cal : Calendar calendar} ->
  HasCalendar (CalendarDateTime calendar @{cal}) where
  calendarCapability = ()

public export
implementation {calendar : Type} -> {cal : Calendar calendar} ->
  HasTime (CalendarDateTime calendar @{cal}) where
  timeCapability = ()

public export
implementation {calendar : Type} -> {cal : Calendar calendar} ->
  ApplyPeriod (CalendarDateTime calendar @{cal}) where
  applyPeriod period value =
    let dateAfterPeriod = applyCalendarPeriod @{cal} period value.date
        (carry, timeAfterPeriod) = applyTimePeriodWithCarry period value.time
     in MkCalendarDateTime (shiftCalendarDays @{cal} carry dateAfterPeriod) timeAfterPeriod

||| Compute the exact signed period from `start` to `end`, treating each civil
||| calendar day as 24 hours.
public export
between : {calendar : Type} -> {auto cal : Calendar calendar} ->
          (start : CalendarDateTime calendar @{cal}) ->
          (end : CalendarDateTime calendar @{cal}) ->
          Period (CalendarDateTime calendar @{cal})
between @{cal} start end = nanoseconds
  ((toDays @{cal} end.date - toDays @{cal} start.date) * nanosecondsPerDay +
   toNanosecondsSinceMidnight end.time - toNanosecondsSinceMidnight start.time)