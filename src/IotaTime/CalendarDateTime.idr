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

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  Eq (CalendarDate calendar @{cal}) =>
  Eq (CalendarDateTimeRep calendar cal) where
  MkCalendarDateTime leftDate leftTime ==
    MkCalendarDateTime rightDate rightTime =
      leftDate == rightDate && leftTime == rightTime

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  Ord (CalendarDate calendar @{cal}) =>
  Ord (CalendarDateTimeRep calendar cal) where
  compare left right = case compare left.date right.date of
    EQ => compare left.time right.time
    ordering => ordering

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  Show (CalendarDate calendar @{cal}) =>
  Show (CalendarDateTimeRep calendar cal) where
  show value = "at (" ++ show value.date ++ ") (" ++ show value.time ++ ")"

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

||| Extracting the date after construction returns the supplied date.
public export
atDatePart : {calendar : Type} -> {auto cal : Calendar calendar} ->
             (valueDate : CalendarDate calendar @{cal}) ->
             (valueTime : LocalTime) ->
             datePart @{cal} (at @{cal} valueDate valueTime) = valueDate
atDatePart _ _ = Refl

||| Extracting the local time after construction returns the supplied time.
public export
atLocalTimeOfDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   (valueDate : CalendarDate calendar @{cal}) ->
                   (valueTime : LocalTime) ->
                   localTimeOfDay @{cal} (at @{cal} valueDate valueTime) = valueTime
atLocalTimeOfDay _ _ = Refl

||| Reconstructing a calendar date-time from its projections is exact.
public export
calendarDateTimeRoundTrip :
  {calendar : Type} -> {auto cal : Calendar calendar} ->
  (value : CalendarDateTime calendar @{cal}) ->
  at @{cal} (datePart @{cal} value) (localTimeOfDay @{cal} value) = value
calendarDateTimeRoundTrip (MkCalendarDateTime _ _) = Refl

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
  ((toDaysFor @{cal} end.date - toDaysFor @{cal} start.date) * nanosecondsPerDay +
   toNanosecondsSinceMidnight end.time - toNanosecondsSinceMidnight start.time)