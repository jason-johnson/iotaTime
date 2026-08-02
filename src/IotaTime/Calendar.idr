module IotaTime.Calendar

import public IotaTime.Calendar.Component
import IotaTime.Period

public export
data DayNth = First | Second | Third | Fourth | Fifth | Last

public export
data CalendarConversionError = TargetCalendarOutOfRange String Integer

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

public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

public export
interface HasCalendarDate date where
  calendarDays : date -> Integer
  acceptsCalendarDays : Integer -> Bool
  calendarDateFromDays : (days : Integer) ->
                         {auto 0 valid : So (acceptsCalendarDays days)} -> date
  calendarDateName : String

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> Year
year @{cal} = year' @{cal}

public export
month : {calendar : Type} -> {auto cal : Calendar calendar} ->
  (date : CalendarDate calendar @{cal}) ->
  MonthRep @{cal} (year {calendar} @{cal} date)
month @{cal} = month' @{cal}

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

public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               (date : CalendarDate calendar @{cal}) ->
               (valueYear : Year ** (MonthRep @{cal} valueYear, DayOfMonth))
yearMonthDay @{cal} date =
  (year' @{cal} date ** toYmd @{cal} date)

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