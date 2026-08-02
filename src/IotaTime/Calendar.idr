module IotaTime.Calendar

import public IotaTime.Calendar.Component

public export
data DayNth = First | Second | Third | Fourth | Fifth | Last

public export
interface Calendar calendar where
  DateRep : Type
  MonthRep : Type
  WeekdayRep : Type

  isValidDays : Integer -> Bool
  fromDays : (days : Integer) -> {auto 0 valid : So (isValidDays days)} -> DateRep
  toDays : DateRep -> Integer
  toYmd : DateRep -> (Year, MonthRep, DayOfMonth)
  calendarName : String

  day' : DateRep -> DayOfMonth
  month' : DateRep -> MonthRep
  year' : DateRep -> Year

  dayOfWeek : DateRep -> WeekdayRep
  next : Integer -> WeekdayRep -> DateRep -> DateRep
  previous : Integer -> WeekdayRep -> DateRep -> DateRep

public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

public export
day : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> DayOfMonth
day @{cal} = day' @{cal}

public export
month : {calendar : Type} -> {auto cal : Calendar calendar} -> CalendarDate calendar @{cal} -> MonthRep @{cal}
month @{cal} = month' @{cal}

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} ->
  CalendarDate calendar @{cal} -> Year
year @{cal} = year' @{cal}

public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               CalendarDate calendar @{cal} -> (Year, MonthRep @{cal}, DayOfMonth)
yearMonthDay @{cal} = toYmd @{cal}