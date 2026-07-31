module IotaTime.Calendar

import public IotaTime.Optics
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

  day' : Lens' DateRep Integer
  month' : DateRep -> MonthRep
  monthl' : Lens' DateRep Integer
  year' : Lens' DateRep Year

  normalizeDay' : Integer -> DateRep -> DateRep
  shiftDays' : Integer -> DateRep -> DateRep
  normalizeMonth' : Integer -> DateRep -> DateRep
  shiftMonths' : Integer -> DateRep -> DateRep

  dayOfWeek : DateRep -> WeekdayRep
  next : Integer -> WeekdayRep -> DateRep -> DateRep
  previous : Integer -> WeekdayRep -> DateRep -> DateRep

public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

public export
day : {calendar : Type} -> {auto cal : Calendar calendar} ->
  Lens' (CalendarDate calendar @{cal}) Integer
day @{cal} = day' @{cal}

public export
month : {calendar : Type} -> {auto cal : Calendar calendar} -> CalendarDate calendar @{cal} -> MonthRep @{cal}
month @{cal} = month' @{cal}

public export
monthl : {calendar : Type} -> {auto cal : Calendar calendar} ->
         Lens' (CalendarDate calendar @{cal}) Integer
monthl @{cal} = monthl' @{cal}

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} -> Lens' (CalendarDate calendar @{cal}) Year
year @{cal} = year' @{cal}

public export
normalizeDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               Integer -> CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}
normalizeDay @{cal} = normalizeDay' @{cal}

public export
shiftDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
            Integer -> CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}
shiftDays @{cal} = shiftDays' @{cal}

public export
normalizeMonth : {calendar : Type} -> {auto cal : Calendar calendar} ->
                 Integer -> CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}
normalizeMonth @{cal} = normalizeMonth' @{cal}

public export
shiftMonths : {calendar : Type} -> {auto cal : Calendar calendar} ->
              Integer -> CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}
shiftMonths @{cal} = shiftMonths' @{cal}

public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               CalendarDate calendar @{cal} -> (Year, MonthRep @{cal}, DayOfMonth)
yearMonthDay @{cal} = toYmd @{cal}