module IotaTime.Calendar

public export
Year : Type
Year = Integer

public export
DayOfMonth : Type
DayOfMonth = Integer

public export
WeekNumber : Type
WeekNumber = Integer

public export
data DayNth = First | Second | Third | Fourth | Fifth | Last

public export
interface Calendar calendar where
  DateRep : Type
  MonthRep : Type
  WeekdayRep : Type

  fromDays : Integer -> DateRep
  toDays : DateRep -> Integer
  toYmd : DateRep -> (Year, MonthRep, DayOfMonth)
  calendarName : String

  modifyDay : (DayOfMonth -> DayOfMonth) -> DateRep -> DateRep
  month : DateRep -> MonthRep
  modifyMonth : (Integer -> Integer) -> DateRep -> DateRep
  modifyYear : (Year -> Year) -> DateRep -> DateRep

  dayOfWeek : DateRep -> WeekdayRep
  next : Integer -> WeekdayRep -> DateRep -> DateRep
  previous : Integer -> WeekdayRep -> DateRep -> DateRep

public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

public export
day : {calendar : Type} -> {auto cal : Calendar calendar} -> CalendarDate calendar @{cal} -> DayOfMonth
day @{cal} date = let (_, _, value) = toYmd @{cal} date in value

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} -> CalendarDate calendar @{cal} -> Year
year @{cal} date = let (value, _, _) = toYmd @{cal} date in value

public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               CalendarDate calendar @{cal} -> (Year, MonthRep @{cal}, DayOfMonth)
yearMonthDay @{cal} = toYmd @{cal}