module IotaTime.Calendar

import public IotaTime.Optics

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
  isValidDate' : DateRep -> Bool
  calendarName : String

  day' : Lens' DateRep DayOfMonth
  month' : DateRep -> MonthRep
  monthl' : Lens' DateRep Integer
  year' : Lens' DateRep Year

  dayOfWeek : DateRep -> WeekdayRep
  next : Integer -> WeekdayRep -> DateRep -> DateRep
  previous : Integer -> WeekdayRep -> DateRep -> DateRep

public export
CalendarDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
CalendarDate calendar @{cal} = DateRep @{cal}

export
data ValidatedDate : (calendar : Type) -> {auto cal : Calendar calendar} -> Type where
  MkValidatedDate : CalendarDate calendar @{cal} -> ValidatedDate calendar @{cal}

public export
validatedEquals : {calendar : Type} -> {auto cal : Calendar calendar} ->
                  Eq (CalendarDate calendar @{cal}) =>
                  ValidatedDate calendar @{cal} -> ValidatedDate calendar @{cal} -> Bool
validatedEquals (MkValidatedDate left) (MkValidatedDate right) = left == right

public export
validatedCompare : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   Ord (CalendarDate calendar @{cal}) =>
                   ValidatedDate calendar @{cal} -> ValidatedDate calendar @{cal} -> Ordering
validatedCompare (MkValidatedDate left) (MkValidatedDate right) = compare left right

public export
validateDate : {calendar : Type} -> {auto cal : Calendar calendar} ->
               CalendarDate calendar @{cal} -> Maybe (ValidatedDate calendar @{cal})
validateDate @{cal} date =
  if isValidDate' @{cal} date then Just (MkValidatedDate date) else Nothing

public export
validatedFromDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
                    Integer -> Maybe (ValidatedDate calendar @{cal})
validatedFromDays @{cal} = validateDate @{cal} . fromDays @{cal}

public export
forgetValidation : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   ValidatedDate calendar @{cal} -> CalendarDate calendar @{cal}
forgetValidation (MkValidatedDate date) = date

public export
validatedToDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
                  ValidatedDate calendar @{cal} -> Integer
validatedToDays @{cal} = toDays @{cal} . forgetValidation

public export
validatedYearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
                        ValidatedDate calendar @{cal} ->
                        (Year, MonthRep @{cal}, DayOfMonth)
validatedYearMonthDay @{cal} = toYmd @{cal} . forgetValidation

public export
validatedDayOfWeek : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     ValidatedDate calendar @{cal} -> WeekdayRep @{cal}
validatedDayOfWeek @{cal} = dayOfWeek @{cal} . forgetValidation

public export
updateValidated : {calendar : Type} -> {auto cal : Calendar calendar} ->
                  (CalendarDate calendar @{cal} -> CalendarDate calendar @{cal}) ->
                  ValidatedDate calendar @{cal} -> Maybe (ValidatedDate calendar @{cal})
updateValidated @{cal} transform = validateDate @{cal} . transform . forgetValidation

public export
nextValidated : {calendar : Type} -> {auto cal : Calendar calendar} ->
                Integer -> WeekdayRep @{cal} ->
                ValidatedDate calendar @{cal} -> Maybe (ValidatedDate calendar @{cal})
nextValidated @{cal} count target = updateValidated @{cal} (next @{cal} count target)

public export
previousValidated : {calendar : Type} -> {auto cal : Calendar calendar} ->
                    Integer -> WeekdayRep @{cal} ->
                    ValidatedDate calendar @{cal} -> Maybe (ValidatedDate calendar @{cal})
previousValidated @{cal} count target =
  updateValidated @{cal} (previous @{cal} count target)

public export
day : {calendar : Type} -> {auto cal : Calendar calendar} -> Lens' (CalendarDate calendar @{cal}) DayOfMonth
day @{cal} = day' @{cal}

public export
month : {calendar : Type} -> {auto cal : Calendar calendar} -> CalendarDate calendar @{cal} -> MonthRep @{cal}
month @{cal} = month' @{cal}

public export
monthl : {calendar : Type} -> {auto cal : Calendar calendar} -> Lens' (CalendarDate calendar @{cal}) Integer
monthl @{cal} = monthl' @{cal}

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} -> Lens' (CalendarDate calendar @{cal}) Year
year @{cal} = year' @{cal}

public export
yearMonthDay : {calendar : Type} -> {auto cal : Calendar calendar} ->
               CalendarDate calendar @{cal} -> (Year, MonthRep @{cal}, DayOfMonth)
yearMonthDay @{cal} = toYmd @{cal}