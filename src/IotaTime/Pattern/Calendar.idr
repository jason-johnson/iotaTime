module IotaTime.Pattern.Calendar

import Data.Fin
import Data.Vect
import IotaTime.Calendar
import IotaTime.Calendar.Coptic
import IotaTime.Calendar.Gregorian
import IotaTime.Calendar.Hebrew
import IotaTime.Calendar.Islamic
import IotaTime.Calendar.Julian
import IotaTime.Calendar.Persian
import IotaTime.Pattern

%default total

||| Selects whether locale-backed patterns use the locale's twelve Gregorian
||| month names or the selected calendar's canonical names.
public export
data PatternMonthNameSource : Nat -> Type where
  GregorianLocaleMonthNames : {monthCount : Nat} ->
    (0 countIsTwelve : monthCount = 12) ->
    PatternMonthNameSource monthCount
  CanonicalCalendarMonthNames : {monthCount : Nat} ->
    PatternMonthNameSource monthCount

||| Calendar-specific projection and runtime refinement used by date patterns.
public export
interface Calendar calendar => CalendarPattern calendar where
  patternMonthCount : Nat
  patternMonthNames : Vect patternMonthCount String
  patternMonthAbbreviations : Vect patternMonthCount String
  patternMonthNameSource : PatternMonthNameSource patternMonthCount
  patternMonthNameSource = CanonicalCalendarMonthNames
  patternMonthIndex : CalendarDate calendar -> Fin patternMonthCount
  patternWeekdayIndex : CalendarDate calendar -> Fin 7
  refinePatternDate : Integer -> Integer -> Integer ->
    Either PatternError (CalendarDate calendar)

refineDay : Integer -> Either PatternError DayOfMonth
refineDay value = case refineDayOfMonth value of
  Left _ => Left (InvalidValue "day is outside 1-31")
  Right day => Right day

refineMonth : Nat -> Integer -> Either PatternError Integer
refineMonth monthCount value =
  let maximum = cast monthCount
  in if value >= 1 && value <= maximum
    then Right value
    else Left (ValueOutOfRange "month" 1 maximum value)

invalidDate : String -> Either PatternError value
invalidDate name = Left (InvalidValue ("invalid " ++ name ++ " date"))

refineTwelveMonthDate : String ->
  (Integer -> monthType) ->
  (DayOfMonth -> monthType -> Year -> Either error result) ->
  Integer -> Integer -> Integer -> Either PatternError result
refineTwelveMonthDate calendarName toMonth refineDate year month day = do
  valueMonth <- refineMonth 12 month
  valueDay <- refineDay day
  case refineDate valueDay (toMonth valueMonth) (yearFromInteger year) of
    Left _ => invalidDate calendarName
    Right date => Right date

abbreviate : String -> String
abbreviate = substr 0 3

abbreviateAll : {monthCount : Nat} ->
                Vect monthCount String -> Vect monthCount String
abbreviateAll = map abbreviate

gregorianMonthNames : Vect 12 String
gregorianMonthNames =
  [ "January", "February", "March", "April", "May", "June"
  , "July", "August", "September", "October", "November", "December"
  ]

monthIndex12 : Integer -> Fin 12
monthIndex12 1 = 0
monthIndex12 2 = 1
monthIndex12 3 = 2
monthIndex12 4 = 3
monthIndex12 5 = 4
monthIndex12 6 = 5
monthIndex12 7 = 6
monthIndex12 8 = 7
monthIndex12 9 = 8
monthIndex12 10 = 9
monthIndex12 11 = 10
monthIndex12 _ = 11

monthIndex13 : Integer -> Fin 13
monthIndex13 1 = 0
monthIndex13 2 = 1
monthIndex13 3 = 2
monthIndex13 4 = 3
monthIndex13 5 = 4
monthIndex13 6 = 5
monthIndex13 7 = 6
monthIndex13 8 = 7
monthIndex13 9 = 8
monthIndex13 10 = 9
monthIndex13 11 = 10
monthIndex13 12 = 11
monthIndex13 _ = 12

weekdayIndex7 : Integer -> Fin 7
weekdayIndex7 0 = 0
weekdayIndex7 1 = 1
weekdayIndex7 2 = 2
weekdayIndex7 3 = 3
weekdayIndex7 4 = 4
weekdayIndex7 5 = 5
weekdayIndex7 _ = 6

gregorianMonth : Integer -> Month
gregorianMonth 1 = January
gregorianMonth 2 = February
gregorianMonth 3 = March
gregorianMonth 4 = April
gregorianMonth 5 = May
gregorianMonth 6 = June
gregorianMonth 7 = July
gregorianMonth 8 = August
gregorianMonth 9 = September
gregorianMonth 10 = October
gregorianMonth 11 = November
gregorianMonth _ = December

public export
CalendarPattern Gregorian where
  patternMonthCount = 12
  patternMonthNames = gregorianMonthNames
  patternMonthAbbreviations = abbreviateAll gregorianMonthNames
  patternMonthNameSource = GregorianLocaleMonthNames Refl
  patternMonthIndex date = monthIndex12
    (IotaTime.Calendar.Gregorian.monthNumber
      (month {calendar = Gregorian} date))
  patternWeekdayIndex date = weekdayIndex7
    (weekdayNumber (dayOfWeek {calendar = Gregorian} date))
  refinePatternDate year month day = do
    valueMonth <- refineMonth 12 month
    valueDay <- refineDay day
    case IotaTime.Calendar.Gregorian.refineDate
      valueDay (gregorianMonth valueMonth)
      (yearFromInteger year) of
      Left _ => invalidDate "Gregorian"
      Right date => Right date

julianMonth : Integer -> JulianMonth
julianMonth 1 = JulianMonths.January
julianMonth 2 = JulianMonths.February
julianMonth 3 = JulianMonths.March
julianMonth 4 = JulianMonths.April
julianMonth 5 = JulianMonths.May
julianMonth 6 = JulianMonths.June
julianMonth 7 = JulianMonths.July
julianMonth 8 = JulianMonths.August
julianMonth 9 = JulianMonths.September
julianMonth 10 = JulianMonths.October
julianMonth 11 = JulianMonths.November
julianMonth _ = JulianMonths.December

public export
CalendarPattern Julian where
  patternMonthCount = 12
  patternMonthNames = gregorianMonthNames
  patternMonthAbbreviations = abbreviateAll gregorianMonthNames
  patternMonthNameSource = GregorianLocaleMonthNames Refl
  patternMonthIndex date = monthIndex12
    (JulianMonths.monthNumber (month {calendar = Julian} date))
  patternWeekdayIndex date = weekdayIndex7
    (JulianWeekdays.weekdayNumber (dayOfWeek {calendar = Julian} date))
  refinePatternDate year month day = do
    valueMonth <- refineMonth 12 month
    valueDay <- refineDay day
    case IotaTime.Calendar.Julian.refineDate
      valueDay (julianMonth valueMonth)
      (yearFromInteger year) of
      Left _ => invalidDate "Julian"
      Right date => Right date

copticMonth : Integer -> CopticMonth
copticMonth 1 = CopticMonths.Thout
copticMonth 2 = CopticMonths.Paopi
copticMonth 3 = CopticMonths.Hathor
copticMonth 4 = CopticMonths.Koiak
copticMonth 5 = CopticMonths.Tobi
copticMonth 6 = CopticMonths.Meshir
copticMonth 7 = CopticMonths.Paremhat
copticMonth 8 = CopticMonths.Paremoude
copticMonth 9 = CopticMonths.Pashons
copticMonth 10 = CopticMonths.Paoni
copticMonth 11 = CopticMonths.Epip
copticMonth 12 = CopticMonths.Mesori
copticMonth _ = CopticMonths.PiKogiEnavot

public export
CalendarPattern Coptic where
  patternMonthCount = 13
  patternMonthNames =
    [ "Thout", "Paopi", "Hathor", "Koiak", "Tobi", "Meshir"
    , "Paremhat", "Paremoude", "Pashons", "Paoni", "Epip", "Mesori"
    , "PiKogiEnavot"
    ]
  patternMonthAbbreviations =
    [ "Tho", "Pao", "Hat", "Koi", "Tob", "Mes", "Par", "Pmd"
    , "Pas", "Pni", "Epi", "Mso", "PKN"
    ]
  patternMonthIndex date = monthIndex13
    (CopticMonths.monthNumber (month {calendar = Coptic} date))
  patternWeekdayIndex date = weekdayIndex7
    (CopticWeekdays.weekdayNumber (dayOfWeek {calendar = Coptic} date))
  refinePatternDate year month day = do
    valueMonth <- refineMonth 13 month
    valueDay <- refineDay day
    case IotaTime.Calendar.Coptic.refineDate
      valueDay (copticMonth valueMonth)
      (yearFromInteger year) of
      Left _ => invalidDate "Coptic"
      Right date => Right date

islamicMonth : Integer -> IslamicMonth
islamicMonth 1 = IslamicMonths.Muharram
islamicMonth 2 = IslamicMonths.Safar
islamicMonth 3 = IslamicMonths.RabiAlAwwal
islamicMonth 4 = IslamicMonths.RabiAlThani
islamicMonth 5 = IslamicMonths.JumadaAlAwwal
islamicMonth 6 = IslamicMonths.JumadaAlThani
islamicMonth 7 = IslamicMonths.Rajab
islamicMonth 8 = IslamicMonths.Shaban
islamicMonth 9 = IslamicMonths.Ramadan
islamicMonth 10 = IslamicMonths.Shawwal
islamicMonth 11 = IslamicMonths.DhulQadah
islamicMonth _ = IslamicMonths.DhulHijjah

islamicMonthNames : Vect 12 String
islamicMonthNames =
  [ "Muharram", "Safar", "RabiAlAwwal", "RabiAlThani"
  , "JumadaAlAwwal", "JumadaAlThani", "Rajab", "Shaban"
  , "Ramadan", "Shawwal", "DhulQadah", "DhulHijjah"
  ]

islamicMonthAbbreviations : Vect 12 String
islamicMonthAbbreviations =
  [ "Muh", "Saf", "RaA", "RaT", "JuA", "JuT"
  , "Raj", "Sha", "Ram", "Shw", "DhQ", "DhH"
  ]

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  CalendarPattern (Islamic pattern) where
  patternMonthCount = 12
  patternMonthNames = islamicMonthNames
  patternMonthAbbreviations = islamicMonthAbbreviations
  patternMonthIndex date = monthIndex12 (IslamicMonths.monthNumber
    (month {calendar = Islamic pattern} date))
  patternWeekdayIndex date = weekdayIndex7 (IslamicWeekdays.weekdayNumber
    (dayOfWeek {calendar = Islamic pattern} date))
  refinePatternDate = refineTwelveMonthDate "Islamic" islamicMonth
    (IotaTime.Calendar.Islamic.refineDate' {pattern})

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  CalendarPattern (CivilIslamic pattern) where
  patternMonthCount = 12
  patternMonthNames = islamicMonthNames
  patternMonthAbbreviations = islamicMonthAbbreviations
  patternMonthIndex date = monthIndex12 (IslamicMonths.monthNumber
    (month {calendar = CivilIslamic pattern} date))
  patternWeekdayIndex date = weekdayIndex7 (IslamicWeekdays.weekdayNumber
    (dayOfWeek {calendar = CivilIslamic pattern} date))
  refinePatternDate = refineTwelveMonthDate "Civil Islamic" islamicMonth
    (refineCivilDate' {pattern})

persianMonth : Integer -> PersianMonth
persianMonth 1 = PersianMonths.Farvardin
persianMonth 2 = PersianMonths.Ordibehesht
persianMonth 3 = PersianMonths.Khordad
persianMonth 4 = PersianMonths.Tir
persianMonth 5 = PersianMonths.Mordad
persianMonth 6 = PersianMonths.Shahrivar
persianMonth 7 = PersianMonths.Mehr
persianMonth 8 = PersianMonths.Aban
persianMonth 9 = PersianMonths.Azar
persianMonth 10 = PersianMonths.Dey
persianMonth 11 = PersianMonths.Bahman
persianMonth _ = PersianMonths.Esfand

persianMonthNames : Vect 12 String
persianMonthNames =
  [ "Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar"
  , "Mehr", "Aban", "Azar", "Dey", "Bahman", "Esfand"
  ]

persianMonthAbbreviations : Vect 12 String
persianMonthAbbreviations =
  [ "Far", "Ord", "Kho", "Tir", "Mor", "Sha"
  , "Meh", "Aba", "Aza", "Dey", "Bah", "Esf"
  ]

public export
CalendarPattern Persian where
  patternMonthCount = 12
  patternMonthNames = persianMonthNames
  patternMonthAbbreviations = persianMonthAbbreviations
  patternMonthIndex date = monthIndex12
    (PersianMonths.monthNumber (month {calendar = Persian} date))
  patternWeekdayIndex date = weekdayIndex7
    (PersianWeekdays.weekdayNumber (dayOfWeek {calendar = Persian} date))
  refinePatternDate = refineTwelveMonthDate "Persian" persianMonth
    IotaTime.Calendar.Persian.refineDate

public export
{rule : PersianArithmeticRule} -> KnownPersianArithmeticRule rule =>
  CalendarPattern (ArithmeticPersian rule) where
  patternMonthCount = 12
  patternMonthNames = persianMonthNames
  patternMonthAbbreviations = persianMonthAbbreviations
  patternMonthIndex date = monthIndex12 (PersianMonths.monthNumber
    (month {calendar = ArithmeticPersian rule} date))
  patternWeekdayIndex date = weekdayIndex7 (PersianWeekdays.weekdayNumber
    (dayOfWeek {calendar = ArithmeticPersian rule} date))
  refinePatternDate = refineTwelveMonthDate
    (ruleName {rule}) persianMonth
    (refineArithmeticRuleDate {rule})

hebrewMonthName : {numbering : HebrewNumbering} ->
                  KnownHebrewNumbering numbering => Integer -> HebrewMonthName
hebrewMonthName {numbering} value =
  case (value - 1 + numberingStart {numbering}) `mod` 13 of
    0 => TishriName
    1 => CheshvanName
    2 => KislevName
    3 => TevetName
    4 => ShevatName
    5 => AdarIName
    6 => AdarName
    7 => NisanName
    8 => IyarName
    9 => SivanName
    10 => TammuzName
    11 => AvName
    _ => ElulName

hebrewMonthLabel : HebrewMonthName -> String
hebrewMonthLabel TishriName = "Tishri"
hebrewMonthLabel CheshvanName = "Cheshvan"
hebrewMonthLabel KislevName = "Kislev"
hebrewMonthLabel TevetName = "Tevet"
hebrewMonthLabel ShevatName = "Shevat"
hebrewMonthLabel AdarIName = "AdarI"
hebrewMonthLabel AdarName = "Adar"
hebrewMonthLabel NisanName = "Nisan"
hebrewMonthLabel IyarName = "Iyar"
hebrewMonthLabel SivanName = "Sivan"
hebrewMonthLabel TammuzName = "Tammuz"
hebrewMonthLabel AvName = "Av"
hebrewMonthLabel ElulName = "Elul"

hebrewNames : {numbering : HebrewNumbering} ->
              KnownHebrewNumbering numbering => Vect 13 String
hebrewNames {numbering} =
  [ hebrewMonthLabel (hebrewMonthName {numbering} 1)
  , hebrewMonthLabel (hebrewMonthName {numbering} 2)
  , hebrewMonthLabel (hebrewMonthName {numbering} 3)
  , hebrewMonthLabel (hebrewMonthName {numbering} 4)
  , hebrewMonthLabel (hebrewMonthName {numbering} 5)
  , hebrewMonthLabel (hebrewMonthName {numbering} 6)
  , hebrewMonthLabel (hebrewMonthName {numbering} 7)
  , hebrewMonthLabel (hebrewMonthName {numbering} 8)
  , hebrewMonthLabel (hebrewMonthName {numbering} 9)
  , hebrewMonthLabel (hebrewMonthName {numbering} 10)
  , hebrewMonthLabel (hebrewMonthName {numbering} 11)
  , hebrewMonthLabel (hebrewMonthName {numbering} 12)
  , hebrewMonthLabel (hebrewMonthName {numbering} 13)
  ]

public export
{numbering : HebrewNumbering} -> KnownHebrewNumbering numbering =>
  CalendarPattern (Hebrew numbering) where
  patternMonthCount = 13
  patternMonthNames = hebrewNames {numbering}
  patternMonthAbbreviations = case numberingStart {numbering} of
    0 => [ "Tis", "Che", "Kis", "Tev", "She", "AdI", "Ada"
      , "Nis", "Iya", "Siv", "Tam", "Av", "Elu" ]
    _ => [ "Nis", "Iya", "Siv", "Tam", "Av", "Elu", "Tis"
      , "Che", "Kis", "Tev", "She", "AdI", "Ada" ]
  patternMonthIndex date = case yearMonthDayFor {calendar = Hebrew numbering} date of
    (_ ** (valueMonth, _)) => monthIndex13
      (IotaTime.Calendar.Hebrew.monthNumber valueMonth)
  patternWeekdayIndex date = weekdayIndex7 (HebrewWeekdays.weekdayNumber
    (dayOfWeek {calendar = Hebrew numbering} date))
  refinePatternDate year month day = do
    valueMonth <- refineMonth 13 month
    valueDay <- refineDay day
    case IotaTime.Calendar.Hebrew.refineDate' {numbering} valueDay
      (hebrewMonthName {numbering} valueMonth) (yearFromInteger year) of
        Left _ => invalidDate "Hebrew"
        Right date => Right date
