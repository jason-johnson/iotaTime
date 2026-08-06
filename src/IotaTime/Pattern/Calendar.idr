module IotaTime.Pattern.Calendar

import IotaTime.Calendar
import IotaTime.Calendar.Coptic
import IotaTime.Calendar.Gregorian
import IotaTime.Calendar.Hebrew
import IotaTime.Calendar.Islamic
import IotaTime.Calendar.Julian
import IotaTime.Calendar.Persian
import IotaTime.Pattern

%default total

||| Calendar-specific projection and runtime refinement used by date patterns.
public export
interface Calendar calendar => CalendarPattern calendar where
  patternMonthLimit : Integer
  patternMonthNames : List String
  patternMonthAbbreviations : List String
  patternMonthNumber : CalendarDate calendar -> Integer
  patternWeekdayNumber : CalendarDate calendar -> Integer
  refinePatternDate : Integer -> Integer -> Integer ->
    Either PatternError (CalendarDate calendar)

refineDay : Integer -> Either PatternError DayOfMonth
refineDay value = case refineDayOfMonth value of
  Left _ => Left (InvalidValue "day is outside 1-31")
  Right day => Right day

invalidDate : String -> Either PatternError value
invalidDate name = Left (InvalidValue ("invalid " ++ name ++ " date"))

abbreviate : String -> String
abbreviate name = substr 0 3 name

abbreviateAll : List String -> List String
abbreviateAll = map abbreviate

gregorianMonthNames : List String
gregorianMonthNames =
  [ "January", "February", "March", "April", "May", "June"
  , "July", "August", "September", "October", "November", "December"
  ]

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
  patternMonthLimit = 12
  patternMonthNames = gregorianMonthNames
  patternMonthAbbreviations = abbreviateAll gregorianMonthNames
  patternMonthNumber date = IotaTime.Calendar.Gregorian.monthNumber
    (month {calendar = Gregorian} date)
  patternWeekdayNumber date = weekdayNumber (dayOfWeek {calendar = Gregorian} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineGregorianDate valueDay (gregorianMonth month) (yearFromInteger year) of
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
  patternMonthLimit = 12
  patternMonthNames = gregorianMonthNames
  patternMonthAbbreviations = abbreviateAll gregorianMonthNames
  patternMonthNumber date = JulianMonths.monthNumber (month {calendar = Julian} date)
  patternWeekdayNumber date = JulianWeekdays.weekdayNumber (dayOfWeek {calendar = Julian} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineJulianDate valueDay (julianMonth month) (yearFromInteger year) of
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
  patternMonthLimit = 13
  patternMonthNames =
    [ "Thout", "Paopi", "Hathor", "Koiak", "Tobi", "Meshir"
    , "Paremhat", "Paremoude", "Pashons", "Paoni", "Epip", "Mesori"
    , "PiKogiEnavot"
    ]
  patternMonthAbbreviations =
    [ "Tho", "Pao", "Hat", "Koi", "Tob", "Mes", "Par", "Pmd"
    , "Pas", "Pni", "Epi", "Mso", "PKN"
    ]
  patternMonthNumber date = CopticMonths.monthNumber (month {calendar = Coptic} date)
  patternWeekdayNumber date = CopticWeekdays.weekdayNumber (dayOfWeek {calendar = Coptic} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineCopticDate valueDay (copticMonth month) (yearFromInteger year) of
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

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  CalendarPattern (Islamic pattern) where
  patternMonthLimit = 12
  patternMonthNames =
    [ "Muharram", "Safar", "RabiAlAwwal", "RabiAlThani"
    , "JumadaAlAwwal", "JumadaAlThani", "Rajab", "Shaban"
    , "Ramadan", "Shawwal", "DhulQadah", "DhulHijjah"
    ]
  patternMonthAbbreviations =
    [ "Muh", "Saf", "RaA", "RaT", "JuA", "JuT"
    , "Raj", "Sha", "Ram", "Shw", "DhQ", "DhH"
    ]
  patternMonthNumber date = IslamicMonths.monthNumber
    (month {calendar = Islamic pattern} date)
  patternWeekdayNumber date = IslamicWeekdays.weekdayNumber
    (dayOfWeek {calendar = Islamic pattern} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineIslamicDate' {pattern} valueDay (islamicMonth month)
      (yearFromInteger year) of
        Left _ => invalidDate "Islamic"
        Right date => Right date

public export
{pattern : IslamicLeapPattern} -> KnownIslamicLeapPattern pattern =>
  CalendarPattern (CivilIslamic pattern) where
  patternMonthLimit = 12
  patternMonthNames =
    [ "Muharram", "Safar", "RabiAlAwwal", "RabiAlThani"
    , "JumadaAlAwwal", "JumadaAlThani", "Rajab", "Shaban"
    , "Ramadan", "Shawwal", "DhulQadah", "DhulHijjah"
    ]
  patternMonthAbbreviations =
    [ "Muh", "Saf", "RaA", "RaT", "JuA", "JuT"
    , "Raj", "Sha", "Ram", "Shw", "DhQ", "DhH"
    ]
  patternMonthNumber date = IslamicMonths.monthNumber
    (month {calendar = CivilIslamic pattern} date)
  patternWeekdayNumber date = IslamicWeekdays.weekdayNumber
    (dayOfWeek {calendar = CivilIslamic pattern} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineCivilIslamicDate' {pattern} valueDay (islamicMonth month)
      (yearFromInteger year) of
        Left _ => invalidDate "Civil Islamic"
        Right date => Right date

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

public export
CalendarPattern Persian where
  patternMonthLimit = 12
  patternMonthNames =
    [ "Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar"
    , "Mehr", "Aban", "Azar", "Dey", "Bahman", "Esfand"
    ]
  patternMonthAbbreviations =
    [ "Far", "Ord", "Kho", "Tir", "Mor", "Sha"
    , "Meh", "Aba", "Aza", "Dey", "Bah", "Esf"
    ]
  patternMonthNumber date = PersianMonths.monthNumber (month {calendar = Persian} date)
  patternWeekdayNumber date = PersianWeekdays.weekdayNumber (dayOfWeek {calendar = Persian} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refinePersianDate valueDay (persianMonth month) (yearFromInteger year) of
      Left _ => invalidDate "Persian"
      Right date => Right date

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
              KnownHebrewNumbering numbering => List String
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
  patternMonthLimit = 13
  patternMonthNames = hebrewNames {numbering}
  patternMonthAbbreviations = case numberingStart {numbering} of
    0 => [ "Tis", "Che", "Kis", "Tev", "She", "AdI", "Ada"
      , "Nis", "Iya", "Siv", "Tam", "Av", "Elu" ]
    _ => [ "Nis", "Iya", "Siv", "Tam", "Av", "Elu", "Tis"
      , "Che", "Kis", "Tev", "She", "AdI", "Ada" ]
  patternMonthNumber date = case yearMonthDay {calendar = Hebrew numbering} date of
    (_ ** (valueMonth, _)) => hebrewMonthNumber valueMonth
  patternWeekdayNumber date = HebrewWeekdays.weekdayNumber
    (dayOfWeek {calendar = Hebrew numbering} date)
  refinePatternDate year month day = do
    valueDay <- refineDay day
    case refineHebrewDate' {numbering} valueDay
      (hebrewMonthName {numbering} month) (yearFromInteger year) of
        Left _ => invalidDate "Hebrew"
        Right date => Right date