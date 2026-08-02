module Test.WithCalendar

import IotaTime
import Test.Support

julianYmd : CalendarDate Julian -> (Year, JulianMonth, DayOfMonth)
julianYmd date = case yearMonthDay {calendar = Julian} date of
  (valueYear ** (valueMonth, valueDay)) => (valueYear, valueMonth, valueDay)

hebrewYmd : CalendarDate HebrewCivil -> (Year, HebrewMonthName, DayOfMonth)
hebrewYmd date = case yearMonthDay {calendar = HebrewCivil} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, monthName valueMonth, valueDay)

gregorianChristmasInJulian : Either CalendarConversionError (CalendarDate Julian)
gregorianChristmasInJulian =
  IotaTime.Calendar.withCalendar (calendarDate 25 December 2024)

gregorianNewYearInHebrew : Either CalendarConversionError (CalendarDate HebrewCivil)
gregorianNewYearInHebrew =
  IotaTime.Calendar.withCalendar (calendarDate 16 September 2023)

backToGregorian : CalendarDate HebrewCivil ->
                  Either CalendarConversionError (CalendarDate Gregorian)
backToGregorian = IotaTime.Calendar.withCalendar

hebrewEpochInGregorian : Either CalendarConversionError (CalendarDate Gregorian)
hebrewEpochInGregorian = IotaTime.Calendar.withCalendar
  (hebrewFromDays (-2103607))

christmasDateTime : CalendarDateTime Gregorian
christmasDateTime = on (localTime 14 30 0 0) (calendarDate 25 December 2024)

christmasDateTimeInJulian : Either CalendarConversionError (CalendarDateTime Julian)
christmasDateTimeInJulian = IotaTime.CalendarDateTime.withCalendar
  christmasDateTime

withCalendarCases : List RuntimeCase
withCalendarCases =
  [ MkRuntimeCase "Gregorian Christmas converts to Julian December 12"
      (case gregorianChristmasInJulian of
        Left _ => False
        Right date => julianYmd date == (2024, JulianMonths.December, 12))
  , MkRuntimeCase "Gregorian new year converts to Hebrew 1 Tishri"
      (case gregorianNewYearInHebrew of
        Left _ => False
        Right date => hebrewYmd date == (5784, TishriName, 1))
  , MkRuntimeCase "calendar conversion round-trip preserves the flat day"
      (case gregorianNewYearInHebrew of
        Left _ => False
        Right hebrewDate =>
          case backToGregorian hebrewDate of
            Left _ => False
            Right gregorianDate =>
              toDays {calendar = Gregorian} gregorianDate ==
                toDays {calendar = HebrewCivil} hebrewDate)
  , MkRuntimeCase "calendar conversion rejects a day before the target range"
      (case hebrewEpochInGregorian of
          Left (TargetCalendarOutOfRange "Gregorian" (-2103607)) => True
          _ => False)
  , MkRuntimeCase "CalendarDateTime conversion preserves local time"
      (case christmasDateTimeInJulian of
        Left _ => False
        Right value =>
          julianYmd (datePart value) == (2024, JulianMonths.December, 12) &&
          localTimeOfDay value == localTime 14 30 0 0)
  ]

export
run : IO Bool
run = runSuite "withCalendar conversion tests" withCalendarCases
