module Test.Coptic

import IotaTime
import IotaTime.Calendar.Coptic
import Test.Support

cymd : CalendarDate Coptic -> (Year, CopticMonth, DayOfMonth)
cymd date = case yearMonthDay {calendar = Coptic} date of
  (valueYear ** (valueMonth, valueDay)) =>
    (valueYear, valueMonth, valueDay)

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

copticRoundTrips : Integer -> Integer -> Bool
copticRoundTrips current final =
  if current > final
    then True
    else case refineCopticDays current of
      Left _ => False
      Right date =>
        let (valueYear, valueMonth, valueDay) = cymd date
         in case refineCopticDate valueDay valueMonth valueYear of
              Left _ => False
              Right rebuilt =>
                toDays {calendar = Coptic} rebuilt == current &&
                copticRoundTrips (current + 1) final

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

gregorianAnchorAsCoptic : Either CalendarConversionError (CalendarDate Coptic)
gregorianAnchorAsCoptic = IotaTime.Calendar.withCalendar
  (calendarDate 11 September 2021)

mixedCopticResult : CalendarDateTime Coptic
mixedCopticResult = applyPeriod (months 1 <+> hours 2)
  (on (localTime 23 30 0 0)
    (copticDate 30 CopticMonths.Mesori 1732))

copticCases : List RuntimeCase
copticCases =
  [ MkRuntimeCase "Coptic epoch is 1 Thout 1"
      (cymd (copticFromDays (-626575)) == (1, CopticMonths.Thout, 1))
  , MkRuntimeCase "Coptic civil conversion round-trips two leap cycles"
      (copticRoundTrips (-626575) (-623653))
  , MkRuntimeCase "Coptic epoch matches Julian August 29 284"
      (toDays {calendar = Coptic} (copticDate 1 CopticMonths.Thout 1) ==
       calendarDays (julianDate 29 JulianMonths.August 284))
  , MkRuntimeCase "Nayrouz 1738 is Gregorian September 11 2021"
      (calendarDays (copticDate 1 CopticMonths.Thout 1738) ==
       calendarDays (calendarDate 11 September 2021))
  , MkRuntimeCase "Nayrouz 1716 is Gregorian September 12 1999"
      (calendarDays (copticDate 1 CopticMonths.Thout 1716) ==
       calendarDays (calendarDate 12 September 1999))
  , MkRuntimeCase "Gregorian anchor converts through the generic calendar API"
      (case gregorianAnchorAsCoptic of
        Left _ => False
        Right date => cymd date == (1738, CopticMonths.Thout, 1))
  , MkRuntimeCase "thirty-day Coptic months reject day thirty-one"
      (isLeft (refineCopticDate 31 CopticMonths.Thout 1716))
  , MkRuntimeCase "epagomenal day six exists in leap year 1731"
      (cymd (copticDate 6 CopticMonths.PiKogiEnavot 1731) ==
       (1731, CopticMonths.PiKogiEnavot, 6))
  , MkRuntimeCase "epagomenal day six is rejected in common year 1732"
      (isLeft (refineCopticDate 6 CopticMonths.PiKogiEnavot 1732))
  , MkRuntimeCase "year zero is rejected"
      (isLeft (refineCopticDate 1 CopticMonths.Thout 0))
  , MkRuntimeCase "Coptic month period enters the epagomenal month"
      (cymd (applyPeriod (months 1)
        (copticDate 1 CopticMonths.Mesori 1716)) ==
        (1716, CopticMonths.PiKogiEnavot, 1))
  , MkRuntimeCase "Coptic year period clamps epagomenal leap day"
      (cymd (applyPeriod (years 1)
        (copticDate 6 CopticMonths.PiKogiEnavot 1731)) ==
        (1732, CopticMonths.PiKogiEnavot, 5))
  , MkRuntimeCase "Coptic epoch weekday is Friday"
      (dayOfWeek {calendar = Coptic} (copticFromDays (-626575)) ==
        CopticWeekdays.Friday)
  , MkRuntimeCase "first Monday of Thout 1716"
      (dayOfWeek {calendar = Coptic}
        (copticFromNthDay First CopticWeekdays.Monday
          CopticMonths.Thout 1716) == CopticWeekdays.Monday)
  , MkRuntimeCase "absent epagomenal weekday is rejected"
      (isLeft (refineCopticNthDay First CopticWeekdays.Sunday
        CopticMonths.PiKogiEnavot 1732))
  , MkRuntimeCase "Coptic week one starts on Sunday"
      (dayOfWeek {calendar = Coptic}
        (copticFromWeekDate 1 CopticWeekdays.Sunday 1716) ==
        CopticWeekdays.Sunday)
  , MkRuntimeCase "Coptic CalendarDateTime accepts mixed periods"
      (cymd (datePart mixedCopticResult) ==
        (1733, CopticMonths.Thout, 1) &&
       timeComponents (localTimeOfDay mixedCopticResult) == (1, 30, 0, 0))
  ]

export
run : IO Bool
run = runSuite "Coptic calendar tests" copticCases
