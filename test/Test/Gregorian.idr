module Test.Gregorian

import IotaTime
import Test.Support

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd date = case yearMonthDay {calendar = Gregorian} date of
    (valueYear ** (valueMonth, valueDay)) => (valueYear, valueMonth, valueDay)

weekday : CalendarDate Gregorian -> DayOfWeek
weekday = dayOfWeek {calendar = Gregorian}

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

refinedYmd : Either GregorianDateError (CalendarDate Gregorian) ->
             Either GregorianDateError (Year, Month, DayOfMonth)
refinedYmd = map ymd

hasYmd : (Year, Month, DayOfMonth) ->
         Either GregorianDateError (Year, Month, DayOfMonth) -> Bool
hasYmd expected (Right actual) = actual == expected
hasYmd _ (Left _) = False

rawDaySamples : List Integer
rawDaySamples =
    [ -152444, -1, 0, 1, 42, 36524, 146096, 146097, 1000000 ]

rawDayRoundTrips : Bool
rawDayRoundTrips = all
    (\value => case refineGregorianDays value of
        Left _ => False
        Right date => toDays {calendar = Gregorian} date == value)
    rawDaySamples

civilRoundTrips : Integer -> Integer -> Bool
civilRoundTrips current final =
    if current > final
        then True
        else
            case refineGregorianDays current of
                Left _ => False
                Right date =>
                    let (valueYear, valueMonth, valueDay) = ymd date
                     in case refineGregorianDate valueDay valueMonth valueYear of
                            Left _ => False
                            Right rebuilt =>
                                if toDays {calendar = Gregorian} rebuilt == current
                                    then civilRoundTrips (current + 1) final
                                    else False

validCivilRoundTrips : Bool
validCivilRoundTrips = civilRoundTrips (-152444) 146096

gregorianCases : List RuntimeCase
gregorianCases =
    [ MkRuntimeCase "Gregorian epoch decodes to March 1 2000"
            (ymd (gregorianFromDays 0) == (2000, March, 1))
    , MkRuntimeCase "flat day conversion round-trips"
            (toDays {calendar = Gregorian} (gregorianFromDays 42) == 42)
        , MkRuntimeCase "valid day conversion round-trips from the Gregorian boundary"
            rawDayRoundTrips
        , MkRuntimeCase "civil conversion round-trips every valid day through February 2400"
            validCivilRoundTrips
        , MkRuntimeCase "day getter views the typed day of month"
            (day {calendar = Gregorian} (calendarDate 31 January 2000) == 31)
        , MkRuntimeCase "month getter views the typed Gregorian month"
            (month {calendar = Gregorian} (calendarDate 31 January 2000) == January)
        , MkRuntimeCase "year getter views the typed year"
            (year {calendar = Gregorian} (calendarDate 31 January 2000) == 2000)
    , MkRuntimeCase "Gregorian changeover is the first valid date"
            (ymd (calendarDate 15 October 1582) == (1582, October, 15))
        , MkRuntimeCase "dynamic date before Gregorian changeover is rejected"
            (isLeft (refineGregorianDate 14 October 1582))
        , MkRuntimeCase "dynamic raw day before changeover is rejected"
            (isLeft (refineGregorianDays (-152445)))
        , MkRuntimeCase "dynamic raw conversion accepts the Gregorian changeover"
            (hasYmd (1582, October, 15) (refinedYmd (refineGregorianDays (-152444))))
        , MkRuntimeCase "dynamic invalid day of month is rejected"
            (isLeft (refineGregorianDate 30 February 2000))
        , MkRuntimeCase "day-of-month refinement accepts one"
            (case refineDayOfMonth 1 of
                Right value => dayOfMonthValue value == 1
                Left _ => False)
        , MkRuntimeCase "day-of-month refinement accepts thirty-one"
            (case refineDayOfMonth 31 of
                Right value => dayOfMonthValue value == 31
                Left _ => False)
        , MkRuntimeCase "day-of-month refinement rejects zero"
            (isLeft (refineDayOfMonth 0))
        , MkRuntimeCase "day-of-month refinement rejects thirty-two"
            (isLeft (refineDayOfMonth 32))
        , MkRuntimeCase "2000 is a leap year"
            (ymd (calendarDate 29 February 2000) == (2000, February, 29))
        , MkRuntimeCase "dynamic 2100 leap day is rejected"
            (isLeft (refineGregorianDate 29 February 2100))
        , MkRuntimeCase "2400 is a leap year"
            (ymd (calendarDate 29 February 2400) == (2400, February, 29))
        , MkRuntimeCase "day period rolls into the next year"
            (ymd (applyPeriod (days 1) (calendarDate 31 December 2000)) ==
                (2001, January, 1))
        , MkRuntimeCase "day period crosses a non-leap century"
            (ymd (applyPeriod (days 1) (calendarDate 28 February 2100)) ==
                (2100, March, 1))
        , MkRuntimeCase "day period crosses the 400-year leap day"
            (ymd (applyPeriod (days 1) (calendarDate 29 February 2400)) ==
                (2400, March, 1))
        , MkRuntimeCase "multi-day period rolls into the following month"
            (ymd (applyPeriod (days 39) (calendarDate 1 March 2000)) ==
                (2000, April, 9))
        , MkRuntimeCase "negative day period moves backward"
            (ymd (applyPeriod (days (-10)) (calendarDate 25 March 2000)) ==
                (2000, March, 15))
        , MkRuntimeCase "day period clamps at Gregorian changeover"
            (applyPeriod (days (-1)) (calendarDate 15 October 1582) ==
                calendarDate 15 October 1582)
        , MkRuntimeCase "month period preserves a valid end-of-month day"
            (ymd (applyPeriod (months 2) (calendarDate 31 January 2000)) ==
                (2000, March, 31))
        , MkRuntimeCase "month period clamps an invalid end-of-month day"
            (ymd (applyPeriod (months 1) (calendarDate 31 January 2000)) ==
                (2000, February, 29))
        , MkRuntimeCase "month period crosses a year boundary"
            (ymd (applyPeriod (months 1) (calendarDate 15 December 2000)) ==
                (2001, January, 15))
        , MkRuntimeCase "combined month periods aggregate before application"
            (ymd (applyPeriod (months 1 <+> months 1) (calendarDate 31 January 2000)) ==
                (2000, March, 31))
        , MkRuntimeCase "month period clamps at Gregorian changeover"
            (ymd (applyPeriod (months (-1)) (calendarDate 14 November 1582)) ==
                (1582, October, 15))
        , MkRuntimeCase "year period clamps leap day"
            (ymd (applyPeriod (years 1) (calendarDate 29 February 2000)) ==
                (2001, February, 28))
        , MkRuntimeCase "year period clamps at Gregorian changeover"
            (ymd (applyPeriod (years (-1)) (calendarDate 14 October 1583)) ==
                (1582, October, 15))
        , MkRuntimeCase "date period fields apply from largest to smallest"
            (ymd (applyPeriod (years 1 <+> days 1) (calendarDate 28 February 1999)) ==
                (2000, February, 29))
        , MkRuntimeCase "negating a date period reverses its direction"
            (ymd (applyPeriod (negatePeriod (weeks 2)) (calendarDate 15 March 2000)) ==
                (2000, March, 1))
        , MkRuntimeCase "scaling a date period multiplies its components"
            (ymd (applyPeriod (scalePeriod 3 (weeks 1)) (calendarDate 1 March 2000)) ==
                (2000, March, 22))
        , MkRuntimeCase "March 1 2000 is Wednesday"
            (weekday (calendarDate 1 March 2000) == Wednesday)
        , MkRuntimeCase "Gregorian dates compare by flat day"
            (calendarDate 29 February 2000 < calendarDate 1 March 2000)
        , MkRuntimeCase "equal Gregorian civil dates compare equal"
            (calendarDate 1 March 2000 == calendarDate 1 March 2000)
        , MkRuntimeCase "next Monday selects the following matching day"
            (ymd (next {calendar = Gregorian} 1 Monday (calendarDate 31 January 2000)) ==
                (2000, February, 7))
        , MkRuntimeCase "next zero selects the previous matching day"
            (ymd (next {calendar = Gregorian} 0 Monday (calendarDate 2 February 2000)) ==
                (2000, January, 31))
        , MkRuntimeCase "next negative one moves one match further backward"
            (ymd (next {calendar = Gregorian} (-1) Monday (calendarDate 2 February 2000)) ==
                (2000, January, 24))
        , MkRuntimeCase "previous Monday selects the preceding matching day"
            (ymd (previous {calendar = Gregorian} 1 Monday (calendarDate 31 January 2000)) ==
                (2000, January, 24))
        , MkRuntimeCase "previous clamps at the Gregorian changeover"
            (previous {calendar = Gregorian} 1 Thursday (calendarDate 15 October 1582) ==
                calendarDate 15 October 1582)
        , MkRuntimeCase "third Monday of January 2000"
            (ymd (fromNthDay Third Monday January 2000) == (2000, January, 17))
        , MkRuntimeCase "last weekday includes a matching final day"
            (ymd (fromNthDay Last Sunday December 2000) == (2000, December, 31))
        , MkRuntimeCase "dynamic fifth weekday is rejected when absent"
            (isLeft (refineGregorianNthDay Fifth Monday February 2000))
        , MkRuntimeCase "Gregorian week one starts on Sunday"
            (ymd (fromWeekDate 1 Sunday 2000) == (1999, December, 26))
        , MkRuntimeCase "Gregorian week zero uses arithmetic week numbering"
            (ymd (fromWeekDate 0 Sunday 2000) == (1999, December, 19))
        , MkRuntimeCase "Gregorian week five starts on January 23"
            (ymd (fromWeekDate 5 Sunday 2000) == (2000, January, 23))
  ]

export
run : IO Bool
run = runSuite "Gregorian calendar tests" gregorianCases