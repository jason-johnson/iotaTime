module Test.Gregorian

import IotaTime
import Test.Support

record TestIdentity (value : Type) where
    constructor MkTestIdentity
    runTestIdentity : value

Functor TestIdentity where
    map function (MkTestIdentity value) = MkTestIdentity (function value)

independentOver :
    ({0 effect : Type -> Type} -> Functor effect =>
        (focus -> effect replacement) -> source -> effect target) ->
    (focus -> replacement) -> source -> target
independentOver optic transform source =
    runTestIdentity (optic (MkTestIdentity . transform) source)

ymd : CalendarDate Gregorian -> (Year, Month, DayOfMonth)
ymd = yearMonthDay {calendar = Gregorian}

modifyDateDay : (DayOfMonth -> DayOfMonth) -> CalendarDate Gregorian -> CalendarDate Gregorian
modifyDateDay transform = modify transform (day {calendar = Gregorian})

modifyDateMonth : (Integer -> Integer) -> CalendarDate Gregorian -> CalendarDate Gregorian
modifyDateMonth transform = modify transform (monthl {calendar = Gregorian})

modifyDateYear : (Year -> Year) -> CalendarDate Gregorian -> CalendarDate Gregorian
modifyDateYear transform = modify transform (year {calendar = Gregorian})

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
            (yearMonthDay {calendar = Gregorian} (gregorianFromDays 0) == (2000, March, 1))
    , MkRuntimeCase "flat day conversion round-trips"
            (toDays {calendar = Gregorian} (gregorianFromDays 42) == 42)
        , MkRuntimeCase "valid day conversion round-trips from the Gregorian boundary"
            rawDayRoundTrips
        , MkRuntimeCase "civil conversion round-trips every valid day through February 2400"
            validCivilRoundTrips
        , MkRuntimeCase "day lens views the day of month"
                        (view (day {calendar = Gregorian}) (calendarDate 31 January 2000) == 31)
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
        , MkRuntimeCase "2000 is a leap year"
            (ymd (calendarDate 29 February 2000) == (2000, February, 29))
        , MkRuntimeCase "dynamic 2100 leap day is rejected"
            (isLeft (refineGregorianDate 29 February 2100))
        , MkRuntimeCase "2400 is a leap year"
            (ymd (calendarDate 29 February 2400) == (2400, February, 29))
        , MkRuntimeCase "day modification rolls into the next year"
            (ymd (modifyDateDay (+ 1) (calendarDate 31 December 2000)) == (2001, January, 1))
        , MkRuntimeCase "day modification crosses a non-leap century"
            (ymd (modifyDateDay (+ 1) (calendarDate 28 February 2100)) == (2100, March, 1))
        , MkRuntimeCase "day modification crosses the 400-year leap day"
            (ymd (modifyDateDay (+ 1) (calendarDate 29 February 2400)) == (2400, March, 1))
    , MkRuntimeCase "setting day 40 normalizes into the following month"
                        (ymd (set (day {calendar = Gregorian}) 40 (calendarDate 1 March 2000)) ==
                                (2000, April, 9))
    , MkRuntimeCase "day lens works with an independent van Laarhoven consumer"
                        (ymd (independentOver (day {calendar = Gregorian}) (+ 10)
                                (calendarDate 25 March 2000)) == (2000, April, 4))
        , MkRuntimeCase "day modification clamps at Gregorian changeover"
            (modifyDateDay (subtract 1) (calendarDate 15 October 1582) == calendarDate 15 October 1582)
        , MkRuntimeCase "month modification preserves a valid end-of-month day"
            (ymd (modifyDateMonth (+ 2) (calendarDate 31 January 2000)) == (2000, March, 31))
        , MkRuntimeCase "month modification clamps an invalid end-of-month day"
            (ymd (modifyDateMonth (+ 1) (calendarDate 31 January 2000)) == (2000, February, 29))
        , MkRuntimeCase "month modification crosses a year boundary"
            (ymd (modifyDateMonth (+ 1) (calendarDate 15 December 2000)) == (2001, January, 15))
        , MkRuntimeCase "month modification clamps at Gregorian changeover"
            (ymd (modifyDateMonth (subtract 1) (calendarDate 14 November 1582)) == (1582, October, 15))
        , MkRuntimeCase "year modification clamps leap day"
            (ymd (modifyDateYear (+ 1) (calendarDate 29 February 2000)) == (2001, February, 28))
        , MkRuntimeCase "year modification clamps at Gregorian changeover"
            (ymd (modifyDateYear (subtract 1) (calendarDate 14 October 1583)) == (1582, October, 15))
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