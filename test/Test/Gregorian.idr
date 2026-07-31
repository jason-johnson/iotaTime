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

ymd : Maybe (CalendarDate Gregorian) -> Maybe (Year, Month, DayOfMonth)
ymd = map (yearMonthDay {calendar = Gregorian})

modifyDateDay : (DayOfMonth -> DayOfMonth) -> Maybe (CalendarDate Gregorian) -> Maybe (CalendarDate Gregorian)
modifyDateDay transform = map (modify transform (day {calendar = Gregorian}))

modifyDateMonth : (Integer -> Integer) -> Maybe (CalendarDate Gregorian) -> Maybe (CalendarDate Gregorian)
modifyDateMonth transform = map (modify transform (monthl {calendar = Gregorian}))

modifyDateYear : (Year -> Year) -> Maybe (CalendarDate Gregorian) -> Maybe (CalendarDate Gregorian)
modifyDateYear transform = map (modify transform (year {calendar = Gregorian}))

weekday : Maybe (CalendarDate Gregorian) -> Maybe DayOfWeek
weekday = map (dayOfWeek {calendar = Gregorian})

validatedYmd : Maybe (ValidatedDate Gregorian) -> Maybe (Year, Month, DayOfMonth)
validatedYmd = map validatedYearMonthDay

isNothing : Maybe value -> Bool
isNothing Nothing = True
isNothing (Just _) = False

rawDaySamples : List Integer
rawDaySamples =
    [ -1000000, -152446, -152445, -152444, -1, 0, 1, 42, 36524, 146096, 146097, 1000000 ]

rawDayRoundTrips : Bool
rawDayRoundTrips = all
    (\value => toDays {calendar = Gregorian} (fromDays {calendar = Gregorian} value) == value)
    rawDaySamples

civilRoundTrips : Integer -> Integer -> Bool
civilRoundTrips current final =
    if current > final
        then True
        else
            let date = fromDays {calendar = Gregorian} current
                (valueYear, valueMonth, valueDay) = yearMonthDay {calendar = Gregorian} date
             in case calendarDate valueDay valueMonth valueYear of
                    Just rebuilt =>
                        if toDays {calendar = Gregorian} rebuilt == current
                            then civilRoundTrips (current + 1) final
                            else False
                    Nothing => False

validCivilRoundTrips : Bool
validCivilRoundTrips = civilRoundTrips (-152444) 146096

gregorianCases : List RuntimeCase
gregorianCases =
    [ MkRuntimeCase "Gregorian epoch decodes to March 1 2000"
            (yearMonthDay {calendar = Gregorian} (fromDays {calendar = Gregorian} 0) == (2000, March, 1))
    , MkRuntimeCase "flat day conversion round-trips"
            (toDays {calendar = Gregorian} (fromDays {calendar = Gregorian} 42) == 42)
        , MkRuntimeCase "raw day conversion round-trips across the Gregorian boundary"
            rawDayRoundTrips
        , MkRuntimeCase "civil conversion round-trips every valid day through February 2400"
            validCivilRoundTrips
        , MkRuntimeCase "day lens views the day of month"
            (map (view (day {calendar = Gregorian})) (calendarDate 31 January 2000) == Just 31)
    , MkRuntimeCase "Gregorian changeover is the first valid date"
      (ymd (calendarDate 15 October 1582) == Just (1582, October, 15))
  , MkRuntimeCase "date before Gregorian changeover is rejected"
      (calendarDate 14 October 1582 == Nothing)
        , MkRuntimeCase "raw pre-changeover date cannot cross the validated boundary"
            (isNothing (validateDate {calendar = Gregorian}
                (fromDays {calendar = Gregorian} (-152445))))
        , MkRuntimeCase "validated raw conversion accepts the Gregorian changeover"
            (validatedYmd (validatedFromDays {calendar = Gregorian} (-152444)) ==
                Just (1582, October, 15))
        , MkRuntimeCase "validated civil constructor preserves a valid date"
            (validatedYmd (validatedCalendarDate 29 February 2000) ==
                Just (2000, February, 29))
        , MkRuntimeCase "validated civil constructor rejects an invalid date"
            (isNothing (validatedCalendarDate 30 February 2000))
        , MkRuntimeCase "validated update rejects crossing the Gregorian boundary"
            (case validatedCalendarDate 15 October 1582 of
                Nothing => False
                Just date => isNothing (previousValidated 1 Thursday date))
        , MkRuntimeCase "validated navigation preserves valid results"
            (case validatedCalendarDate 31 January 2000 of
                Nothing => False
                Just date => validatedYmd (nextValidated 1 Monday date) ==
                    Just (2000, February, 7))
        , MkRuntimeCase "validated dates preserve underlying equality"
            (case (validatedCalendarDate 1 March 2000, validatedCalendarDate 1 March 2000) of
                (Just left, Just right) => validatedEquals left right
                _ => False)
        , MkRuntimeCase "validated dates preserve underlying ordering"
            (case (validatedCalendarDate 29 February 2000, validatedCalendarDate 1 March 2000) of
                (Just left, Just right) => validatedCompare left right == LT
                _ => False)
  , MkRuntimeCase "invalid day of month is rejected"
      (calendarDate 30 February 2000 == Nothing)
  , MkRuntimeCase "2000 is a leap year"
      (ymd (calendarDate 29 February 2000) == Just (2000, February, 29))
  , MkRuntimeCase "2100 is not a leap year"
      (calendarDate 29 February 2100 == Nothing)
  , MkRuntimeCase "2400 is a leap year"
      (ymd (calendarDate 29 February 2400) == Just (2400, February, 29))
  , MkRuntimeCase "day modification rolls into the next year"
      (ymd (modifyDateDay (+ 1) (calendarDate 31 December 2000)) == Just (2001, January, 1))
  , MkRuntimeCase "day modification crosses a non-leap century"
      (ymd (modifyDateDay (+ 1) (calendarDate 28 February 2100)) == Just (2100, March, 1))
  , MkRuntimeCase "day modification crosses the 400-year leap day"
      (ymd (modifyDateDay (+ 1) (calendarDate 29 February 2400)) == Just (2400, March, 1))
    , MkRuntimeCase "setting day 40 normalizes into the following month"
            (ymd (map (set (day {calendar = Gregorian}) 40) (calendarDate 1 March 2000)) ==
                Just (2000, April, 9))
    , MkRuntimeCase "day lens works with an independent van Laarhoven consumer"
            (ymd (map (independentOver (day {calendar = Gregorian}) (+ 10))
                (calendarDate 25 March 2000)) == Just (2000, April, 4))
  , MkRuntimeCase "day modification clamps at Gregorian changeover"
      (modifyDateDay (subtract 1) (calendarDate 15 October 1582) == calendarDate 15 October 1582)
  , MkRuntimeCase "month modification preserves a valid end-of-month day"
      (ymd (modifyDateMonth (+ 2) (calendarDate 31 January 2000)) == Just (2000, March, 31))
  , MkRuntimeCase "month modification clamps an invalid end-of-month day"
      (ymd (modifyDateMonth (+ 1) (calendarDate 31 January 2000)) == Just (2000, February, 29))
  , MkRuntimeCase "month modification crosses a year boundary"
      (ymd (modifyDateMonth (+ 1) (calendarDate 15 December 2000)) == Just (2001, January, 15))
  , MkRuntimeCase "month modification clamps at Gregorian changeover"
      (ymd (modifyDateMonth (subtract 1) (calendarDate 14 November 1582)) == Just (1582, October, 15))
  , MkRuntimeCase "year modification clamps leap day"
      (ymd (modifyDateYear (+ 1) (calendarDate 29 February 2000)) == Just (2001, February, 28))
  , MkRuntimeCase "year modification clamps at Gregorian changeover"
      (ymd (modifyDateYear (subtract 1) (calendarDate 14 October 1583)) == Just (1582, October, 15))
  , MkRuntimeCase "March 1 2000 is Wednesday"
      (weekday (calendarDate 1 March 2000) == Just Wednesday)
    , MkRuntimeCase "Gregorian dates compare by flat day"
            (calendarDate 29 February 2000 < calendarDate 1 March 2000)
    , MkRuntimeCase "equal Gregorian civil dates compare equal"
            (calendarDate 1 March 2000 == calendarDate 1 March 2000)
  , MkRuntimeCase "next Monday selects the following matching day"
      (ymd (map (next {calendar = Gregorian} 1 Monday) (calendarDate 31 January 2000)) ==
        Just (2000, February, 7))
    , MkRuntimeCase "next zero selects the previous matching day"
            (ymd (map (next {calendar = Gregorian} 0 Monday) (calendarDate 2 February 2000)) ==
                Just (2000, January, 31))
    , MkRuntimeCase "next negative one moves one match further backward"
            (ymd (map (next {calendar = Gregorian} (-1) Monday) (calendarDate 2 February 2000)) ==
                Just (2000, January, 24))
  , MkRuntimeCase "previous Monday selects the preceding matching day"
      (ymd (map (previous {calendar = Gregorian} 1 Monday) (calendarDate 31 January 2000)) ==
        Just (2000, January, 24))
    , MkRuntimeCase "previous zero selects the following matching day"
            (ymd (map (previous {calendar = Gregorian} 0 Monday) (calendarDate 2 February 2000)) ==
                Just (2000, February, 7))
    , MkRuntimeCase "previous negative one moves one match further forward"
            (ymd (map (previous {calendar = Gregorian} (-1) Monday) (calendarDate 2 February 2000)) ==
                Just (2000, February, 14))
  , MkRuntimeCase "third Monday of January 2000"
      (ymd (fromNthDay Third Monday January 2000) == Just (2000, January, 17))
    , MkRuntimeCase "validated nth-day constructor preserves its guarantee"
            (validatedYmd (validatedFromNthDay Third Monday January 2000) ==
                Just (2000, January, 17))
  , MkRuntimeCase "last weekday includes a matching final day"
      (ymd (fromNthDay Last Sunday December 2000) == Just (2000, December, 31))
  , MkRuntimeCase "fifth weekday is rejected when absent"
      (fromNthDay Fifth Monday February 2000 == Nothing)
  , MkRuntimeCase "Gregorian week one starts on Sunday"
      (ymd (fromWeekDate 1 Sunday 2000) == Just (1999, December, 26))
  , MkRuntimeCase "Gregorian week zero uses arithmetic week numbering"
      (ymd (fromWeekDate 0 Sunday 2000) == Just (1999, December, 19))
  , MkRuntimeCase "Gregorian week five starts on January 23"
      (ymd (fromWeekDate 5 Sunday 2000) == Just (2000, January, 23))
    , MkRuntimeCase "validated week-date constructor preserves its guarantee"
            (validatedYmd (validatedFromWeekDate 5 Sunday 2000) ==
                Just (2000, January, 23))
  ]

export
run : IO Bool
run = runSuite "Gregorian calendar tests" gregorianCases