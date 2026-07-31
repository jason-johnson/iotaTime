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

gregorianCases : List RuntimeCase
gregorianCases =
    [ MkRuntimeCase "Gregorian epoch decodes to March 1 2000"
            (yearMonthDay {calendar = Gregorian} (fromDays {calendar = Gregorian} 0) == (2000, March, 1))
    , MkRuntimeCase "flat day conversion round-trips"
            (toDays {calendar = Gregorian} (fromDays {calendar = Gregorian} 42) == 42)
        , MkRuntimeCase "day lens views the day of month"
            (map (view (day {calendar = Gregorian})) (calendarDate 31 January 2000) == Just 31)
    , MkRuntimeCase "Gregorian changeover is the first valid date"
      (ymd (calendarDate 15 October 1582) == Just (1582, October, 15))
  , MkRuntimeCase "date before Gregorian changeover is rejected"
      (calendarDate 14 October 1582 == Nothing)
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
  , MkRuntimeCase "next Monday selects the following matching day"
      (ymd (map (next {calendar = Gregorian} 1 Monday) (calendarDate 31 January 2000)) ==
        Just (2000, February, 7))
  , MkRuntimeCase "previous Monday selects the preceding matching day"
      (ymd (map (previous {calendar = Gregorian} 1 Monday) (calendarDate 31 January 2000)) ==
        Just (2000, January, 24))
  , MkRuntimeCase "third Monday of January 2000"
      (ymd (fromNthDay Third Monday January 2000) == Just (2000, January, 17))
  , MkRuntimeCase "last weekday includes a matching final day"
      (ymd (fromNthDay Last Sunday December 2000) == Just (2000, December, 31))
  , MkRuntimeCase "fifth weekday is rejected when absent"
      (fromNthDay Fifth Monday February 2000 == Nothing)
  , MkRuntimeCase "Gregorian week one starts on Sunday"
      (ymd (fromWeekDate 1 Sunday 2000) == Just (1999, December, 26))
  , MkRuntimeCase "Gregorian week five starts on January 23"
      (ymd (fromWeekDate 5 Sunday 2000) == Just (2000, January, 23))
  ]

export
run : IO Bool
run = runSuite "Gregorian calendar tests" gregorianCases