module Test.PatternCalendar

import Data.Vect
import IotaTime
import Test.Support

roundTrips : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
             CalendarDate calendar -> String -> Bool
roundTrips {calendar} value expected =
  IotaTime.Pattern.format (pR {calendar}) value == expected &&
  case IotaTime.Pattern.parse (pR {calendar}) expected of
    Left _ => False
    Right actual =>
      toDaysFor {calendar} actual == toDaysFor {calendar} value

rejects : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
          String -> Bool
rejects {calendar} source = case IotaTime.Pattern.parse (pR {calendar}) source of
  Left _ => True
  Right _ => False

rejectsMonthRefinement : {calendar : Type} ->
                         {auto patterned : CalendarPattern calendar} ->
                         Integer -> Integer -> Bool
rejectsMonthRefinement {calendar} year month =
  case refinePatternDate {calendar} year month 1 of
    Left (ValueOutOfRange "month" 1 _ value) => value == month
    _ => False

sameDateTime : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
               CalendarDateTime calendar -> CalendarDateTime calendar -> Bool
sameDateTime {calendar} left right =
  toDaysFor {calendar} (datePart left) ==
    toDaysFor {calendar} (datePart right) &&
  localTimeOfDay left == localTimeOfDay right

namedPattern : {calendar : Type} ->
               {auto patterned : CalendarPattern calendar} ->
               Pattern DateFields (CalendarDate calendar)
namedPattern {calendar} =
  ((pyyyy {calendar} <% char '-') <+> (pMMMM {calendar} <% char '-')) <+>
  pdd {calendar}

namedRoundTrips : {calendar : Type} ->
                  {auto patterned : CalendarPattern calendar} ->
                  CalendarDate calendar -> String -> Bool
namedRoundTrips {calendar} value expected =
  IotaTime.Pattern.format (namedPattern {calendar}) value == expected &&
  case IotaTime.Pattern.parse (namedPattern {calendar}) expected of
    Left _ => False
    Right actual =>
      toDaysFor {calendar} actual == toDaysFor {calendar} value

customCopticMonthNames : Vect 13 String
customCopticMonthNames =
  [ "M01", "M02", "M03", "M04", "M05", "M06", "M07"
  , "M08", "M09", "M10", "M11", "M12", "M13"
  ]

customCopticPattern : Pattern DateFields (CalendarDate Coptic)
customCopticPattern =
  ((pyyyy {calendar = Coptic} <% char '-') <+>
    (pMonthName {calendar = Coptic} customCopticMonthNames <% char '-')) <+>
  pdd {calendar = Coptic}

dateTimeRoundTrips : {calendar : Type} ->
                     {auto patterned : CalendarPattern calendar} ->
                     CalendarDateTime calendar -> String -> Bool
dateTimeRoundTrips {calendar} value expected =
  IotaTime.Pattern.format (ps {calendar}) value == expected &&
  case IotaTime.Pattern.parse (ps {calendar}) expected of
    Left _ => False
    Right actual => sameDateTime actual value

offsetRoundTrips : {calendar : Type} ->
                   {auto patterned : CalendarPattern calendar} ->
                   OffsetDateTime calendar -> String -> Bool
offsetRoundTrips {calendar} value expected =
  IotaTime.Pattern.format (pOffsetDateTime {calendar}) value == expected &&
  case IotaTime.Pattern.parse (pOffsetDateTime {calendar}) expected of
    Left _ => False
    Right actual => sameDateTime (toCalendarDateTime actual)
      (toCalendarDateTime value) && offset actual == offset value

patternCalendarCases : List RuntimeCase
patternCalendarCases =
  [ MkRuntimeCase "Gregorian numeric date pattern remains polymorphic"
      (roundTrips {calendar = Gregorian}
        (IotaTime.Calendar.Gregorian.calendarDate 29 February 2000) "2000-02-29")
  , MkRuntimeCase "Julian numeric date pattern round-trips"
      (roundTrips {calendar = Julian}
        (IotaTime.Calendar.Julian.calendarDate 29 JulianMonths.February 1900) "1900-02-29")
  , MkRuntimeCase "Coptic numeric date pattern supports month thirteen"
      (roundTrips {calendar = Coptic}
        (IotaTime.Calendar.Coptic.calendarDate 6 CopticMonths.PiKogiEnavot 1731) "1731-13-06")
  , MkRuntimeCase "Islamic numeric date patterns retain their leap index"
      (roundTrips {calendar = IslamicBase15}
        (IotaTime.Calendar.Islamic.calendarDate' {pattern = Base15}
          30 IslamicMonths.DhulHijjah 15) "0015-12-30")
  , MkRuntimeCase "civil Islamic patterns retain epoch and leap indexes"
      (roundTrips {calendar = CivilIslamicBase15}
        (IotaTime.Calendar.Islamic.civilCalendarDate' {pattern = Base15}
          30 IslamicMonths.DhulHijjah 15) "0015-12-30")
  , MkRuntimeCase "Persian numeric date pattern round-trips"
      (roundTrips {calendar = Persian}
        (IotaTime.Calendar.Persian.calendarDate 30 PersianMonths.Esfand 1403) "1403-12-30")
  , MkRuntimeCase "Hebrew civil numeric pattern resolves year-indexed months"
      (roundTrips {calendar = HebrewCivil}
        (IotaTime.Calendar.Hebrew.calendarDate 1 5784 HebrewMonths.AdarI) "5784-06-01")
  , MkRuntimeCase "Hebrew scriptural numbering changes the numeric month"
      (roundTrips {calendar = HebrewScriptural}
        (IotaTime.Calendar.Hebrew.calendarDate' {numbering = Scriptural}
          1 5784 HebrewMonths.AdarI) "5784-12-01")
  , MkRuntimeCase "Coptic canonical month names round-trip"
      (namedRoundTrips {calendar = Coptic}
        (IotaTime.Calendar.Coptic.calendarDate 6 CopticMonths.PiKogiEnavot 1731)
        "1731-PiKogiEnavot-06")
  , MkRuntimeCase "Islamic canonical month names round-trip"
      (namedRoundTrips {calendar = IslamicBase15}
        (IotaTime.Calendar.Islamic.calendarDate' {pattern = Base15} 1 IslamicMonths.Ramadan 1443)
        "1443-Ramadan-01")
  , MkRuntimeCase "civil Islamic canonical month names round-trip"
      (namedRoundTrips {calendar = CivilIslamicBase15}
        (IotaTime.Calendar.Islamic.civilCalendarDate' {pattern = Base15}
          1 IslamicMonths.Ramadan 1443) "1443-Ramadan-01")
  , MkRuntimeCase "Hebrew canonical month names round-trip"
      (namedRoundTrips {calendar = HebrewCivil}
        (IotaTime.Calendar.Hebrew.calendarDate 1 5784 HebrewMonths.AdarI) "5784-AdarI-01")
  , MkRuntimeCase "custom month names require the calendar's month count"
      (let expected = IotaTime.Calendar.Coptic.calendarDate 6 CopticMonths.PiKogiEnavot 1731 in
        IotaTime.Pattern.format customCopticPattern expected == "1731-M13-06" &&
        case IotaTime.Pattern.parse customCopticPattern "1731-M13-06" of
          Left _ => False
          Right actual => calendarDays actual == calendarDays expected)
  , MkRuntimeCase "Julian date-time patterns round-trip"
      (dateTimeRoundTrips {calendar = Julian}
        (on (localTime 23 59 58 0)
          (IotaTime.Calendar.Julian.calendarDate 29 JulianMonths.February 1900))
        "1900-02-29T23:59:58")
  , MkRuntimeCase "Coptic date-time patterns round-trip"
      (dateTimeRoundTrips {calendar = Coptic}
        (on (localTime 1 2 3 0)
          (IotaTime.Calendar.Coptic.calendarDate 6 CopticMonths.PiKogiEnavot 1731))
        "1731-13-06T01:02:03")
  , MkRuntimeCase "offset date-time patterns preserve non-Gregorian values"
      (offsetRoundTrips {calendar = Persian}
        (fromCalendarDateTimeWithOffset
          (on (localTime 12 30 0 0)
            (IotaTime.Calendar.Persian.calendarDate 30 PersianMonths.Esfand 1403))
          (IotaTime.Offset.fromMinutes 210))
        "1403-12-30T12:30:00+03:30")
    , MkRuntimeCase "Julian refinement rejects invalid dates"
      (rejects {calendar = Julian} "1901-02-29")
    , MkRuntimeCase "Coptic refinement rejects invalid dates"
      (rejects {calendar = Coptic} "1730-13-06")
    , MkRuntimeCase "Islamic refinement rejects invalid dates"
      (rejects {calendar = IslamicBase15} "0016-12-30")
    , MkRuntimeCase "Persian refinement rejects invalid dates"
      (rejects {calendar = Persian} "1501-01-01")
    , MkRuntimeCase "Hebrew refinement rejects invalid dates"
      (rejects {calendar = HebrewCivil} "5786-06-01")
    , MkRuntimeCase "Gregorian refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = Gregorian} 2000 0 &&
       rejectsMonthRefinement {calendar = Gregorian} 2000 13)
    , MkRuntimeCase "Julian refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = Julian} 1900 0 &&
       rejectsMonthRefinement {calendar = Julian} 1900 13)
    , MkRuntimeCase "Coptic refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = Coptic} 1731 0 &&
       rejectsMonthRefinement {calendar = Coptic} 1731 14)
    , MkRuntimeCase "Islamic refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = IslamicBase15} 1443 0 &&
       rejectsMonthRefinement {calendar = IslamicBase15} 1443 13)
    , MkRuntimeCase "civil Islamic refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = CivilIslamicBase15} 1443 0 &&
       rejectsMonthRefinement {calendar = CivilIslamicBase15} 1443 13)
    , MkRuntimeCase "Persian refinement rejects out-of-range months"
      (rejectsMonthRefinement {calendar = Persian} 1400 0 &&
       rejectsMonthRefinement {calendar = Persian} 1400 13)
    , MkRuntimeCase "simple arithmetic Persian refinement rejects month zero"
      (rejectsMonthRefinement {calendar = ArithmeticPersian Simple} 1400 0)
    , MkRuntimeCase "Birashk Persian refinement rejects month thirteen"
      (rejectsMonthRefinement {calendar = ArithmeticPersian Birashk} 1400 13)
    , MkRuntimeCase "Hebrew civil refinement rejects month zero"
      (rejectsMonthRefinement {calendar = HebrewCivil} 5784 0)
    , MkRuntimeCase "Hebrew scriptural refinement rejects month fourteen"
      (rejectsMonthRefinement {calendar = HebrewScriptural} 5784 14)
  ]

zonedCalendarCases : TimeZone -> IO (List RuntimeCase)
zonedCalendarCases utcZone = do
  let provider : String -> IO (Either String TimeZone)
      provider "UTC" = pure (Right utcZone)
      provider name = pure (Left ("unknown zone: " ++ name))
  coptic <- parseStandardZonedDateTime {calendar = Coptic} provider
    fromCalendarDateTimeStrictly "1731-13-06T01:02:03 UTC"
  hebrew <- parseStandardZonedDateTime {calendar = HebrewCivil} provider
    fromCalendarDateTimeStrictly "5784-06-01T04:05:06 UTC"
  pure
    [ MkRuntimeCase "zoned patterns resolve Coptic month thirteen"
        (case coptic of
          Left _ => False
          Right value =>
            formatZonedDateTime (pZonedDateTime {calendar = Coptic}) value ==
              "1731-13-06T01:02:03 UTC")
    , MkRuntimeCase "zoned patterns preserve Hebrew leap-month indexing"
        (case hebrew of
          Left _ => False
          Right value =>
            formatZonedDateTime
              (pZonedDateTime {calendar = HebrewCivil}) value ==
              "5784-06-01T04:05:06 UTC")
    ]

export
run : IO Bool
run = do
  loadedUtc <- utc
  case loadedUtc of
    Left _ => runSuite "calendar-polymorphic pattern tests"
      (patternCalendarCases ++
        [MkRuntimeCase "UTC loads for zoned calendar tests" False])
    Right utcZone => do
      zonedCases <- zonedCalendarCases utcZone
      runSuite "calendar-polymorphic pattern tests"
        (patternCalendarCases ++ zonedCases)