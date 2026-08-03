module Test.PatternCalendar

import IotaTime
import Test.Support

roundTrips : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
             CalendarDate calendar -> String -> Bool
roundTrips {calendar} value expected =
  IotaTime.Pattern.format (pR {calendar}) value == expected &&
  case IotaTime.Pattern.parse (pR {calendar}) expected of
    Left _ => False
    Right actual => toDays actual == toDays value

rejects : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
          String -> Bool
rejects {calendar} source = case IotaTime.Pattern.parse (pR {calendar}) source of
  Left _ => True
  Right _ => False

sameDateTime : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
               CalendarDateTime calendar -> CalendarDateTime calendar -> Bool
sameDateTime left right =
  toDays (datePart left) == toDays (datePart right) &&
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
    Right actual => toDays actual == toDays value

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
    Right actual => sameDateTime (localDateTime actual) (localDateTime value) &&
      offsetOf actual == offsetOf value

patternCalendarCases : List RuntimeCase
patternCalendarCases =
  [ MkRuntimeCase "Gregorian numeric date pattern remains polymorphic"
      (roundTrips {calendar = Gregorian}
        (calendarDate 29 February 2000) "2000-02-29")
  , MkRuntimeCase "Julian numeric date pattern round-trips"
      (roundTrips {calendar = Julian}
        (julianDate 29 JulianMonths.February 1900) "1900-02-29")
  , MkRuntimeCase "Coptic numeric date pattern supports month thirteen"
      (roundTrips {calendar = Coptic}
        (copticDate 6 CopticMonths.PiKogiEnavot 1731) "1731-13-06")
  , MkRuntimeCase "Islamic numeric date patterns retain their leap index"
      (roundTrips {calendar = IslamicBase15}
        (islamicDate' {pattern = Base15}
          30 IslamicMonths.DhulHijjah 15) "0015-12-30")
  , MkRuntimeCase "Persian numeric date pattern round-trips"
      (roundTrips {calendar = Persian}
        (persianDate 30 PersianMonths.Esfand 1403) "1403-12-30")
  , MkRuntimeCase "Hebrew civil numeric pattern resolves year-indexed months"
      (roundTrips {calendar = HebrewCivil}
        (hebrewDate 1 5784 HebrewMonths.AdarI) "5784-06-01")
  , MkRuntimeCase "Hebrew scriptural numbering changes the numeric month"
      (roundTrips {calendar = HebrewScriptural}
        (hebrewDate' {numbering = Scriptural}
          1 5784 HebrewMonths.AdarI) "5784-12-01")
  , MkRuntimeCase "canonical month names are calendar-polymorphic"
      (namedRoundTrips {calendar = Coptic}
        (copticDate 6 CopticMonths.PiKogiEnavot 1731)
        "1731-PiKogiEnavot-06" &&
       namedRoundTrips {calendar = IslamicBase15}
        (islamicDate' {pattern = Base15} 1 IslamicMonths.Ramadan 1443)
        "1443-Ramadan-01" &&
       namedRoundTrips {calendar = HebrewCivil}
        (hebrewDate 1 5784 HebrewMonths.AdarI) "5784-AdarI-01")
  , MkRuntimeCase "calendar date-time patterns round-trip non-Gregorian values"
      (dateTimeRoundTrips {calendar = Julian}
        (on (localTime 23 59 58 0)
          (julianDate 29 JulianMonths.February 1900))
        "1900-02-29T23:59:58" &&
       dateTimeRoundTrips {calendar = Coptic}
        (on (localTime 1 2 3 0)
          (copticDate 6 CopticMonths.PiKogiEnavot 1731))
        "1731-13-06T01:02:03")
  , MkRuntimeCase "offset date-time patterns preserve non-Gregorian values"
      (offsetRoundTrips {calendar = Persian}
        (atOffset
          (on (localTime 12 30 0 0)
            (persianDate 30 PersianMonths.Esfand 1403))
          (offsetFromMinutes 210))
        "1403-12-30T12:30:00+03:30")
  , MkRuntimeCase "calendar-specific refinement rejects invalid dates"
      (rejects {calendar = Julian} "1901-02-29" &&
       rejects {calendar = Coptic} "1730-13-06" &&
       rejects {calendar = IslamicBase15} "0016-12-30" &&
       rejects {calendar = Persian} "1501-01-01" &&
       rejects {calendar = HebrewCivil} "5786-06-01")
  ]

utcProvider : String -> IO (Either String TimeZone)
utcProvider "UTC" = pure (Right (fixedDateTimeZone "UTC" zeroOffset))
utcProvider name = pure (Left ("unknown zone: " ++ name))

zonedCalendarCases : IO (List RuntimeCase)
zonedCalendarCases = do
  coptic <- parseStandardZonedDateTime {calendar = Coptic} utcProvider
    fromCalendarDateTimeStrictly "1731-13-06T01:02:03 UTC"
  hebrew <- parseStandardZonedDateTime {calendar = HebrewCivil} utcProvider
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
  zonedCases <- zonedCalendarCases
  runSuite "calendar-polymorphic pattern tests"
    (patternCalendarCases ++ zonedCases)