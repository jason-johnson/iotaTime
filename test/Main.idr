module Main

import Test.Proof
import Test.Runtime
import Test.Regression
import Test.Duration
import Test.Interval
import Test.Offset
import Test.OffsetDateTime
import Test.DateTimeZone
import Test.Tzif
import Test.ZonedDateTime
import Test.Gregorian
import Test.Iso
import Test.LocalTime
import Test.CalendarDateTime
import Test.Julian
import Test.Coptic
import Test.Islamic
import Test.Persian
import Test.Hebrew
import Test.WithCalendar
import Test.Pattern
import Test.PatternLocalTime
import Test.PatternOffset
import Test.PatternDuration
import Test.Locale
import Test.Support

proofsChecked : ()
proofsChecked =
  let _ = zeroTickRoundTrip in
  let _ = negativeTickRoundTrip in
  let _ = largeTickRoundTrip in
  let _ = roundTripConstructor in
  let _ = gregorianLeapCycle in
  let _ = gregorianCenturyException in
  let _ = julianCenturyLeap in
  let _ = hebrewLeapCycle in
  ()

main : IO ()
main = do
  let _ = proofsChecked
  runtimePassed <- Test.Runtime.run
  regressionPassed <- Test.Regression.run
  durationPassed <- Test.Duration.run
  intervalPassed <- Test.Interval.run
  offsetPassed <- Test.Offset.run
  offsetDateTimePassed <- Test.OffsetDateTime.run
  dateTimeZonePassed <- Test.DateTimeZone.run
  tzifPassed <- Test.Tzif.run
  zonedDateTimePassed <- Test.ZonedDateTime.run
  gregorianPassed <- Test.Gregorian.run
  isoPassed <- Test.Iso.run
  localTimePassed <- Test.LocalTime.run
  calendarDateTimePassed <- Test.CalendarDateTime.run
  julianPassed <- Test.Julian.run
  copticPassed <- Test.Coptic.run
  islamicPassed <- Test.Islamic.run
  persianPassed <- Test.Persian.run
  hebrewPassed <- Test.Hebrew.run
  withCalendarPassed <- Test.WithCalendar.run
  patternPassed <- Test.Pattern.run
  patternLocalTimePassed <- Test.PatternLocalTime.run
  patternOffsetPassed <- Test.PatternOffset.run
  patternDurationPassed <- Test.PatternDuration.run
  localePassed <- Test.Locale.run
  finalizeResults
    [ ("proof tests (compile-time)", True)
    , ("runtime behavior tests", runtimePassed)
    , ("regression tests", regressionPassed)
    , ("duration and instant tests", durationPassed)
    , ("interval tests", intervalPassed)
    , ("offset tests", offsetPassed)
    , ("offset date-time tests", offsetDateTimePassed)
    , ("date-time zone tests", dateTimeZonePassed)
    , ("TZif tests", tzifPassed)
    , ("zoned date-time tests", zonedDateTimePassed)
    , ("Gregorian calendar tests", gregorianPassed)
    , ("ISO week-date tests", isoPassed)
    , ("local time tests", localTimePassed)
    , ("calendar date-time tests", calendarDateTimePassed)
    , ("Julian calendar tests", julianPassed)
    , ("Coptic calendar tests", copticPassed)
    , ("Islamic calendar tests", islamicPassed)
    , ("Persian calendar tests", persianPassed)
    , ("Hebrew calendar tests", hebrewPassed)
    , ("withCalendar conversion tests", withCalendarPassed)
    , ("calendar date pattern tests", patternPassed)
    , ("local time pattern tests", patternLocalTimePassed)
    , ("offset pattern tests", patternOffsetPassed)
    , ("duration pattern tests", patternDurationPassed)
    , ("locale tests", localePassed)
    ]
