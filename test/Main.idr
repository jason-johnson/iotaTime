module Main

import Test.Proof
import Test.Runtime
import Test.Regression
import Test.Duration
import Test.Interval
import Test.Offset
import Test.OffsetDateTime
import Test.DateTimeZone
import Test.ZonedDateTime
import Test.Gregorian
import Test.LocalTime
import Test.CalendarDateTime
import Test.Julian
import Test.Hebrew
import Test.WithCalendar
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
  zonedDateTimePassed <- Test.ZonedDateTime.run
  gregorianPassed <- Test.Gregorian.run
  localTimePassed <- Test.LocalTime.run
  calendarDateTimePassed <- Test.CalendarDateTime.run
  julianPassed <- Test.Julian.run
  hebrewPassed <- Test.Hebrew.run
  withCalendarPassed <- Test.WithCalendar.run
  finalizeResults
    [ ("proof tests (compile-time)", True)
    , ("runtime behavior tests", runtimePassed)
    , ("regression tests", regressionPassed)
    , ("duration and instant tests", durationPassed)
    , ("interval tests", intervalPassed)
    , ("offset tests", offsetPassed)
    , ("offset date-time tests", offsetDateTimePassed)
    , ("date-time zone tests", dateTimeZonePassed)
    , ("zoned date-time tests", zonedDateTimePassed)
    , ("Gregorian calendar tests", gregorianPassed)
    , ("local time tests", localTimePassed)
    , ("calendar date-time tests", calendarDateTimePassed)
    , ("Julian calendar tests", julianPassed)
    , ("Hebrew calendar tests", hebrewPassed)
    , ("withCalendar conversion tests", withCalendarPassed)
    ]
