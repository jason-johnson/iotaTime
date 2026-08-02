module Main

import Test.Proof
import Test.Runtime
import Test.Regression
import Test.Gregorian
import Test.LocalTime
import Test.CalendarDateTime
import Test.Julian
import Test.Hebrew
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
  gregorianPassed <- Test.Gregorian.run
  localTimePassed <- Test.LocalTime.run
  calendarDateTimePassed <- Test.CalendarDateTime.run
  julianPassed <- Test.Julian.run
  hebrewPassed <- Test.Hebrew.run
  finalizeResults
    [ ("proof tests (compile-time)", True)
    , ("runtime behavior tests", runtimePassed)
    , ("regression tests", regressionPassed)
    , ("Gregorian calendar tests", gregorianPassed)
    , ("local time tests", localTimePassed)
    , ("calendar date-time tests", calendarDateTimePassed)
    , ("Julian calendar tests", julianPassed)
    , ("Hebrew calendar tests", hebrewPassed)
    ]
