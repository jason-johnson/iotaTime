module Main

import Test.Proof
import Test.Runtime
import Test.Regression
import Test.Gregorian
import Test.LocalTime
import Test.CalendarDateTime
import Test.Support

proofsChecked : ()
proofsChecked =
  let _ = zeroTickRoundTrip in
  let _ = negativeTickRoundTrip in
  let _ = largeTickRoundTrip in
  let _ = roundTripConstructor in
  let _ = gregorianLeapCycle in
  let _ = gregorianCenturyException in
  ()

main : IO ()
main = do
  let _ = proofsChecked
  runtimePassed <- Test.Runtime.run
  regressionPassed <- Test.Regression.run
  gregorianPassed <- Test.Gregorian.run
  localTimePassed <- Test.LocalTime.run
  calendarDateTimePassed <- Test.CalendarDateTime.run
  finalizeResults
    [ ("proof tests (compile-time)", True)
    , ("runtime behavior tests", runtimePassed)
    , ("regression tests", regressionPassed)
    , ("Gregorian calendar tests", gregorianPassed)
    , ("local time tests", localTimePassed)
    , ("calendar date-time tests", calendarDateTimePassed)
    ]
