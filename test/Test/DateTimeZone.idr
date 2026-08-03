module Test.DateTimeZone

import IotaTime
import Test.Support

nanosecondsPerHour : Integer
nanosecondsPerHour = 3600 * 1000000000

localAt : Hour -> Minute -> CalendarDateTime Gregorian
localAt valueHour valueMinute = on
  (localTime valueHour valueMinute 0 0)
  (calendarDate 1 March 2000)

springZone : DateTimeZone
springZone = dateTimeZone "Test/Spring"
  (transitionInfo zeroOffset False "STD")
  [(0, transitionInfo (offsetFromHours 1) True "DST")]

fallZone : DateTimeZone
fallZone = dateTimeZone "Test/Fall"
  (transitionInfo (offsetFromHours 1) True "DST")
  [(0, transitionInfo zeroOffset False "STD")]

threeWayZone : DateTimeZone
threeWayZone = dateTimeZone "Test/ThreeWay"
  (transitionInfo (offsetFromHours 2) True "TWO")
  [(0, transitionInfo (offsetFromHours 1) True "ONE"),
   (nanosecondsPerHour, transitionInfo zeroOffset False "ZERO")]

dateTimeZoneCases : List RuntimeCase
dateTimeZoneCases =
  [ MkRuntimeCase "fixed zone preserves its identifier"
      (zoneId (fixedDateTimeZone "UTC" zeroOffset) == "UTC")
  , MkRuntimeCase "fixed zone has one offset at every instant"
      (zoneOffsetAt (fixedDateTimeZone "UTC+2" (offsetFromHours 2))
        (fromNanosecondsSinceEpoch (-999999999999)) == offsetFromHours 2)
  , MkRuntimeCase "transition uses the initial offset before its instant"
      (zoneOffsetAt springZone (fromNanosecondsSinceEpoch (-1)) == zeroOffset)
  , MkRuntimeCase "transition uses the new offset at its instant"
      (zoneOffsetAt springZone epoch == offsetFromHours 1)
  , MkRuntimeCase "active transition carries DST metadata"
      (let info = activeTransitionAt springZone epoch in
        isDaylightSavingTime info && abbreviation info == "DST")
  , MkRuntimeCase "later transition offset replaces the earlier offset"
      (zoneOffsetAt threeWayZone
        (fromNanosecondsSinceEpoch nanosecondsPerHour) == zeroOffset)
  , MkRuntimeCase "fixed-zone local mapping is unambiguous"
      (case mapLocal (fixedDateTimeZone "UTC+2" (offsetFromHours 2))
        (localAt 2 0) of
          Unambiguous value => toInstant value == epoch
          _ => False)
  , MkRuntimeCase "spring-forward local time is skipped"
      (case mapLocal springZone (localAt 0 30) of
          Skipped => True
          _ => False)
  , MkRuntimeCase "local time after spring transition is unambiguous"
      (case mapLocal springZone (localAt 1 30) of
          Unambiguous value => offsetOf value == offsetFromHours 1 &&
            toInstant value == fromNanosecondsSinceEpoch (nanosecondsPerHour `div` 2)
          _ => False)
  , MkRuntimeCase "fall-back local time is ambiguous in instant order"
      (case mapLocal fallZone (localAt 0 30) of
          Ambiguous earlier later [] =>
            offsetOf earlier == offsetFromHours 1 &&
            offsetOf later == zeroOffset &&
            toInstant earlier < toInstant later
          _ => False)
  , MkRuntimeCase "pathological zone retains every ambiguous candidate"
      (case mapLocal threeWayZone (localAt 1 30) of
          Ambiguous first second [third] =>
            toInstant first < toInstant second &&
            toInstant second < toInstant third
          _ => False)
  , MkRuntimeCase "runtime ordered transitions are accepted"
      (case refineDateTimeZone "Runtime"
        (transitionInfo zeroOffset False "STD")
        [(fromNanosecondsSinceEpoch 0,
          transitionInfo (offsetFromHours 1) True "DST"),
         (fromNanosecondsSinceEpoch nanosecondsPerHour,
          transitionInfo zeroOffset False "STD")] of
          Right value => zoneOffsetAt value epoch == offsetFromHours 1
          Left _ => False)
  , MkRuntimeCase "runtime duplicate transitions are rejected"
      (case refineDateTimeZone "Runtime"
        (transitionInfo zeroOffset False "STD")
        [(epoch, transitionInfo (offsetFromHours 1) True "DST"),
         (epoch, transitionInfo zeroOffset False "STD")] of
          Left TransitionsNotStrictlyIncreasing => True
          Left _ => False
          Right _ => False)
  , MkRuntimeCase "runtime reversed transitions are rejected"
      (case refineDateTimeZone "Runtime"
        (transitionInfo zeroOffset False "STD")
        [(fromNanosecondsSinceEpoch 1,
          transitionInfo (offsetFromHours 1) True "DST"),
         (epoch, transitionInfo zeroOffset False "STD")] of
          Left TransitionsNotStrictlyIncreasing => True
          Left _ => False
          Right _ => False)
  ]

export
run : IO Bool
run = runSuite "date-time zone tests" dateTimeZoneCases
