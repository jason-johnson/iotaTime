module Test.LocalTime

import IotaTime
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

betweenApplies : LocalTime -> LocalTime -> Bool
betweenApplies start end =
  applyPeriod (IotaTime.LocalTime.between start end) start == end

betweenReverses : LocalTime -> LocalTime -> Bool
betweenReverses start end =
  let forward = IotaTime.LocalTime.between start end
      backward = IotaTime.LocalTime.between end start
   in applyPeriod backward (applyPeriod forward start) == start

localTimeCases : List RuntimeCase
localTimeCases =
  [ MkRuntimeCase "local time exposes typed components"
      (timeComponents (localTime 23 59 58 999999999) == (23, 59, 58, 999999999))
    , MkRuntimeCase "local time show reconstructs its components"
            (show (localTime 4 30 2 7) == "localTime 4 30 2 7")
  , MkRuntimeCase "time-only periods wrap LocalTime"
      (timeComponents (applyPeriod (hours 2) (localTime 23 30 0 0)) == (1, 30, 0, 0))
  , MkRuntimeCase "negative subsecond periods wrap LocalTime"
      (timeComponents (applyPeriod (nanoseconds (-1)) (localTime 0 0 0 0)) ==
        (23, 59, 59, 999999999))
  , MkRuntimeCase "large time periods do not overflow the representation"
      (timeComponents (applyPeriod (hours 100000) (localTime 0 0 0 0)) == (16, 0, 0, 0))
  , MkRuntimeCase "combined time fields apply as one period"
      (timeComponents (applyPeriod (minutes 1 <+> seconds 30) (localTime 23 59 0 0)) ==
        (0, 0, 30, 0))
  , MkRuntimeCase "between advances a local time to a later value"
      (betweenApplies (localTime 9 15 30 100) (localTime 17 45 40 200))
  , MkRuntimeCase "between an earlier end produces a negative same-day period"
      (betweenApplies (localTime 23 30 0 0) (localTime 1 15 0 0))
  , MkRuntimeCase "between reverses by swapping its endpoints"
      (betweenReverses (localTime 4 5 6 7) (localTime 20 30 40 50))
  , MkRuntimeCase "dynamic local time accepts valid components"
    (case refineLocalTime 12 34 56 789 of
      Right value => value == localTime 12 34 56 789
      Left _ => False)
  , MkRuntimeCase "dynamic local time rejects invalid hour"
      (isLeft (refineLocalTime 24 0 0 0))
  , MkRuntimeCase "dynamic local time rejects invalid minute"
      (isLeft (refineLocalTime 0 60 0 0))
  , MkRuntimeCase "dynamic local time rejects invalid second"
      (isLeft (refineLocalTime 0 0 60 0))
  , MkRuntimeCase "dynamic local time rejects invalid nanosecond"
      (isLeft (refineLocalTime 0 0 0 1000000000))
  ]

export
run : IO Bool
run = runSuite "local time tests" localTimeCases