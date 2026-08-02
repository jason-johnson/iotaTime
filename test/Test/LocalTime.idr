module Test.LocalTime

import IotaTime
import Test.Support

isLeft : Either left right -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

timeComponents : LocalTime -> (Hour, Minute, Second, Nanosecond)
timeComponents value = (hour value, minute value, second value, nanosecond value)

localTimeCases : List RuntimeCase
localTimeCases =
  [ MkRuntimeCase "local time exposes typed components"
      (timeComponents (localTime 23 59 58 999999999) == (23, 59, 58, 999999999))
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