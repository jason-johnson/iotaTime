module Test.TimeZone

import IotaTime
import Test.Support

zoneHourAt : TimeZone -> Integer -> Maybe Hour
zoneHourAt zone seconds = case IotaTime.ZonedDateTime.fromInstant
  {calendar = Gregorian} (fromSecondsSinceUnixEpoch seconds) zone of
    Right value => Just (IotaTime.ZonedDateTime.hour value)
    Left _ => Nothing

export
run : IO Bool
run = do
  systemUtc <- utc
  systemNewYork <- timeZone "America/New_York"
  rejectedPath <- timeZone "../etc/passwd"
  systemLocal <- localZone
  listedZones <- availableZones
  runSuite "time-zone provider tests"
    [ MkRuntimeCase "system UTC zone is loaded"
        (case systemUtc of
          Right value => zoneId value == "UTC" && show value == "<TimeZone UTC>"
          Left _ => False)
    , MkRuntimeCase "time-zone equality uses the zone identifier"
        (case (systemUtc, systemUtc) of
          (Right left, Right right) => left == right
          _ => False)
    , MkRuntimeCase "TZDB recurrence supplies standard time in 2100"
        (case systemNewYork of
          Right value => zoneHourAt value 4103697600 == Just 7
          Left _ => False)
    , MkRuntimeCase "TZDB recurrence supplies daylight time in 2100"
        (case systemNewYork of
          Right value => zoneHourAt value 4119336000 == Just 8
          Left _ => False)
    , MkRuntimeCase "zone names cannot escape the TZDB root"
        (case rejectedPath of
          Left (InvalidZoneName "../etc/passwd") => True
          _ => False)
    , MkRuntimeCase "local platform zone is decoded"
        (case systemLocal of
          Right _ => True
          Left _ => False)
    , MkRuntimeCase "available zones include UTC"
        (case listedZones of
          Right values => elem "UTC" values
          Left _ => False)
    ]