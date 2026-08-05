module Test.TimeZone

import IotaTime
import Test.Support

zoneHourAt : TimeZone -> Integer -> Maybe Hour
zoneHourAt zone seconds = case IotaTime.ZonedDateTime.fromInstant
  {calendar = Gregorian} (fromSecondsSinceUnixEpoch seconds) zone of
    Right value => Just (IotaTime.ZonedDateTime.hour value)
    Left _ => Nothing

testStandardInfo : TransitionInfo
testStandardInfo = transitionInfo IotaTime.Offset.empty False "TST"

testDaylightInfo : TransitionInfo
testDaylightInfo = transitionInfoWithSavings
  (IotaTime.Offset.fromHours 1) (IotaTime.Offset.fromHours 1) "TDT"

testTransitionZone : Either DateTimeZoneError TimeZone
testTransitionZone = refineDateTimeZone "Test/Transitions" testStandardInfo
  [ (fromNanosecondsSinceEpoch 1000000000, testDaylightInfo)
  , (fromNanosecondsSinceEpoch 2000000000, testStandardInfo)
  ]

intervalContains : ZoneInterval -> Instant -> Bool
intervalContains interval instant =
  maybe True (\start => start <= instant) (intervalStart interval) &&
  maybe True (\end => instant < end) (intervalEnd interval)

tzdataParserWorks : Bool
tzdataParserWorks =
  let (version, aliases) = parseTzdataIdentity
        "# version 2026a\nL Test/Canonical Test/Alias\n"
      metadata = MkTzdbMetadata version Nothing aliases []
   in version == Just "2026a" &&
      canonicalZoneId metadata "Test/Alias" == "Test/Canonical" &&
      canonicalZoneId metadata "Test/Canonical" == "Test/Canonical"

export
run : IO Bool
run = do
  systemUtc <- utc
  systemNewYork <- timeZone "America/New_York"
  rejectedPath <- timeZone "../etc/passwd"
  systemLocal <- localZone
  listedZones <- availableZones
  systemMetadata <- metadata
  runSuite "time-zone provider tests"
    [ MkRuntimeCase "system UTC zone is loaded"
        (case systemUtc of
          Right value => zoneId value == "UTC" && show value == "<TimeZone UTC>"
          Left _ => False)
    , MkRuntimeCase "time-zone equality uses the zone identifier"
        (case (systemUtc, systemUtc) of
          (Right left, Right right) => left == right
          _ => False)
    , MkRuntimeCase "fixed zone interval is unbounded with zero savings"
        (let interval = zoneIntervalAt
               (fixedDateTimeZone "Fixed/+02" (IotaTime.Offset.fromHours 2))
               (fromNanosecondsSinceEpoch 0)
          in intervalStart interval == Nothing &&
             intervalEnd interval == Nothing &&
             totalOffsetSeconds (wallOffset interval) == 7200 &&
             savings interval == Just IotaTime.Offset.empty &&
             intervalAbbreviation interval == "Fixed/+02")
    , MkRuntimeCase "explicit zone interval exposes half-open bounds and state"
        (case testTransitionZone of
          Right zone =>
            let query = fromNanosecondsSinceEpoch 1500000000
                interval = zoneIntervalAt zone query
             in intervalStart interval ==
                  Just (fromNanosecondsSinceEpoch 1000000000) &&
                intervalEnd interval ==
                  Just (fromNanosecondsSinceEpoch 2000000000) &&
                intervalContains interval query &&
                totalOffsetSeconds (wallOffset interval) == 3600 &&
                savings interval == Just (IotaTime.Offset.fromHours 1) &&
                intervalIsDaylightSavingTime interval &&
                intervalAbbreviation interval == "TDT"
          Left _ => False)
    , MkRuntimeCase "zone transition instant starts the following interval"
        (case testTransitionZone of
          Right zone =>
            let boundary = fromNanosecondsSinceEpoch 2000000000
                interval = zoneIntervalAt zone boundary
             in intervalStart interval == Just boundary &&
                intervalEnd interval == Nothing &&
                intervalContains interval boundary &&
                totalOffsetSeconds (wallOffset interval) == 0 &&
                not (intervalIsDaylightSavingTime interval)
          Left _ => False)
    , MkRuntimeCase "TZDB recurrence supplies standard time in 2100"
        (case systemNewYork of
          Right value => zoneHourAt value 4103697600 == Just 7
          Left _ => False)
    , MkRuntimeCase "TZDB recurrence supplies daylight time in 2100"
        (case systemNewYork of
          Right value => zoneHourAt value 4119336000 == Just 8
          Left _ => False)
    , MkRuntimeCase "TZDB recurrence interval exposes standard state"
        (case systemNewYork of
          Right value =>
            let query = fromSecondsSinceUnixEpoch 4103697600
                interval = zoneIntervalAt value query
             in intervalContains interval query &&
                intervalStart interval /= Nothing &&
                intervalEnd interval /= Nothing &&
                totalOffsetSeconds (wallOffset interval) == -18000 &&
                savings interval == Just IotaTime.Offset.empty &&
                intervalAbbreviation interval == "EST"
          Left _ => False)
    , MkRuntimeCase "TZDB recurrence interval exposes daylight savings"
        (case systemNewYork of
          Right value =>
            let query = fromSecondsSinceUnixEpoch 4119336000
                interval = zoneIntervalAt value query
             in intervalContains interval query &&
                totalOffsetSeconds (wallOffset interval) == -14400 &&
                savings interval == Just (IotaTime.Offset.fromHours 1) &&
                intervalIsDaylightSavingTime interval &&
                intervalAbbreviation interval == "EDT"
          Left _ => False)
    , MkRuntimeCase "TZIF interval infers historical daylight savings"
        (case systemNewYork of
          Right value =>
            let standard = zoneIntervalAt value
                  (fromSecondsSinceUnixEpoch 1704067200)
                daylight = zoneIntervalAt value
                  (fromSecondsSinceUnixEpoch 1720051200)
             in savings standard == Just IotaTime.Offset.empty &&
                savings daylight == Just (IotaTime.Offset.fromHours 1) &&
                intervalAbbreviation standard == "EST" &&
                intervalAbbreviation daylight == "EDT"
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
    , MkRuntimeCase "tzdata identity parser reads version and aliases"
        tzdataParserWorks
    , MkRuntimeCase "system metadata exposes TZDB and CLDR versions"
        (case systemMetadata of
          Right value => value.tzdbVersion /= Nothing &&
            value.cldrVersion == Just "48"
          Left _ => False)
    , MkRuntimeCase "system metadata canonicalizes a TZDB alias"
        (case systemMetadata of
          Right value => canonicalZoneId value "US/Eastern" ==
            "America/New_York"
          Left _ => False)
    , MkRuntimeCase "Windows IDs resolve globally and by territory"
        (case systemMetadata of
          Right value =>
            ianaZoneIdsForWindows value "Eastern Standard Time" "001" ==
              ["America/New_York"] &&
            elem "America/Detroit"
              (ianaZoneIdsForWindows value "Eastern Standard Time" "US")
          Left _ => False)
    , MkRuntimeCase "IANA aliases resolve back to Windows IDs"
        (case systemMetadata of
          Right value =>
            windowsZoneIdForIana value "US/Eastern" "US" ==
              Just "Eastern Standard Time"
          Left _ => False)
    ]