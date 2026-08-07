module Test.TimeZone

import IotaTime
import IotaTime.Tzdb.Windows.Platform
import IotaTime.Tzdb.Windows.Types
import Test.Support
import Data.IORef

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
      metadata = MkTzdbMetadata version aliases
   in version == Just "2026a" &&
      canonicalZoneId metadata "Test/Alias" == "Test/Canonical" &&
      canonicalZoneId metadata "Test/Canonical" == "Test/Canonical"

counted : IORef Integer -> IO value -> IO value
counted reference action = do
  count <- readIORef reference
  writeIORef reference (count + 1)
  action

hasZoneId : String -> Either TzdbError TimeZone -> Bool
hasZoneId expected (Right zone) = zoneId zone == expected
hasZoneId expected (Left _) = False

isMissingZone : String -> Either TzdbError TimeZone -> Bool
isMissingZone expected (Left (WindowsZoneNotFound actual)) = expected == actual
isMissingZone expected _ = False

hasAvailableZones : List String -> Either TzdbError (List String) -> Bool
hasAvailableZones expected (Right actual) = expected == actual
hasAvailableZones expected (Left _) = False

hasTzdbVersion : Maybe String -> Either TzdbError TzdbMetadata -> Bool
hasTzdbVersion expected (Right actual) = actual.tzdbVersion == expected
hasTzdbVersion expected (Left _) = False

cachePolicyWorks : IO Bool
cachePolicyWorks = do
  namedCount <- newIORef 0
  localCount <- newIORef 0
  availableCount <- newIORef 0
  metadataCount <- newIORef 0
  let base = MkTimeZoneProvider
        (pure (Right (fixedDateTimeZone "UTC" zeroOffset)))
        (\name => counted namedCount $ pure $
          if name == "Missing"
            then Left (WindowsZoneNotFound name)
            else Right (fixedDateTimeZone name zeroOffset))
        (counted localCount $
          pure (Right (fixedDateTimeZone "Local" zeroOffset)))
        (counted availableCount $ pure (Right ["Test/A", "Test/B"]))
        (counted metadataCount $
          pure (Right (MkTzdbMetadata (Just "test") [])))
  cached <- cachedTimeZoneProvider defaultTimeZoneCachePolicy base
  firstA <- timeZoneWith cached "Test/A"
  secondA <- timeZoneWith cached "Test/A"
  firstB <- timeZoneWith cached "Test/B"
  firstMissing <- timeZoneWith cached "Missing"
  secondMissing <- timeZoneWith cached "Missing"
  firstLocal <- localZoneWith cached
  secondLocal <- localZoneWith cached
  firstAvailable <- availableZonesWith cached
  secondAvailable <- availableZonesWith cached
  firstMetadata <- metadataWith cached
  secondMetadata <- metadataWith cached
  cachedLocal <- cachedTimeZoneProvider
    (MkTimeZoneCachePolicy False False False True) base
  thirdLocal <- localZoneWith cachedLocal
  fourthLocal <- localZoneWith cachedLocal
  namedCalls <- readIORef namedCount
  localCalls <- readIORef localCount
  availableCalls <- readIORef availableCount
  metadataCalls <- readIORef metadataCount
  pure $
    hasZoneId "Test/A" firstA && hasZoneId "Test/A" secondA &&
    hasZoneId "Test/B" firstB &&
    isMissingZone "Missing" firstMissing &&
    isMissingZone "Missing" secondMissing &&
    hasZoneId "Local" firstLocal && hasZoneId "Local" secondLocal &&
    hasZoneId "Local" thirdLocal && hasZoneId "Local" fourthLocal &&
    hasAvailableZones ["Test/A", "Test/B"] firstAvailable &&
    hasAvailableZones ["Test/A", "Test/B"] secondAvailable &&
    hasTzdbVersion (Just "test") firstMetadata &&
    hasTzdbVersion (Just "test") secondMetadata &&
    namedCalls == 4 && localCalls == 3 &&
    availableCalls == 1 && metadataCalls == 1

windowsSnapshotReadsOnce : IO Bool
windowsSnapshotReadsOnce = do
  sourceCount <- newIORef 0
  let snapshot = MkWindowsRegistrySnapshot "Missing Local" []
      source = MkWindowsRegistrySource
        (counted sourceCount (pure (Right snapshot)))
  loaded <- windowsRegistrySnapshotProvider source
  case loaded of
    Left _ => pure False
    Right provider => do
      firstAvailable <- availableZonesWith provider
      secondAvailable <- availableZonesWith provider
      local <- localZoneWith provider
      reads <- readIORef sourceCount
      pure $
        hasAvailableZones [] firstAvailable &&
        hasAvailableZones [] secondAvailable &&
        isMissingZone "Missing Local" local &&
        reads == 1

export
run : IO Bool
run = do
  systemUtc <- utc
  systemNewYork <- timeZone "America/New_York"
  rejectedPath <- timeZone "../etc/passwd"
  systemLocal <- localZone
  listedZones <- availableZones
  systemMetadata <- metadata
  cachePassed <- cachePolicyWorks
  snapshotPassed <- windowsSnapshotReadsOnce
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
    , MkRuntimeCase "system metadata exposes a TZDB version"
        (case systemMetadata of
          Right value => value.tzdbVersion /= Nothing
          Left _ => False)
    , MkRuntimeCase "system metadata canonicalizes a TZDB alias"
        (case systemMetadata of
          Right value => canonicalZoneId value "US/Eastern" ==
            "America/New_York"
          Left _ => False)
    , MkRuntimeCase "opt-in provider caching retains only successful stable queries"
        cachePassed
    , MkRuntimeCase "Windows snapshot providers read their registry source once"
      snapshotPassed
    ]