module Test.Tzif

import IotaTime
import IotaTime.Tzdb
import IotaTime.Tzdb.Tzif
import IotaTime.Tzdb.Windows
import Data.String
import Test.Support

zeros : Nat -> List Bits8
zeros Z = []
zeros (S count) = 0 :: zeros count

word32 : Integer -> List Bits8
word32 value =
  [ cast ((value `div` 16777216) `mod` 256)
  , cast ((value `div` 65536) `mod` 256)
  , cast ((value `div` 256) `mod` 256)
  , cast (value `mod` 256)
  ]

word16Little : Integer -> List Bits8
word16Little value =
  [ cast (value `mod` 256)
  , cast ((value `div` 256) `mod` 256)
  ]

word32Little : Integer -> List Bits8
word32Little value =
  let unsigned = if value < 0 then value + 4294967296 else value
   in word16Little unsigned ++ word16Little (unsigned `div` 65536)

systemTimeBytes : Integer -> Integer -> Integer -> Integer -> Integer ->
                  Integer -> Integer -> Integer -> List Bits8
systemTimeBytes year month weekday week hour minute second milliseconds =
  concatMap word16Little
    [year, month, weekday, week, hour, minute, second, milliseconds]

easternTziBytes : Integer -> List Bits8
easternTziBytes standardMilliseconds =
  word32Little 300 ++ word32Little 0 ++ word32Little (-60) ++
  systemTimeBytes 0 11 0 1 2 0 0 standardMilliseconds ++
  systemTimeBytes 0 3 0 2 2 0 0 0

easternBefore2007TziBytes : List Bits8
easternBefore2007TziBytes =
  word32Little 300 ++ word32Little 0 ++ word32Little (-60) ++
  systemTimeBytes 0 10 0 5 2 0 0 0 ++
  systemTimeBytes 0 4 0 1 2 0 0 0

easternRegistryZone : WindowsRegistryZone
easternRegistryZone = MkWindowsRegistryZone "Eastern Standard Time" "EST" "EDT"
  (easternTziBytes 0)
  [(2006, easternBefore2007TziBytes), (2007, easternTziBytes 0)]

hexCharacter : Integer -> Char
hexCharacter value = if value < 10
  then chr (cast '0' + cast value)
  else chr (cast 'A' + cast (value - 10))

hexString : List Bits8 -> String
hexString bytes = pack (concatMap encode bytes)
  where
    encode : Bits8 -> List Char
    encode byte =
      let value : Integer = cast byte
       in [hexCharacter (value `div` 16), hexCharacter (value `mod` 16)]

easternRegistryProtocol : String
easternRegistryProtocol = unlines
  [ "LOCAL\tEastern Standard Time"
  , "ZONE"
  , "ID\tEastern Standard Time"
  , "STD\tEST"
  , "DST\tEDT"
  , "TZI\t" ++ hexString (easternTziBytes 0)
  , "DYNAMIC\t2006\t" ++ hexString easternBefore2007TziBytes
  , "DYNAMIC\t2007\t" ++ hexString (easternTziBytes 0)
  , "END"
  ]

header : Bits8 -> Integer -> List Bits8
header version transitionCount =
  [84, 90, 105, 102, version] ++ zeros 15 ++
  word32 0 ++ word32 0 ++ word32 0 ++ word32 transitionCount ++
  word32 1 ++ word32 4

fixedPayload : List Bits8
fixedPayload = word32 0 ++ [0, 0] ++ [85, 84, 67, 0]

fixedPayload64 : List Bits8
fixedPayload64 = zeros 8 ++ [0, 0] ++ [85, 84, 67, 0]

version1Fixed : List Bits8
version1Fixed = header 0 0 ++ fixedPayload

version4Fixed : List Bits8
version4Fixed = header 52 0 ++ fixedPayload ++ header 52 0 ++
  fixedPayload ++ [10, 85, 84, 67, 48, 10]

invalidTypeIndex : List Bits8
invalidTypeIndex = header 0 1 ++ word32 0 ++ [1] ++ fixedPayload

easternTzif : TzifData
easternTzif = MkTzifData Version4
  (transitionInfo (offsetFromHours (-5)) False "EST") []
  (Just "EST5EDT,M3.2.0/2,M11.1.0/2")

springGapLocal : CalendarDateTime Gregorian
springGapLocal = on (localTime 2 30 0 0) (calendarDate 10 March 2024)

autumnOverlapLocal : CalendarDateTime Gregorian
autumnOverlapLocal = on (localTime 1 30 0 0) (calendarDate 3 November 2024)

windowsEastern : WindowsZoneRule
windowsEastern = MkWindowsZoneRule 300 0 (-60) "EST" "EDT"
  (MkWindowsTransitionDate 0 3 2 0 2 0 0)
  (MkWindowsTransitionDate 0 11 1 0 2 0 0)

windowsEasternZone : Either WindowsTimeZoneError TimeZone
windowsEasternZone = windowsRecurringTimeZone "Eastern Standard Time"
  (transitionInfo (offsetFromHours (-5)) False "EST") [] windowsEastern

windowsEasternBefore2007 : WindowsZoneRule
windowsEasternBefore2007 = MkWindowsZoneRule 300 0 (-60) "EST" "EDT"
  (MkWindowsTransitionDate 0 4 1 0 2 0 0)
  (MkWindowsTransitionDate 0 10 5 0 2 0 0)

windowsEasternDynamic : Either WindowsTimeZoneError TimeZone
windowsEasternDynamic = windowsDynamicTimeZone "Eastern Standard Time"
  windowsEastern
  [ MkWindowsDynamicRule 2006 windowsEasternBefore2007
  , MkWindowsDynamicRule 2007 windowsEastern
  ]

windowsSydney : WindowsZoneRule
windowsSydney = MkWindowsZoneRule (-600) 0 (-60) "AEST" "AEDT"
  (MkWindowsTransitionDate 0 10 1 0 2 0 0)
  (MkWindowsTransitionDate 0 4 1 0 3 0 0)

windowsFixedTwo : WindowsZoneRule
windowsFixedTwo = MkWindowsZoneRule (-120) 0 0 "TWO" "TWO"
  (MkWindowsTransitionDate 0 0 0 0 0 0 0)
  (MkWindowsTransitionDate 0 0 0 0 0 0 0)

tzifCases : List RuntimeCase
tzifCases =
  [ MkRuntimeCase "TZif v1 fixed zone is decoded"
      (case parseTzif version1Fixed of
        Right value => value.version == Version1 &&
          abbreviation value.initialTransition == "UTC" &&
          null value.transitions && value.posixFooter == Nothing
        Left _ => False)
  , MkRuntimeCase "TZif v4 POSIX footer is retained"
      (case parseTzif version4Fixed of
        Right value => value.version == Version4 &&
          value.posixFooter == Just "UTC0"
        Left _ => False)
  , MkRuntimeCase "TZif transition type indexes are bounds checked"
      (case parseTzif invalidTypeIndex of
        Left (InvalidTransitionTypeIndex 1) => True
        _ => False)
  , MkRuntimeCase "POSIX fixed offsets use the reversed sign convention"
      (case parsePosixZone "<+03>-3" of
        Right (PosixFixed info) => totalOffsetSeconds (utcOffset info) == 10800
        _ => False)
  , MkRuntimeCase "POSIX transition rule ranges are validated"
      (case parsePosixZone "EST5EDT,M13.2.0,M11.1.0" of
        Left (PosixRuleOutOfRange (MonthOutOfRange 13)) => True
        _ => False)
  , MkRuntimeCase "recurring footer enters daylight time in future years"
      (case timeZoneFromTzif "America/New_York" easternTzif of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710053999) ==
            offsetFromHours (-5) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710054000) ==
            offsetFromHours (-4)
        Left _ => False)
  , MkRuntimeCase "recurring footer returns to standard time"
      (case timeZoneFromTzif "America/New_York" easternTzif of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1730613599) ==
            offsetFromHours (-4) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1730613600) ==
            offsetFromHours (-5)
        Left _ => False)
  , MkRuntimeCase "recurring spring local time is skipped"
      (case timeZoneFromTzif "America/New_York" easternTzif of
        Right zone => case mapLocal zone springGapLocal of
            Skipped => True
            _ => False
        Left _ => False)
  , MkRuntimeCase "recurring spring gap shifts leniently"
      (case timeZoneFromTzif "America/New_York" easternTzif of
        Right zone => case fromCalendarDateTimeLeniently
          springGapLocal zone of
            Right value => IotaTime.ZonedDateTime.hour value == 3 &&
              IotaTime.ZonedDateTime.minute value == 30 &&
              zonedOffset value == offsetFromHours (-4)
            Left _ => False
        Left _ => False)
  , MkRuntimeCase "recurring autumn local time is ambiguous"
      (case timeZoneFromTzif "America/New_York" easternTzif of
        Right zone => case mapLocal zone autumnOverlapLocal of
            Ambiguous first second [] =>
              offsetOf first == offsetFromHours (-4) &&
              offsetOf second == offsetFromHours (-5)
            _ => False
        Left _ => False)
  , MkRuntimeCase "Windows bias rules enter daylight time"
      (case windowsEasternZone of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710053999) ==
            offsetFromHours (-5) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710054000) ==
            offsetFromHours (-4)
        _ => False)
  , MkRuntimeCase "Windows bias rules return to standard time"
      (case windowsEasternZone of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1730613599) ==
            offsetFromHours (-4) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1730613600) ==
            offsetFromHours (-5)
        _ => False)
    , MkRuntimeCase "binary Windows TZI values decode into zones"
        (case parseWindowsTzi "EST" "EDT" (easternTziBytes 0) of
          Right rule => case windowsTimeZone "Eastern Standard Time" rule of
            Right zone =>
              zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710053999) ==
                offsetFromHours (-5) &&
              zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1710054000) ==
                offsetFromHours (-4)
            Left _ => False
          Left _ => False)
    , MkRuntimeCase "binary Windows TZI length is exact"
        (case parseWindowsTzi "EST" "EDT" (zeros 43) of
          Left (WindowsTziLength 43) => True
          _ => False)
    , MkRuntimeCase "binary Windows TZI does not truncate milliseconds"
        (case parseWindowsTzi "EST" "EDT" (easternTziBytes 1) of
          Left (WindowsTransitionMillisecondsUnsupported 1) => True
          _ => False)
    , MkRuntimeCase "Windows registry snapshots attribute default TZI errors"
        (case windowsRegistryTimeZone
          (MkWindowsRegistryZone "Broken" "STD" "DST" (zeros 43) []) of
            Left (InvalidDefaultTzi (WindowsTziLength 43)) => True
            _ => False)
    , MkRuntimeCase "Windows registry command protocol converts Dynamic DST"
        (case parseWindowsRegistrySnapshot easternRegistryProtocol of
          Right snapshot => case snapshot.snapshotZones of
            [registry] => case windowsRegistryTimeZone registry of
              Right zone => snapshot.snapshotLocalZoneId ==
                  "Eastern Standard Time" &&
                zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1142856000) ==
                  offsetFromHours (-5) &&
                zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1174392000) ==
                  offsetFromHours (-4)
              Left _ => False
            _ => False
          Left _ => False)
    , MkRuntimeCase "Windows registry command protocol rejects malformed hex"
        (case parseWindowsRegistrySnapshot
          "LOCAL\tUTC\nZONE\nID\tUTC\nSTD\tUTC\nDST\tUTC\nTZI\t0G\nEND\n" of
            Left (InvalidRegistryHex "0G") => True
            _ => False)
    , MkRuntimeCase "Windows registry command protocol rejects missing END"
        (case parseWindowsRegistrySnapshot
          "LOCAL\tUTC\nZONE\nID\tUTC\nSTD\tUTC\nDST\tUTC\nTZI\t00\n" of
            Left IncompleteRegistryZone => True
            _ => False)
  , MkRuntimeCase "Windows transition clock fields are validated"
      (case windowsRecurringTimeZone "Invalid"
        (transitionInfo (offsetFromHours (-5)) False "EST") []
        (MkWindowsZoneRule 300 0 (-60) "EST" "EDT"
        (MkWindowsTransitionDate 0 3 2 0 24 0 0)
        (MkWindowsTransitionDate 0 11 1 0 2 0 0)) of
          Left (InvalidWindowsRule (WindowsTimeOutOfRange 24 0 0)) => True
          _ => False)
  , MkRuntimeCase "Windows absolute transition dates are not misread as recurring"
      (case windowsTimeZone "Absolute" (MkWindowsZoneRule 300 0 (-60)
        "EST" "EDT"
        (MkWindowsTransitionDate 2024 3 2 0 2 0 0)
        (MkWindowsTransitionDate 2024 11 1 0 2 0 0)) of
          Left (InvalidWindowsRule
            (WindowsAbsoluteTransitionUnsupported 2024)) => True
          _ => False)
  , MkRuntimeCase "Windows month-zero TZI produces a fixed zone"
      (case windowsTimeZone "India Standard Time" (MkWindowsZoneRule
        (-330) 0 0 "IST" "IST"
        (MkWindowsTransitionDate 0 0 0 0 0 0 0)
        (MkWindowsTransitionDate 0 0 0 0 0 0 0)) of
          Right zone => zoneOffsetAt zone
            (fromSecondsSinceUnixEpoch 4103697600) == offsetFromMinutes 330
          Left _ => False)
  , MkRuntimeCase "Windows partial daylight rules are rejected"
      (case windowsTimeZone "Partial" (MkWindowsZoneRule 0 0 (-60)
        "STD" "DST"
        (MkWindowsTransitionDate 0 3 2 0 2 0 0)
        (MkWindowsTransitionDate 0 0 0 0 0 0 0)) of
          Left (InvalidWindowsRule IncompleteWindowsDaylightRule) => True
          _ => False)
  , MkRuntimeCase "Windows Dynamic DST uses the pre-2007 spring rule"
      (case windowsEasternDynamic of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1143961199) ==
            offsetFromHours (-5) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1143961200) ==
            offsetFromHours (-4)
        Left _ => False)
  , MkRuntimeCase "Windows Dynamic DST switches rules in 2007"
      (case windowsEasternDynamic of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1173596399) ==
            offsetFromHours (-5) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1173596400) ==
            offsetFromHours (-4) &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1194156000) ==
            offsetFromHours (-5)
        Left _ => False)
  , MkRuntimeCase "southern recurrence remains daylight across New Year"
      (case windowsTimeZone "AUS Eastern Standard Time" windowsSydney of
        Right zone =>
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1704067200) ==
            offsetFromHours 11 &&
          zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1718409600) ==
            offsetFromHours 10
        Left _ => False)
  , MkRuntimeCase "Windows Dynamic DST supports fixed historical eras"
      (case windowsDynamicTimeZone "Mixed" windowsEastern
        [ MkWindowsDynamicRule 2000 windowsFixedTwo
        , MkWindowsDynamicRule 2001 windowsEastern
        ] of
          Right zone =>
            zoneOffsetAt zone (fromSecondsSinceUnixEpoch 962409600) ==
              offsetFromHours 2 &&
            zoneOffsetAt zone (fromSecondsSinceUnixEpoch 978307200) ==
              offsetFromHours (-5)
          Left _ => False)
  , MkRuntimeCase "Windows Dynamic DST years must increase"
      (case windowsDynamicTimeZone "Invalid" windowsEastern
        [ MkWindowsDynamicRule 2007 windowsEastern
        , MkWindowsDynamicRule 2006 windowsEasternBefore2007
        ] of
          Left DynamicYearsNotStrictlyIncreasing => True
          _ => False)
  ]

export
run : IO Bool
run = do
  purePassed <- runSuite "TZif tests" tzifCases
  let injectedZone = fixedDateTimeZone "Injected/Zone" (offsetFromHours 9)
  let injectedProvider = MkTimeZoneProvider
        (pure (Right injectedZone))
        (\_ => pure (Right injectedZone))
        (pure (Right injectedZone))
        (pure (Right ["Injected/Zone"]))
  let registrySnapshot = MkWindowsRegistrySnapshot "Eastern Standard Time"
        [easternRegistryZone]
  let registrySource = MkWindowsRegistrySource (pure (Right registrySnapshot))
  let registryProvider = windowsRegistryTimeZoneProvider registrySource
  let failingSource = MkWindowsRegistrySource
        (pure (Left "registry unavailable"))
  let failingRegistryProvider = windowsRegistryTimeZoneProvider failingSource
  injected <- timeZoneWith injectedProvider "anything"
  registryNamed <- timeZoneWith registryProvider "Eastern Standard Time"
  registryLocal <- localZoneWith registryProvider
  registryUtc <- utcWith registryProvider
  registryZones <- availableZonesWith registryProvider
  registryMissing <- timeZoneWith registryProvider "Missing"
  registryFailure <- timeZoneWith failingRegistryProvider "Eastern Standard Time"
  systemUtc <- utc
  systemNewYork <- timeZone "America/New_York"
  rejectedPath <- timeZone "../etc/passwd"
  systemLocal <- localZone
  listedZones <- availableZones
  discoveryPassed <- runSuite "TZDB discovery tests"
    [ MkRuntimeCase "platform providers are injectable"
        (case injected of
          Right value => zoneId value == "Injected/Zone"
          Left _ => False)
    , MkRuntimeCase "Windows registry provider loads Dynamic DST zones"
        (case registryNamed of
          Right value =>
            zoneOffsetAt value (fromSecondsSinceUnixEpoch 1142856000) ==
              offsetFromHours (-5) &&
            zoneOffsetAt value (fromSecondsSinceUnixEpoch 1174392000) ==
              offsetFromHours (-4)
          Left _ => False)
    , MkRuntimeCase "Windows registry provider resolves the local ID"
        (case registryLocal of
          Right value => zoneId value == "Eastern Standard Time"
          Left _ => False)
    , MkRuntimeCase "Windows registry provider supplies UTC independently"
        (case registryUtc of
          Right value => zoneOffsetAt value epoch == zeroOffset
          Left _ => False)
    , MkRuntimeCase "Windows registry provider enumerates zone IDs"
        (case registryZones of
          Right ["Eastern Standard Time"] => True
          _ => False)
    , MkRuntimeCase "Windows registry provider reports unknown IDs"
        (case registryMissing of
          Left (WindowsZoneNotFound "Missing") => True
          _ => False)
    , MkRuntimeCase "Windows registry provider preserves source failures"
        (case registryFailure of
          Left (WindowsRegistrySourceError "registry unavailable") => True
          _ => False)
    , MkRuntimeCase "system UTC zone is loaded"
        (case systemUtc of
          Right value => zoneOffsetAt value epoch == zeroOffset
          Left _ => False)
    , MkRuntimeCase "real TZif footer supplies standard time in 2100"
        (case systemNewYork of
          Right value => zoneOffsetAt value
            (fromSecondsSinceUnixEpoch 4103697600) == offsetFromHours (-5)
          Left _ => False)
    , MkRuntimeCase "real TZif footer supplies daylight time in 2100"
        (case systemNewYork of
          Right value => zoneOffsetAt value
            (fromSecondsSinceUnixEpoch 4119336000) == offsetFromHours (-4)
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
  pure (purePassed && discoveryPassed)