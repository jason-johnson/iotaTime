module Test.Tzif

import IotaTime
import IotaTime.Tzdb
import IotaTime.Tzdb.Tzif
import IotaTime.Tzdb.Windows
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
  (MkWindowsTransitionDate 3 2 0 2 0 0)
  (MkWindowsTransitionDate 11 1 0 2 0 0)

windowsEasternZone : Either WindowsTimeZoneError TimeZone
windowsEasternZone = windowsRecurringTimeZone "Eastern Standard Time"
  (transitionInfo (offsetFromHours (-5)) False "EST") [] windowsEastern

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
  , MkRuntimeCase "Windows transition clock fields are validated"
      (case windowsRecurringTimeZone "Invalid"
        (transitionInfo (offsetFromHours (-5)) False "EST") []
        (MkWindowsZoneRule 300 0 (-60) "EST" "EDT"
        (MkWindowsTransitionDate 3 2 0 24 0 0)
        (MkWindowsTransitionDate 11 1 0 2 0 0)) of
          Left (InvalidWindowsRule (WindowsTimeOutOfRange 24 0 0)) => True
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
  injected <- timeZoneWith injectedProvider "anything"
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