module WindowsSmoke

import IotaTime
import System

report : String -> Bool -> IO Bool
report name passed = do
  putStrLn ("  [" ++ (if passed then "PASS" else "FAIL") ++ "] " ++ name)
  pure passed

allPassed : List Bool -> Bool
allPassed [] = True
allPassed (passed :: rest) = passed && allPassed rest

zoneHourAt : TimeZone -> Integer -> Maybe Hour
zoneHourAt zone seconds = case IotaTime.ZonedDateTime.fromInstant
  {calendar = Gregorian} (fromSecondsSinceUnixEpoch seconds) zone of
    Right value => Just (IotaTime.ZonedDateTime.hour value)
    Left _ => Nothing

export
main : IO ()
main = do
  putStrLn "Running suite: Windows registry smoke tests"
  utcResult <- utc
  zonesResult <- availableZones
  easternResult <- timeZone "Eastern Standard Time"
  ianaEasternResult <- timeZone "America/New_York"
  localResult <- localZone
  metadataResult <- metadata
  missingResult <- timeZone "IotaTime Missing Zone"
  snapshotProviderResult <- windowsSnapshotTimeZoneProvider
  germanLocaleResult <- localeByName "de-DE"
  currentLocaleResult <- currentLocale
  missingLocaleResult <- localeByName "iotatime-LOCALE-DOES-NOT-EXIST"

  utcPassed <- report "UTC loads without registry dependence"
    (case utcResult of
      Right zone => zoneId zone == "UTC"
      Left _ => False)
  zonesPassed <- report "registry zones include Eastern Standard Time"
    (case zonesResult of
      Right zones => elem "Eastern Standard Time" zones
      Left _ => False)
  dynamicPassed <- report "registry Dynamic DST changes in 2007"
    (case easternResult of
      Right zone =>
        zoneHourAt zone 1142856000 == Just 7 &&
        zoneHourAt zone 1174392000 == Just 8
      Left _ => False)
  ianaPassed <- report "IANA names resolve through Windows ICU"
    (case ianaEasternResult of
      Right zone => zoneId zone == "America/New_York" &&
        zoneHourAt zone 1142856000 == Just 7 &&
        zoneHourAt zone 1174392000 == Just 8
      Left _ => False)
  localPassed <- report "local Windows zone resolves"
    (case localResult of
      Right _ => True
      Left _ => False)
  metadataPassed <- report "Windows metadata does not invent TZDB identity"
    (case metadataResult of
      Right value => value.tzdbVersion == Nothing &&
        null value.zoneAliases
      Left _ => False)
  missingPassed <- report "unknown Windows zone remains explicit"
    (case missingResult of
      Left (WindowsZoneNotFound "IotaTime Missing Zone") => True
      _ => False)
  snapshotPassed <- case snapshotProviderResult of
    Left _ => report "snapshot provider shares one valid registry view" False
    Right provider => do
      snapshotZones <- availableZonesWith provider
      snapshotEastern <- timeZoneWith provider "America/New_York"
      snapshotLocal <- localZoneWith provider
      report "snapshot provider shares one valid registry view"
        (case (snapshotZones, snapshotEastern, snapshotLocal) of
          (Right zones, Right eastern, Right _) =>
            elem "Eastern Standard Time" zones &&
            zoneId eastern == "America/New_York"
          _ => False)
  germanLocalePassed <- report "de-DE locale loads and compiles its date layout"
    (case germanLocaleResult of
      Left _ => False
      Right locale => case localeDatePattern locale of
        Left _ => False
        Right pattern =>
          IotaTime.Pattern.format pattern (IotaTime.Calendar.Gregorian.calendarDate 15 March 2020) ==
            "15.03.2020")
  currentLocalePassed <- report "current Windows locale is structurally complete"
    (case currentLocaleResult of
      Left _ => False
      Right locale =>
        localeId locale /= "" &&
        case (localeDatePattern locale, localeTimePattern locale) of
          (Right _, Right _) => True
          _ => False)
  missingLocalePassed <- report "unknown Windows locale remains explicit"
    (case missingLocaleResult of
      Left (LocaleNotFound "iotatime-LOCALE-DOES-NOT-EXIST") => True
      _ => False)

  if allPassed
    [ utcPassed, zonesPassed, dynamicPassed, ianaPassed, localPassed
    , metadataPassed
    , missingPassed, snapshotPassed
    , germanLocalePassed, currentLocalePassed, missingLocalePassed
    ]
    then putStrLn "All Windows registry smoke tests passed"
    else exitFailure
