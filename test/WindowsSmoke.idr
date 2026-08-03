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

export
main : IO ()
main = do
  putStrLn "Running suite: Windows registry smoke tests"
  utcResult <- utc
  zonesResult <- availableZones
  easternResult <- timeZone "Eastern Standard Time"
  localResult <- localZone
  missingResult <- timeZone "IotaTime Missing Zone"

  utcPassed <- report "UTC loads without registry dependence"
    (case utcResult of
      Right zone => zoneOffsetAt zone epoch == zeroOffset
      Left _ => False)
  zonesPassed <- report "registry zones include Eastern Standard Time"
    (case zonesResult of
      Right zones => elem "Eastern Standard Time" zones
      Left _ => False)
  dynamicPassed <- report "registry Dynamic DST changes in 2007"
    (case easternResult of
      Right zone =>
        zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1142856000) ==
          offsetFromHours (-5) &&
        zoneOffsetAt zone (fromSecondsSinceUnixEpoch 1174392000) ==
          offsetFromHours (-4)
      Left _ => False)
  localPassed <- report "local Windows zone resolves"
    (case localResult of
      Right _ => True
      Left _ => False)
  missingPassed <- report "unknown Windows zone remains explicit"
    (case missingResult of
      Left (WindowsZoneNotFound "IotaTime Missing Zone") => True
      _ => False)

  if allPassed
    [utcPassed, zonesPassed, dynamicPassed, localPassed, missingPassed]
    then putStrLn "All Windows registry smoke tests passed"
    else exitFailure
