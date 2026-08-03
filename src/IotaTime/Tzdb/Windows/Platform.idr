module IotaTime.Tzdb.Windows.Platform

import public IotaTime.Tzdb.Provider
import public IotaTime.Tzdb.Windows
import Data.List
import System.File.Process
import System.File.ReadWrite

%default total

mapLeft : (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert (Left error) = Left (convert error)
mapLeft convert (Right value) = Right value

||| Native Windows adapters provide one atomic registry snapshot containing
||| both the available zones and locally configured Windows zone identifier.
public export
record WindowsRegistrySource where
  constructor MkWindowsRegistrySource
  sourceRegistrySnapshot : IO (Either String WindowsRegistrySnapshot)

windowsRegistrySnapshot : WindowsRegistrySource ->
                          IO (Either TzdbError WindowsRegistrySnapshot)
windowsRegistrySnapshot source = do
  loaded <- source.sourceRegistrySnapshot
  pure (mapLeft WindowsRegistrySourceError loaded)

windowsRegistryZones : WindowsRegistrySource ->
                       IO (Either TzdbError (List WindowsRegistryZone))
windowsRegistryZones source = map (map snapshotZones)
  (windowsRegistrySnapshot source)

findWindowsZone : String -> List WindowsRegistryZone ->
                  Maybe WindowsRegistryZone
findWindowsZone name [] = Nothing
findWindowsZone name (zone :: rest) =
  if zone.registryZoneId == name then Just zone else findWindowsZone name rest

windowsRegistryNamedZone : WindowsRegistrySource -> String ->
                           IO (Either TzdbError TimeZone)
windowsRegistryNamedZone source name = do
  loaded <- windowsRegistryZones source
  pure $ do
    zones <- loaded
    registry <- case findWindowsZone name zones of
      Nothing => Left (WindowsZoneNotFound name)
      Just value => Right value
    mapLeft TzdbWindowsError (windowsRegistryTimeZone registry)

windowsRegistryLocalZone : WindowsRegistrySource ->
                           IO (Either TzdbError TimeZone)
windowsRegistryLocalZone source = do
  loaded <- windowsRegistrySnapshot source
  pure $ do
    snapshot <- loaded
    registry <- case findWindowsZone snapshot.snapshotLocalZoneId
      snapshot.snapshotZones of
        Nothing => Left (WindowsZoneNotFound snapshot.snapshotLocalZoneId)
        Just value => Right value
    mapLeft TzdbWindowsError (windowsRegistryTimeZone registry)

windowsRegistryAvailableZones : WindowsRegistrySource ->
                                IO (Either TzdbError (List String))
windowsRegistryAvailableZones source = do
  loaded <- windowsRegistryZones source
  pure (map (sort . map registryZoneId) loaded)

||| Build a provider around a Windows registry reader.
public export
windowsRegistryTimeZoneProvider : WindowsRegistrySource -> TimeZoneProvider
windowsRegistryTimeZoneProvider source = MkTimeZoneProvider
  (pure (Right (fixedDateTimeZone "UTC" zeroOffset)))
  (windowsRegistryNamedZone source)
  (windowsRegistryLocalZone source)
  (windowsRegistryAvailableZones source)

windowsRegistryScript : String
windowsRegistryScript = concat
  [ "$ErrorActionPreference='Stop';"
  , "[Console]::OutputEncoding=New-Object System.Text.UTF8Encoding($false);"
  , "$zones='Registry::HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones';"
  , "$local=(Get-ItemProperty 'Registry::HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation').TimeZoneKeyName;"
  , "if([string]::IsNullOrEmpty($local)){throw 'TimeZoneKeyName is empty'};"
  , "Write-Output (\"LOCAL`t\"+$local);"
  , "Get-ChildItem $zones|Sort-Object PSChildName|ForEach-Object{"
  , "$zone=Get-ItemProperty $_.PSPath;"
  , "Write-Output 'ZONE';"
  , "Write-Output (\"ID`t\"+$_.PSChildName);"
  , "Write-Output (\"STD`t\"+$zone.Std);"
  , "Write-Output (\"DST`t\"+$zone.Dlt);"
  , "$hex=-join($zone.TZI|ForEach-Object{$_.ToString('X2')});"
  , "Write-Output (\"TZI`t\"+$hex);"
  , "$dynamic=Join-Path $_.PSPath 'Dynamic DST';"
  , "if(Test-Path $dynamic){"
  , "$values=Get-ItemProperty $dynamic;"
  , "$values.PSObject.Properties|Where-Object{$_.Name -match '^\\d{4}$'}|"
  , "Sort-Object {[int]$_.Name}|ForEach-Object{"
  , "$dynamicHex=-join($_.Value|ForEach-Object{$_.ToString('X2')});"
  , "Write-Output (\"DYNAMIC`t\"+$_.Name+\"`t\"+$dynamicHex)}};"
  , "Write-Output 'END'}"
  ]

protocolErrorMessage : WindowsRegistryProtocolError -> String
protocolErrorMessage MissingLocalZoneId = "missing local Windows zone identifier"
protocolErrorMessage (UnexpectedRegistryLine line) =
  "unexpected Windows registry output: " ++ line
protocolErrorMessage (InvalidRegistryHex value) =
  "invalid Windows registry hex value: " ++ value
protocolErrorMessage (InvalidDynamicRegistryLine line) =
  "invalid Windows Dynamic DST output: " ++ line
protocolErrorMessage IncompleteRegistryZone = "incomplete Windows registry zone"

runWindowsRegistryScript : IO (Either String WindowsRegistrySnapshot)
runWindowsRegistryScript = do
  opened <- System.File.Process.Escaped.popen
    [ "powershell.exe", "-NoLogo", "-NoProfile", "-NonInteractive"
    , "-ExecutionPolicy", "Bypass", "-Command", windowsRegistryScript
    ] Read
  case opened of
    Left error => pure (Left (show error))
    Right process => do
      output <- assert_total (fRead process)
      exitCode <- pclose process
      case output of
        Left error => pure (Left (show error))
        Right value => if exitCode /= 0
          then pure (Left ("PowerShell registry query exited with code " ++
            show exitCode))
          else pure (case parseWindowsRegistrySnapshot value of
            Left error => Left (protocolErrorMessage error)
            Right snapshot => Right snapshot)

||| Registry source using built-in Windows PowerShell and its Registry provider.
public export
windowsPowerShellRegistrySource : WindowsRegistrySource
windowsPowerShellRegistrySource = MkWindowsRegistrySource
  runWindowsRegistryScript
