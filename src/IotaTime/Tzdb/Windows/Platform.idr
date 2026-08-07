module IotaTime.Tzdb.Windows.Platform

import IotaTime.Tzdb.Provider
import IotaTime.Tzdb.Windows
import IotaTime.Tzdb.Windows.Types
import Data.List

%default total

%foreign "C:iotatime_windows_registry_snapshot, libiotatime_windows"
prim__windowsRegistrySnapshot : PrimIO AnyPtr

%foreign "C:iotatime_windows_snapshot_string, libiotatime_windows"
prim__windowsSnapshotString : AnyPtr -> String

%foreign "C:iotatime_windows_snapshot_free, libiotatime_windows"
prim__windowsSnapshotFree : AnyPtr -> PrimIO ()

%foreign "C:iotatime_windows_iana_to_windows, libiotatime_windows"
prim__windowsIanaToWindows : String -> PrimIO AnyPtr

%foreign "C:iotatime_windows_windows_to_iana, libiotatime_windows"
prim__windowsWindowsToIana : String -> PrimIO AnyPtr

mapLeft : (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert (Left error) = Left (convert error)
mapLeft convert (Right value) = Right value

convertZoneId : (String -> PrimIO AnyPtr) -> String -> IO (Maybe String)
convertZoneId convert value = do
  pointer <- primIO (convert value)
  if prim__nullAnyPtr pointer /= 0
    then pure Nothing
    else do
      let converted = prim__windowsSnapshotString pointer
      primIO (prim__windowsSnapshotFree pointer)
      pure (Just converted)

ianaToWindowsZone : String -> IO (Maybe String)
ianaToWindowsZone = convertZoneId prim__windowsIanaToWindows

windowsToIanaZone : String -> IO (Maybe String)
windowsToIanaZone = convertZoneId prim__windowsWindowsToIana

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
  converted <- ianaToWindowsZone name
  loaded <- windowsRegistryZones source
  let registryName = case converted of
        Nothing => name
        Just value => value
  pure $ do
    zones <- loaded
    registry <- case findWindowsZone registryName zones of
      Nothing => Left (WindowsZoneNotFound name)
      Just value => Right value
    mapLeft TzdbWindowsError (windowsRegistryTimeZoneAs name registry)

windowsRegistryLocalZone : WindowsRegistrySource ->
                           IO (Either TzdbError TimeZone)
windowsRegistryLocalZone source = do
  loaded <- windowsRegistrySnapshot source
  case loaded of
    Left error => pure (Left error)
    Right snapshot => do
      canonical <- windowsToIanaZone snapshot.snapshotLocalZoneId
      let valueId = case canonical of
            Nothing => snapshot.snapshotLocalZoneId
            Just value => value
      pure $ do
        registry <- case findWindowsZone snapshot.snapshotLocalZoneId
          snapshot.snapshotZones of
            Nothing => Left (WindowsZoneNotFound snapshot.snapshotLocalZoneId)
            Just value => Right value
        mapLeft TzdbWindowsError (windowsRegistryTimeZoneAs valueId registry)

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
  (pure (Right (MkTzdbMetadata Nothing [])))

||| Read a registry source once and return a provider with a consistent,
||| immutable view of its zones and local-zone identifier.
public export
windowsRegistrySnapshotProvider : WindowsRegistrySource ->
                                  IO (Either TzdbError TimeZoneProvider)
windowsRegistrySnapshotProvider source = do
  loaded <- windowsRegistrySnapshot source
  pure $ map
    (\snapshot => windowsRegistryTimeZoneProvider
      (MkWindowsRegistrySource (pure (Right snapshot))))
    loaded

protocolErrorMessage : WindowsRegistryProtocolError -> String
protocolErrorMessage MissingLocalZoneId = "missing local Windows zone identifier"
protocolErrorMessage (UnexpectedRegistryLine line) =
  "unexpected Windows registry output: " ++ line
protocolErrorMessage (InvalidRegistryHex value) =
  "invalid Windows registry hex value: " ++ value
protocolErrorMessage (InvalidDynamicRegistryLine line) =
  "invalid Windows Dynamic DST output: " ++ line
protocolErrorMessage IncompleteRegistryZone = "incomplete Windows registry zone"

nativeError : String -> Maybe String
nativeError source = map pack (strip (unpack "ERROR\t") (unpack source))
  where
    strip : List Char -> List Char -> Maybe (List Char)
    strip [] remaining = Just remaining
    strip (expected :: rest) (actual :: remaining) =
      if expected == actual then strip rest remaining else Nothing
    strip _ _ = Nothing

runWindowsNativeRegistry : IO (Either String WindowsRegistrySnapshot)
runWindowsNativeRegistry = do
  pointer <- primIO prim__windowsRegistrySnapshot
  if prim__nullAnyPtr pointer /= 0
    then pure (Left "native Windows registry snapshot allocation failed")
    else do
      let output = prim__windowsSnapshotString pointer
      primIO (prim__windowsSnapshotFree pointer)
      pure $ case nativeError output of
        Just error => Left error
        Nothing => case parseWindowsRegistrySnapshot output of
          Left error => Left (protocolErrorMessage error)
          Right snapshot => Right snapshot

||| Registry source backed by Win32 registry APIs through the native FFI.
public export
windowsNativeRegistrySource : WindowsRegistrySource
windowsNativeRegistrySource = MkWindowsRegistrySource
  runWindowsNativeRegistry
