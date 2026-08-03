module IotaTime.Tzdb

import public IotaTime.Tzdb.Tzif
import public IotaTime.Tzdb.Posix
import public IotaTime.Tzdb.Windows
import public Data.Buffer
import public System.File.Buffer
import public System
import public System.Directory
import public System.Info
import Data.List

%default total

public export
data TzdbError
  = TzdbFileError String
  | TzdbParseError TzifError
  | TzdbPosixError PosixTzError
  | TzdbZoneError DateTimeZoneError
  | TzdbWindowsError WindowsRegistryError
  | WindowsRegistrySourceError String
  | WindowsZoneNotFound String
  | InvalidZoneName String
  | UnsupportedPlatform String

||| Platform-specific time-zone discovery. Native adapters can provide
||| registry-backed implementations without coupling them to TZif decoding.
public export
record TimeZoneProvider where
  constructor MkTimeZoneProvider
  providerUtc : IO (Either TzdbError TimeZone)
  providerTimeZone : String -> IO (Either TzdbError TimeZone)
  providerLocalZone : IO (Either TzdbError TimeZone)
  providerAvailableZones : IO (Either TzdbError (List String))

||| Native Windows adapters only need to enumerate registry snapshots and
||| report the locally configured Windows zone identifier.
public export
record WindowsRegistrySource where
  constructor MkWindowsRegistrySource
  sourceRegistryZones : IO (Either String (List WindowsRegistryZone))
  sourceLocalZoneId : IO (Either String String)

mapLeft : (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert (Left error) = Left (convert error)
mapLeft convert (Right value) = Right value

||| Convert decoded TZif data into an invariant-preserving zone. A recurring
||| footer controls instants after the final explicit transition; absent and
||| fixed footers preserve the final explicit state indefinitely.
public export
timeZoneFromTzif : String -> TzifData -> Either TzdbError TimeZone
timeZoneFromTzif valueId decoded = case decoded.posixFooter of
  Nothing => finiteZone
  Just "" => finiteZone
  Just footer => do
    parsed <- mapLeft TzdbPosixError (parsePosixZone footer)
    case parsed of
      PosixFixed _ => finiteZone
      PosixRecurring recurrence => mapLeft TzdbZoneError
        (refineRecurringDateTimeZone valueId decoded.initialTransition
          decoded.transitions recurrence)
  where
    finiteZone : Either TzdbError TimeZone
    finiteZone = mapLeft TzdbZoneError
      (refineDateTimeZone valueId decoded.initialTransition decoded.transitions)

bufferBytes : Buffer -> IO (List Bits8)
bufferBytes buffer = do
  size <- rawSize buffer
  readBytes (cast size) 0
  where
    readBytes : Nat -> Int -> IO (List Bits8)
    readBytes Z offset = pure []
    readBytes (S count) offset = do
      byte <- getBits8 buffer offset
      remaining <- readBytes count (offset + 1)
      pure (byte :: remaining)

||| Read and decode one TZif file. Filesystem and format failures remain
||| distinct typed trust-boundary errors.
public export
loadTzifFile : String -> IO (Either TzdbError TzifData)
loadTzifFile path = do
  loaded <- createBufferFromFile path
  case loaded of
    Left error => pure (Left (TzdbFileError (show error)))
    Right buffer => do
      bytes <- bufferBytes buffer
      pure (mapLeft TzdbParseError (parseTzif bytes))

||| Read, decode, and validate one TZif file as a time zone.
public export
loadTimeZoneFile : String -> String -> IO (Either TzdbError TimeZone)
loadTimeZoneFile valueId path = do
  decoded <- loadTzifFile path
  pure (decoded >>= timeZoneFromTzif valueId)

zoneInfoRoot : IO String
zoneInfoRoot = do
  configured <- getEnv "TZDIR"
  pure (case configured of
    Just value => if value == "" then "/usr/share/zoneinfo" else value
    Nothing => "/usr/share/zoneinfo")

pathComponents : List Char -> List (List Char)
pathComponents source = go [] source
  where
    go : List Char -> List Char -> List (List Char)
    go current [] = [reverse current]
    go current ('/' :: rest) = reverse current :: go [] rest
    go current (value :: rest) = go (value :: current) rest

validZoneName : String -> Bool
validZoneName source =
  let characters = unpack source
   in not (null characters) && headIsRelative characters &&
      all validComponent (pathComponents characters)
  where
    headIsRelative : List Char -> Bool
    headIsRelative ('/' :: _) = False
    headIsRelative _ = True

    validComponent : List Char -> Bool
    validComponent [] = False
    validComponent ['.'] = False
    validComponent ['.', '.'] = False
    validComponent values = all (/= '\0') values

zonePath : String -> String -> String
zonePath root name = root ++ "/" ++ name

unixTimeZone : String -> IO (Either TzdbError TimeZone)
unixTimeZone name = if validZoneName name
  then do
    root <- zoneInfoRoot
    loadTimeZoneFile name (zonePath root name)
  else pure (Left (InvalidZoneName name))

unixUtc : IO (Either TzdbError TimeZone)
unixUtc = unixTimeZone "UTC"

stripLeadingColon : String -> String
stripLeadingColon source = case unpack source of
  ':' :: rest => pack rest
  _ => source

unixLocalZone : IO (Either TzdbError TimeZone)
unixLocalZone = do
  configured <- getEnv "TZ"
  case configured of
    Nothing => loadTimeZoneFile "local" "/etc/localtime"
    Just "" => unixUtc
    Just source =>
      let value = stripLeadingColon source
       in case unpack value of
            '/' :: _ => loadTimeZoneFile value value
            _ => unixTimeZone value

collectZonePath : String -> String -> IO (List String)
collectZonePath root relative = do
  let path = if relative == "" then root else zonePath root relative
  listed <- listDir path
  case listed of
    Right entries => assert_total (collectEntries entries)
    Left _ => do
      decoded <- loadTzifFile path
      pure (case decoded of
        Right _ => [relative]
        Left _ => [])
  where
    collectEntries : List String -> IO (List String)
    collectEntries [] = pure []
    collectEntries (entry :: rest) = do
      let child = if relative == "" then entry else relative ++ "/" ++ entry
      found <- collectZonePath root child
      remaining <- collectEntries rest
      pure (found ++ remaining)

unixAvailableZones : IO (Either TzdbError (List String))
unixAvailableZones = do
  root <- zoneInfoRoot
  listed <- listDir root
  case listed of
    Left error => pure (Left (TzdbFileError (show error)))
    Right _ => map (Right . sort) (collectZonePath root "")

||| The built-in Unix filesystem provider.
public export
unixTimeZoneProvider : TimeZoneProvider
unixTimeZoneProvider = MkTimeZoneProvider unixUtc unixTimeZone
  unixLocalZone unixAvailableZones

windowsRegistryZones : WindowsRegistrySource ->
                       IO (Either TzdbError (List WindowsRegistryZone))
windowsRegistryZones source = do
  loaded <- source.sourceRegistryZones
  pure (mapLeft WindowsRegistrySourceError loaded)

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
  loadedId <- source.sourceLocalZoneId
  case loadedId of
    Left error => pure (Left (WindowsRegistrySourceError error))
    Right zoneId => windowsRegistryNamedZone source zoneId

windowsRegistryAvailableZones : WindowsRegistrySource ->
                                IO (Either TzdbError (List String))
windowsRegistryAvailableZones source = do
  loaded <- windowsRegistryZones source
  pure (map (sort . map registryZoneId) loaded)

||| Build a provider around a native Windows registry reader.
public export
windowsRegistryTimeZoneProvider : WindowsRegistrySource -> TimeZoneProvider
windowsRegistryTimeZoneProvider source = MkTimeZoneProvider
  (pure (Right (fixedDateTimeZone "UTC" zeroOffset)))
  (windowsRegistryNamedZone source)
  (windowsRegistryLocalZone source)
  (windowsRegistryAvailableZones source)

unsupportedWindowsProvider : TimeZoneProvider
unsupportedWindowsProvider = MkTimeZoneProvider unsupported
  (\_ => unsupported) unsupported unsupported
  where
    unsupported : IO (Either TzdbError value)
    unsupported = pure (Left (UnsupportedPlatform
      "Windows TZDB discovery requires a registry/ICU TimeZoneProvider"))

||| The provider selected for the current operating system.
public export
systemTimeZoneProvider : TimeZoneProvider
systemTimeZoneProvider = if isWindows
  then unsupportedWindowsProvider
  else unixTimeZoneProvider

||| Load UTC through an explicit platform provider.
public export
utcWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
utcWith = providerUtc

||| Load a named zone through an explicit platform provider.
public export
timeZoneWith : TimeZoneProvider -> String -> IO (Either TzdbError TimeZone)
timeZoneWith = providerTimeZone

||| Load the local zone through an explicit platform provider.
public export
localZoneWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
localZoneWith = providerLocalZone

||| Enumerate zones through an explicit platform provider.
public export
availableZonesWith : TimeZoneProvider -> IO (Either TzdbError (List String))
availableZonesWith = providerAvailableZones

||| Load UTC from the platform TZDB.
public export
utc : IO (Either TzdbError TimeZone)
utc = utcWith systemTimeZoneProvider

||| Load a named zone from the platform TZDB.
public export
timeZone : String -> IO (Either TzdbError TimeZone)
timeZone = timeZoneWith systemTimeZoneProvider

||| Load the locally configured platform zone.
public export
localZone : IO (Either TzdbError TimeZone)
localZone = localZoneWith systemTimeZoneProvider

||| List every zone available through the platform provider.
public export
availableZones : IO (Either TzdbError (List String))
availableZones = availableZonesWith systemTimeZoneProvider