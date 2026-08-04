module IotaTime.Tzdb.Provider

import public IotaTime.DateTimeZone
import IotaTime.Tzdb.Posix
import IotaTime.Tzdb.Tzif
import IotaTime.Tzdb.Windows.Types

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

||| Platform-specific time-zone discovery behind one shared contract.
public export
record TimeZoneProvider where
  constructor MkTimeZoneProvider
  providerUtc : IO (Either TzdbError TimeZone)
  providerTimeZone : String -> IO (Either TzdbError TimeZone)
  providerLocalZone : IO (Either TzdbError TimeZone)
  providerAvailableZones : IO (Either TzdbError (List String))
