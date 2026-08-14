module IotaTime.TimeZone.Error

import IotaTime.TimeZone.Core
import IotaTime.Tzdb.Posix
import IotaTime.Tzdb.Tzif
import IotaTime.Tzdb.Windows.Types

%default total

public export
data TzdbError
  = TzdbFileError String
  | TzdbParseError TzifError
  | TzdbPosixError PosixTzError
  | TzdbZoneError TimeZoneError
  | TzdbWindowsError WindowsRegistryError
  | WindowsRegistrySourceError String
  | WindowsZoneNotFound String
  | InvalidZoneName String
  | UnsupportedPlatform String
