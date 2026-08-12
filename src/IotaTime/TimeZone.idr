module IotaTime.TimeZone

import public IotaTime.TimeZone.Core
import public IotaTime.Tzdb.Provider
import IotaTime.Tzdb

%default total

||| Load UTC from the platform time-zone database.
public export
utc : IO (Either TzdbError TimeZone)
utc = IotaTime.Tzdb.loadSystemUtc

||| Load a named zone from the platform time-zone database.
public export
timeZone : String -> IO (Either TzdbError TimeZone)
timeZone = IotaTime.Tzdb.loadSystemTimeZone

||| Load the locally configured platform zone.
public export
localZone : IO (Either TzdbError TimeZone)
localZone = IotaTime.Tzdb.loadSystemLocalZone

||| List every zone available through the platform provider.
public export
availableZones : IO (Either TzdbError (List String))
availableZones = IotaTime.Tzdb.loadSystemAvailableZones
