module IotaTime.TimeZone

import public IotaTime.TimeZone.Core
import public IotaTime.TimeZone.Error
import public IotaTime.Tzdb.Metadata
import IotaTime.Tzdb.Provider
import IotaTime.Tzdb

%default total

||| An opaque source of time zones and associated metadata.
public export
TimeZoneProvider : Type
TimeZoneProvider = IotaTime.Tzdb.Provider.TimeZoneProviderRep

||| Build a provider from its loading operations.
public export
timeZoneProvider : IO (Either TzdbError TimeZone) ->
									 (String -> IO (Either TzdbError TimeZone)) ->
									 IO (Either TzdbError TimeZone) ->
									 IO (Either TzdbError (List String)) ->
									 IO (Either TzdbError TzdbMetadata) ->
									 TimeZoneProvider
timeZoneProvider = IotaTime.Tzdb.Provider.timeZoneProvider

||| An opaque selection of provider results to cache.
public export
TimeZoneCachePolicy : Type
TimeZoneCachePolicy = IotaTime.Tzdb.Provider.TimeZoneCachePolicyRep

||| Select which successful provider queries are retained in memory.
public export
timeZoneCachePolicy : (cacheNamedZones : Bool) ->
											(cacheAvailableZones : Bool) ->
											(cacheMetadata : Bool) ->
											(cacheLocalZone : Bool) ->
											TimeZoneCachePolicy
timeZoneCachePolicy = IotaTime.Tzdb.Provider.timeZoneCachePolicy

||| Cache named zones, discovery, and metadata while keeping the local zone live.
public export
defaultTimeZoneCachePolicy : TimeZoneCachePolicy
defaultTimeZoneCachePolicy =
	IotaTime.Tzdb.Provider.defaultTimeZoneCachePolicy

||| Wrap a provider in caller-owned caches for selected successful operations.
public export
cachedTimeZoneProvider : TimeZoneCachePolicy -> TimeZoneProvider ->
												 IO TimeZoneProvider
cachedTimeZoneProvider = IotaTime.Tzdb.Provider.cachedTimeZoneProvider

||| The built-in Unix filesystem provider.
public export
unixTimeZoneProvider : TimeZoneProvider
unixTimeZoneProvider = IotaTime.Tzdb.unixTimeZoneProvider

||| The provider selected for the current operating system.
public export
systemTimeZoneProvider : TimeZoneProvider
systemTimeZoneProvider = IotaTime.Tzdb.systemTimeZoneProvider

||| Snapshot the native Windows registry into an immutable provider.
public export
windowsSnapshotTimeZoneProvider : IO (Either TzdbError TimeZoneProvider)
windowsSnapshotTimeZoneProvider =
	IotaTime.Tzdb.windowsSnapshotTimeZoneProvider

||| Load UTC through an explicit provider.
public export
utcWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
utcWith = IotaTime.Tzdb.utcWith

||| Load a named zone through an explicit provider.
public export
timeZoneWith : TimeZoneProvider -> String -> IO (Either TzdbError TimeZone)
timeZoneWith = IotaTime.Tzdb.timeZoneWith

||| Load the local zone through an explicit provider.
public export
localZoneWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
localZoneWith = IotaTime.Tzdb.localZoneWith

||| Enumerate zones through an explicit provider.
public export
availableZonesWith : TimeZoneProvider ->
										 IO (Either TzdbError (List String))
availableZonesWith = IotaTime.Tzdb.availableZonesWith

||| Query version and identifier metadata through an explicit provider.
public export
metadataWith : TimeZoneProvider -> IO (Either TzdbError TzdbMetadata)
metadataWith = IotaTime.Tzdb.metadataWith

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

||| Query version, aliases, and platform mappings from the system provider.
public export
metadata : IO (Either TzdbError TzdbMetadata)
metadata = IotaTime.TimeZone.metadataWith
	IotaTime.TimeZone.systemTimeZoneProvider
