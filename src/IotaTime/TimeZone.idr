module IotaTime.TimeZone

import public IotaTime.TimeZone.Core
import public IotaTime.TimeZone.Error
import public IotaTime.Tzdb.Metadata
import IotaTime.Tzdb.Provider
import IotaTime.Tzdb

%default total

export
record TimeZoneProviderRep where
	constructor MkTimeZoneProviderRep
	providerRepresentation : IotaTime.Tzdb.Provider.TimeZoneProviderRep

||| An opaque source of time zones and associated metadata.
public export
TimeZoneProvider : Type
TimeZoneProvider = IotaTime.TimeZone.TimeZoneProviderRep

||| Build a provider from its loading operations.
public export
timeZoneProvider : IO (Either TzdbError TimeZone) ->
									 (String -> IO (Either TzdbError TimeZone)) ->
									 IO (Either TzdbError TimeZone) ->
									 IO (Either TzdbError (List String)) ->
									 IO (Either TzdbError TzdbMetadata) ->
									 TimeZoneProvider
timeZoneProvider utcAction zoneAction localAction availableAction metadataAction =
	MkTimeZoneProviderRep $ IotaTime.Tzdb.Provider.timeZoneProvider
		utcAction zoneAction localAction availableAction metadataAction

export
record TimeZoneCachePolicyRep where
	constructor MkTimeZoneCachePolicyRep
	cachePolicyRepresentation : IotaTime.Tzdb.Provider.TimeZoneCachePolicyRep

||| An opaque selection of provider results to cache.
public export
TimeZoneCachePolicy : Type
TimeZoneCachePolicy = IotaTime.TimeZone.TimeZoneCachePolicyRep

||| Select which successful provider queries are retained in memory.
public export
timeZoneCachePolicy : (cacheNamedZones : Bool) ->
											(cacheAvailableZones : Bool) ->
											(cacheMetadata : Bool) ->
											(cacheLocalZone : Bool) ->
											TimeZoneCachePolicy
timeZoneCachePolicy named available valueMetadata local =
	MkTimeZoneCachePolicyRep $ IotaTime.Tzdb.Provider.timeZoneCachePolicy
		named available valueMetadata local

||| Cache named zones, discovery, and metadata while keeping the local zone live.
public export
defaultTimeZoneCachePolicy : TimeZoneCachePolicy
defaultTimeZoneCachePolicy =
	MkTimeZoneCachePolicyRep IotaTime.Tzdb.Provider.defaultTimeZoneCachePolicy

||| Wrap a provider in caller-owned caches for selected successful operations.
public export
cachedTimeZoneProvider : TimeZoneCachePolicy -> TimeZoneProvider ->
												 IO TimeZoneProvider
cachedTimeZoneProvider (MkTimeZoneCachePolicyRep policy)
								 (MkTimeZoneProviderRep provider) =
	MkTimeZoneProviderRep <$>
		IotaTime.Tzdb.Provider.cachedTimeZoneProvider policy provider

||| The built-in Unix filesystem provider.
public export
unixTimeZoneProvider : TimeZoneProvider
unixTimeZoneProvider = MkTimeZoneProviderRep IotaTime.Tzdb.unixTimeZoneProvider

||| The provider selected for the current operating system.
public export
systemTimeZoneProvider : TimeZoneProvider
systemTimeZoneProvider =
	MkTimeZoneProviderRep IotaTime.Tzdb.systemTimeZoneProvider

||| Snapshot the native Windows registry into an immutable provider.
public export
windowsSnapshotTimeZoneProvider : IO (Either TzdbError TimeZoneProvider)
windowsSnapshotTimeZoneProvider = do
	provider <- IotaTime.Tzdb.windowsSnapshotTimeZoneProvider
	pure (MkTimeZoneProviderRep <$> provider)

||| Load UTC through an explicit provider.
public export
utcWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
utcWith (MkTimeZoneProviderRep provider) = IotaTime.Tzdb.utcWith provider

||| Load a named zone through an explicit provider.
public export
timeZoneWith : TimeZoneProvider -> String -> IO (Either TzdbError TimeZone)
timeZoneWith (MkTimeZoneProviderRep provider) = IotaTime.Tzdb.timeZoneWith provider

||| Load the local zone through an explicit provider.
public export
localZoneWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
localZoneWith (MkTimeZoneProviderRep provider) = IotaTime.Tzdb.localZoneWith provider

||| Enumerate zones through an explicit provider.
public export
availableZonesWith : TimeZoneProvider ->
										 IO (Either TzdbError (List String))
availableZonesWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.availableZonesWith provider

||| Query version and identifier metadata through an explicit provider.
public export
metadataWith : TimeZoneProvider -> IO (Either TzdbError TzdbMetadata)
metadataWith (MkTimeZoneProviderRep provider) = IotaTime.Tzdb.metadataWith provider

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
