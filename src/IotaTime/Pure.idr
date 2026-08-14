||| Native-free iotaTime surface for applications that supply their own
||| time-zone provider and do not require operating-system locale acquisition.
module IotaTime.Pure

import public IotaTime.Duration
import public IotaTime.Instant
import public IotaTime.Interval
import public IotaTime.Offset
import public IotaTime.Period
import public IotaTime.Calendar
import public IotaTime.Calendar.Gregorian
import public IotaTime.Calendar.Iso
import public IotaTime.Calendar.Coptic
import public IotaTime.Calendar.Islamic
import public IotaTime.Calendar.Persian
import public IotaTime.Calendar.Julian
import public IotaTime.Calendar.Hebrew
import public IotaTime.LocalTime
import public IotaTime.CalendarDateTime
import public IotaTime.OffsetDateTime
import public IotaTime.TimeZone.Core
import public IotaTime.TimeZone.Error
import public IotaTime.ZonedDateTime
import public IotaTime.Clock
import public IotaTime.Tzdb.Metadata
import IotaTime.Tzdb.Provider
import public IotaTime.Pattern
import public IotaTime.Pattern.Scalar
import public IotaTime.Pattern.Calendar
import public IotaTime.Pattern.Duration
import public IotaTime.Pattern.Offset

export
record TimeZoneProviderRep where
	constructor MkTimeZoneProviderRep
	providerRepresentation : IotaTime.Tzdb.Provider.TimeZoneProviderRep

||| An opaque caller-supplied source of time zones and associated metadata.
public export
TimeZoneProvider : Type
TimeZoneProvider = IotaTime.Pure.TimeZoneProviderRep

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

public export
TimeZoneCachePolicy : Type
TimeZoneCachePolicy = IotaTime.Pure.TimeZoneCachePolicyRep

public export
timeZoneCachePolicy : (cacheNamedZones : Bool) ->
											(cacheAvailableZones : Bool) ->
											(cacheMetadata : Bool) ->
											(cacheLocalZone : Bool) ->
											TimeZoneCachePolicy
timeZoneCachePolicy named available valueMetadata local =
	MkTimeZoneCachePolicyRep $ IotaTime.Tzdb.Provider.timeZoneCachePolicy
		named available valueMetadata local

public export
defaultTimeZoneCachePolicy : TimeZoneCachePolicy
defaultTimeZoneCachePolicy =
	MkTimeZoneCachePolicyRep IotaTime.Tzdb.Provider.defaultTimeZoneCachePolicy

public export
cachedTimeZoneProvider : TimeZoneCachePolicy -> TimeZoneProvider ->
												 IO TimeZoneProvider
cachedTimeZoneProvider (MkTimeZoneCachePolicyRep policy)
								 (MkTimeZoneProviderRep provider) =
	MkTimeZoneProviderRep <$>
		IotaTime.Tzdb.Provider.cachedTimeZoneProvider policy provider

public export
utcWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
utcWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.Provider.runProviderUtc provider

public export
timeZoneWith : TimeZoneProvider -> String -> IO (Either TzdbError TimeZone)
timeZoneWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.Provider.runProviderTimeZone provider

public export
localZoneWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
localZoneWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.Provider.runProviderLocalZone provider

public export
availableZonesWith : TimeZoneProvider ->
										 IO (Either TzdbError (List String))
availableZonesWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.Provider.runProviderAvailableZones provider

public export
metadataWith : TimeZoneProvider -> IO (Either TzdbError TzdbMetadata)
metadataWith (MkTimeZoneProviderRep provider) =
	IotaTime.Tzdb.Provider.runProviderMetadata provider
