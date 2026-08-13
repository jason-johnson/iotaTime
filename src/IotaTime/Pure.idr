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

||| An opaque caller-supplied source of time zones and associated metadata.
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

public export
TimeZoneCachePolicy : Type
TimeZoneCachePolicy = IotaTime.Tzdb.Provider.TimeZoneCachePolicyRep

public export
timeZoneCachePolicy : (cacheNamedZones : Bool) ->
											(cacheAvailableZones : Bool) ->
											(cacheMetadata : Bool) ->
											(cacheLocalZone : Bool) ->
											TimeZoneCachePolicy
timeZoneCachePolicy = IotaTime.Tzdb.Provider.timeZoneCachePolicy

public export
defaultTimeZoneCachePolicy : TimeZoneCachePolicy
defaultTimeZoneCachePolicy =
	IotaTime.Tzdb.Provider.defaultTimeZoneCachePolicy

public export
cachedTimeZoneProvider : TimeZoneCachePolicy -> TimeZoneProvider ->
												 IO TimeZoneProvider
cachedTimeZoneProvider = IotaTime.Tzdb.Provider.cachedTimeZoneProvider

public export
utcWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
utcWith = IotaTime.Tzdb.Provider.runProviderUtc

public export
timeZoneWith : TimeZoneProvider -> String -> IO (Either TzdbError TimeZone)
timeZoneWith = IotaTime.Tzdb.Provider.runProviderTimeZone

public export
localZoneWith : TimeZoneProvider -> IO (Either TzdbError TimeZone)
localZoneWith = IotaTime.Tzdb.Provider.runProviderLocalZone

public export
availableZonesWith : TimeZoneProvider ->
										 IO (Either TzdbError (List String))
availableZonesWith = IotaTime.Tzdb.Provider.runProviderAvailableZones

public export
metadataWith : TimeZoneProvider -> IO (Either TzdbError TzdbMetadata)
metadataWith = IotaTime.Tzdb.Provider.runProviderMetadata
