module IotaTime.Tzdb.Provider

import IotaTime.TimeZone.Core
import IotaTime.TimeZone.Error
import IotaTime.Tzdb.Metadata
import Data.IORef
import System.Concurrency

%default total

||| Platform-specific time-zone discovery behind one shared contract.
export
record TimeZoneProviderRep where
  constructor MkTimeZoneProvider
  providerUtc : IO (Either TzdbError TimeZone)
  providerTimeZone : String -> IO (Either TzdbError TimeZone)
  providerLocalZone : IO (Either TzdbError TimeZone)
  providerAvailableZones : IO (Either TzdbError (List String))
  providerMetadata : IO (Either TzdbError TzdbMetadata)

export
timeZoneProvider : IO (Either TzdbError TimeZone) ->
                   (String -> IO (Either TzdbError TimeZone)) ->
                   IO (Either TzdbError TimeZone) ->
                   IO (Either TzdbError (List String)) ->
                   IO (Either TzdbError TzdbMetadata) ->
                   TimeZoneProviderRep
timeZoneProvider = MkTimeZoneProvider

export
runProviderUtc : TimeZoneProviderRep -> IO (Either TzdbError TimeZone)
runProviderUtc (MkTimeZoneProvider action _ _ _ _) = action

export
runProviderTimeZone : TimeZoneProviderRep -> String ->
                      IO (Either TzdbError TimeZone)
runProviderTimeZone (MkTimeZoneProvider _ action _ _ _) = action

export
runProviderLocalZone : TimeZoneProviderRep -> IO (Either TzdbError TimeZone)
runProviderLocalZone (MkTimeZoneProvider _ _ action _ _) = action

export
runProviderAvailableZones : TimeZoneProviderRep ->
                            IO (Either TzdbError (List String))
runProviderAvailableZones (MkTimeZoneProvider _ _ _ action _) = action

export
runProviderMetadata : TimeZoneProviderRep -> IO (Either TzdbError TzdbMetadata)
runProviderMetadata (MkTimeZoneProvider _ _ _ _ action) = action

||| Selects which successful provider queries are retained in memory.
||| Failures are always retried. Local-zone caching is independent because the
||| host's local-zone configuration may change while a process is running.
export
record TimeZoneCachePolicyRep where
  constructor MkTimeZoneCachePolicy
  cacheNamedZones : Bool
  cacheAvailableZones : Bool
  cacheMetadata : Bool
  cacheLocalZone : Bool

export
timeZoneCachePolicy : (cacheNamedZones : Bool) ->
                      (cacheAvailableZones : Bool) ->
                      (cacheMetadata : Bool) ->
                      (cacheLocalZone : Bool) ->
                      TimeZoneCachePolicyRep
timeZoneCachePolicy = MkTimeZoneCachePolicy

||| Cache named zones, discovery, and metadata while continuing to observe
||| changes to the host's local-zone configuration.
export
defaultTimeZoneCachePolicy : TimeZoneCachePolicyRep
defaultTimeZoneCachePolicy = MkTimeZoneCachePolicy True True True False

findNamedZone : String -> List (String, TimeZone) -> Maybe TimeZone
findNamedZone name [] = Nothing
findNamedZone name ((cachedName, zone) :: rest) =
  if name == cachedName then Just zone else findNamedZone name rest

withMutex : Mutex -> IO value -> IO value
withMutex mutex action = do
  mutexAcquire mutex
  result <- action
  mutexRelease mutex
  pure result

cachedSuccessful : Bool -> Mutex -> IORef (Maybe value) ->
                   IO (Either error value) -> IO (Either error value)
cachedSuccessful False mutex reference load = load
cachedSuccessful True mutex reference load = withMutex mutex $ do
  cached <- readIORef reference
  case cached of
    Just value => pure (Right value)
    Nothing => do
      loaded <- load
      case loaded of
        Left error => pure (Left error)
        Right value => do
          writeIORef reference (Just value)
          pure (Right value)

cachedNamedZone : Bool -> Mutex -> IORef (List (String, TimeZone)) ->
                  (String -> IO (Either TzdbError TimeZone)) -> String ->
                  IO (Either TzdbError TimeZone)
cachedNamedZone False mutex reference load name = load name
cachedNamedZone True mutex reference load name = withMutex mutex $ do
  cached <- readIORef reference
  case findNamedZone name cached of
    Just zone => pure (Right zone)
    Nothing => do
      loaded <- load name
      case loaded of
        Left error => pure (Left error)
        Right zone => do
          writeIORef reference ((name, zone) :: cached)
          pure (Right zone)

||| Wrap a provider in caller-owned, opt-in successful-result caches.
||| Construct a new wrapper to refresh all cached values.
export
cachedTimeZoneProvider : TimeZoneCachePolicyRep -> TimeZoneProviderRep ->
                         IO TimeZoneProviderRep
cachedTimeZoneProvider policy provider = do
  namedLock <- makeMutex
  availableLock <- makeMutex
  metadataLock <- makeMutex
  localLock <- makeMutex
  namedCache <- newIORef []
  availableCache <- newIORef Nothing
  metadataCache <- newIORef Nothing
  localCache <- newIORef Nothing
  pure $ MkTimeZoneProvider
    provider.providerUtc
    (cachedNamedZone policy.cacheNamedZones namedLock namedCache
      provider.providerTimeZone)
    (cachedSuccessful policy.cacheLocalZone localLock localCache
      provider.providerLocalZone)
    (cachedSuccessful policy.cacheAvailableZones availableLock availableCache
      provider.providerAvailableZones)
    (cachedSuccessful policy.cacheMetadata metadataLock metadataCache
      provider.providerMetadata)
