module IotaTime.Tzdb.Provider

import public IotaTime.DateTimeZone
import public IotaTime.Tzdb.Metadata
import IotaTime.Tzdb.Posix
import IotaTime.Tzdb.Tzif
import IotaTime.Tzdb.Windows.Types
import Data.IORef
import System.Concurrency

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
  providerMetadata : IO (Either TzdbError TzdbMetadata)

||| Selects which successful provider queries are retained in memory.
||| Failures are always retried. Local-zone caching is independent because the
||| host's local-zone configuration may change while a process is running.
public export
record TimeZoneCachePolicy where
  constructor MkTimeZoneCachePolicy
  cacheNamedZones : Bool
  cacheAvailableZones : Bool
  cacheMetadata : Bool
  cacheLocalZone : Bool

||| Cache named zones, discovery, and metadata while continuing to observe
||| changes to the host's local-zone configuration.
public export
defaultTimeZoneCachePolicy : TimeZoneCachePolicy
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
public export
cachedTimeZoneProvider : TimeZoneCachePolicy -> TimeZoneProvider ->
                         IO TimeZoneProvider
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
