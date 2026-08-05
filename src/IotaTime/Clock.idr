module IotaTime.Clock

import IotaTime.Instant

%default total

||| A source of current instants. Application code can quantify over this
||| capability to replace the system clock in deterministic tests.
public export
interface Clock clock where
  getCurrentInstant : clock -> IO Instant

||| The host operating system's UTC clock.
public export
data SystemClock = MkSystemClock

public export
Clock SystemClock where
  getCurrentInstant MkSystemClock = now

||| The system clock value used by production applications.
public export
systemClock : SystemClock
systemClock = MkSystemClock

||| A clock that always returns one instant.
public export
data FixedClock = MkFixedClock Instant

public export
Clock FixedClock where
  getCurrentInstant (MkFixedClock value) = pure value

||| Construct a deterministic clock fixed at the supplied instant.
public export
fixedClock : Instant -> FixedClock
fixedClock = MkFixedClock
