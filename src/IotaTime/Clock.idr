module IotaTime.Clock

import IotaTime.Instant
import IotaTime.ZonedDateTime

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

||| A clock paired with a time zone and calendar representation.
export
record ZonedClock (calendar : Type) (clock : Type) where
  constructor MkZonedClock
  underlyingClock : clock
  clockZone : TimeZone

||| Pair any clock with the zone used to display its current instant.
public export
zonedClock : {calendar : Type} -> clock -> TimeZone -> ZonedClock calendar clock
zonedClock = MkZonedClock

||| Read a clock and display its current instant in the configured zone.
public export
getCurrentZonedDateTime : {calendar : Type} -> {clock : Type} ->
                          {auto cal : Calendar calendar} ->
                          {auto rep : HasCalendarDate
                            (CalendarDate calendar @{cal})} ->
                          Clock clock => ZonedClock calendar clock ->
                          IO (Either CalendarConversionError
                            (ZonedDateTime calendar @{cal}))
getCurrentZonedDateTime value = do
  instant <- getCurrentInstant value.underlyingClock
  pure (IotaTime.ZonedDateTime.fromInstant instant value.clockZone)
