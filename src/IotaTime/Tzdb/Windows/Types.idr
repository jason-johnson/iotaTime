module IotaTime.Tzdb.Windows.Types

import public IotaTime.DateTimeZone

%default total

||| Recurring SYSTEMTIME fields used by Windows TZI registry values.
public export
record WindowsTransitionDate where
  constructor MkWindowsTransitionDate
  year : Integer
  month : Integer
  week : Integer
  weekday : Integer
  hour : Integer
  minute : Integer
  second : Integer

||| The recurring portion of a Windows REG_TZI_FORMAT value. Bias fields are
||| minutes added to local time to obtain UTC, following Windows conventions.
public export
record WindowsZoneRule where
  constructor MkWindowsZoneRule
  biasMinutes : Integer
  standardBiasMinutes : Integer
  daylightBiasMinutes : Integer
  standardName : String
  daylightName : String
  daylightStart : WindowsTransitionDate
  standardStart : WindowsTransitionDate

||| A Dynamic DST registry value effective from January 1 of its year.
public export
record WindowsDynamicRule where
  constructor MkWindowsDynamicRule
  effectiveYear : Integer
  dynamicRule : WindowsZoneRule

||| Platform-neutral data read from one Windows time-zone registry key.
public export
record WindowsRegistryZone where
  constructor MkWindowsRegistryZone
  registryZoneId : String
  registryStandardName : String
  registryDaylightName : String
  registryDefaultTzi : List Bits8
  registryDynamicTzi : List (Integer, List Bits8)

||| Complete result emitted by a native Windows registry source.
public export
record WindowsRegistrySnapshot where
  constructor MkWindowsRegistrySnapshot
  snapshotLocalZoneId : String
  snapshotZones : List WindowsRegistryZone

public export
data WindowsZoneError
  = WindowsOffsetOutOfRange Integer
  | WindowsTimeOutOfRange Integer Integer Integer
  | WindowsTransitionMillisecondsUnsupported Integer
  | WindowsAbsoluteTransitionUnsupported Integer
  | IncompleteWindowsDaylightRule
  | WindowsTziLength Integer
  | WindowsRecurrenceError RecurrenceRuleError

public export
data WindowsTimeZoneError
  = InvalidWindowsRule WindowsZoneError
  | InvalidWindowsTransitions DateTimeZoneError
  | DynamicYearsNotStrictlyIncreasing

public export
data WindowsRegistryError
  = InvalidDefaultTzi WindowsZoneError
  | InvalidDynamicTzi Integer WindowsZoneError
  | InvalidRegistryTimeZone WindowsTimeZoneError

public export
data WindowsRegistryProtocolError
  = MissingLocalZoneId
  | UnexpectedRegistryLine String
  | InvalidRegistryHex String
  | InvalidDynamicRegistryLine String
  | IncompleteRegistryZone
