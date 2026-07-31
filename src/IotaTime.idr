module IotaTime

import public IotaTime.Calendar
import public IotaTime.Calendar.Gregorian

public export
record Instant where
  constructor MkInstant
  ticks : Integer

export
epoch : Instant
epoch = MkInstant 0
