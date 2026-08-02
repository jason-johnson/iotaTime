module IotaTime

import public IotaTime.Period
import public IotaTime.LocalTime
import public IotaTime.Calendar
import public IotaTime.Calendar.Gregorian
import public IotaTime.Calendar.Julian
import public IotaTime.Calendar.Hebrew
import public IotaTime.CalendarDateTime

public export
record Instant where
  constructor MkInstant
  ticks : Integer

export
epoch : Instant
epoch = MkInstant 0
