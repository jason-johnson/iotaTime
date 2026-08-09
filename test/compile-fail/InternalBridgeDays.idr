module InternalBridgeDays

import IotaTime

invalid : Integer
invalid = toBridgeDays
  (IotaTime.Calendar.Gregorian.calendarDate 1 January 2024)
