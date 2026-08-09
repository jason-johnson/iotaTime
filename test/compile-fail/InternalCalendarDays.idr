module InternalCalendarDays

import IotaTime

invalid : Integer
invalid = calendarDays
  (IotaTime.Calendar.Gregorian.calendarDate 1 January 2024)
