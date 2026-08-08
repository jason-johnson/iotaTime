module MixedPeriodForDate
-- EXPECT: Can't find an implementation for HasTime GregorianDate.

import IotaTime

invalid : CalendarDate Gregorian
invalid = applyPeriod
  (months 2 <+> minutes 20)
  (IotaTime.Calendar.Gregorian.calendarDate 1 January 2000)