module MixedPeriodForLocalTime
-- EXPECT: Can't find an implementation for HasCalendar LocalTime.

import IotaTime

invalid : LocalTime
invalid = applyPeriod (months 2 <+> minutes 20) (localTime 12 0 0 0)