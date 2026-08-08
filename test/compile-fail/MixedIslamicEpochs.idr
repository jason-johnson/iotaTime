module MixedIslamicEpochs
-- EXPECT: Mismatch between: Astronomical and Civil

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate CivilIslamicBcl
invalid = IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1443