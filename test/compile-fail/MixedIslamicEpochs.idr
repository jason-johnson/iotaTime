module MixedIslamicEpochs
-- EXPECT: Mismatch between: Astronomical and Civil

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate CivilIslamicBcl
invalid = islamicDate 1 IslamicMonths.Muharram 1443