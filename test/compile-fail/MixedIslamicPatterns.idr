module MixedIslamicPatterns
-- EXPECT: Mismatch between: Base16 and Base15

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate IslamicBase15
invalid = IotaTime.Calendar.Islamic.calendarDate 1 IslamicMonths.Muharram 1443
