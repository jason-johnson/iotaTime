module ForgedPersianDate
-- EXPECT: MkPersianDate is private.

import IotaTime
import IotaTime.Calendar.Persian

invalid : CalendarDate Persian
invalid = MkPersianDate (-503285)
