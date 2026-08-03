module ForgedIslamicDate
-- EXPECT: MkIslamicDate is private.

import IotaTime
import IotaTime.Calendar.Islamic

invalid : CalendarDate IslamicBcl
invalid = MkIslamicDate (-503167)
