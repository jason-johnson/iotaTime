module ForgedCopticDate
-- EXPECT: MkCopticDate is private.

import IotaTime

invalid : CalendarDate Coptic
invalid = MkCopticDate (-626576)
