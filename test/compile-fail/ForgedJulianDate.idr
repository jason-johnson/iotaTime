module ForgedJulianDate
-- EXPECT: MkJulianDate is private.

import IotaTime

invalid : CalendarDate Julian
invalid = MkJulianDate (-746632)