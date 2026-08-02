module InvalidJulianDayCount
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = julianFromDays (-746632)