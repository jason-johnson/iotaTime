module InvalidJulianLeapDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = julianDate 29 JulianMonths.February 1901