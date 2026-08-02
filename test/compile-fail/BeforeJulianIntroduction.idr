module BeforeJulianIntroduction
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Julian
invalid = julianDate 31 JulianMonths.December (-45)