module CopticYearZero
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Coptic
invalid = copticDate 1 CopticMonths.Thout 0
