module CopticCommonYearEpagomenalDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate Coptic
invalid = IotaTime.Calendar.Coptic.calendarDate 6 CopticMonths.PiKogiEnavot 1732
