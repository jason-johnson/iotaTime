module InvalidHebrewKislevDay
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = hebrewDate 30 5781 HebrewMonths.Kislev
