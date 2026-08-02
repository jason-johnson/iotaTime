module ForgedHebrewDate
-- EXPECT: MkHebrewDate is private.

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = MkHebrewDate (-2103608) 1 HebrewMonths.Tishri 1
