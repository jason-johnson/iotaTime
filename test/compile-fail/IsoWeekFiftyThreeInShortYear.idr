module IsoWeekFiftyThreeInShortYear
-- EXPECT: Can't find an implementation for So (

import IotaTime
import IotaTime.Calendar.Iso

invalid : CalendarDate Gregorian
invalid = IotaTime.Calendar.Iso.fromWeekDate 53 Monday 2021