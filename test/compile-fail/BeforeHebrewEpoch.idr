module BeforeHebrewEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = hebrewFromDays (-2103608)
