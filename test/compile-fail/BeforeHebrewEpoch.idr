module BeforeHebrewEpoch
-- EXPECT: Can't find an implementation for So (

import IotaTime

invalid : CalendarDate HebrewCivil
invalid = IotaTime.Calendar.Hebrew.fromDays (-2103608)
