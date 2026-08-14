-- EXPECT: Name IotaTime.Pattern.LocalTime.TimeFieldsRep.(.parsedHour) is private.

module ExposedTimeFieldsField

import IotaTime

invalid : TimeFields -> Integer
invalid fields = fields.parsedHour
