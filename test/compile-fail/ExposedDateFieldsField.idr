-- EXPECT: Name IotaTime.Pattern.CalendarDate.DateFieldsRep.(.parsedYear) is private.

module ExposedDateFieldsField

import IotaTime

invalid : DateFields -> Integer
invalid fields = fields.parsedYear
