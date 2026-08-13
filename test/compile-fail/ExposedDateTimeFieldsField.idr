-- EXPECT: Name IotaTime.Pattern.Locale.DateTimeFieldsRep.(.parsedDateFields) is private.

module ExposedDateTimeFieldsField

import IotaTime

invalid : DateTimeFields -> DateFields
invalid fields = fields.parsedDateFields
