module IotaTime.Pattern.OffsetDateTime

import IotaTime.Calendar.Gregorian
import IotaTime.OffsetDateTime
import IotaTime.Pattern
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime
import IotaTime.Pattern.Offset

%default total

public export
offsetDateTimePattern :
  Pattern localState (CalendarDateTime Gregorian) ->
  Pattern offsetState Offset ->
  Pattern (localState, offsetState) (OffsetDateTime Gregorian)
offsetDateTimePattern = pairPattern localDateTime offsetOf atOffset

public export
pOffsetDateTime :
  Pattern ((DateFields, TimeFields), Offset) (OffsetDateTime Gregorian)
pOffsetDateTime = offsetDateTimePattern ps pOffset