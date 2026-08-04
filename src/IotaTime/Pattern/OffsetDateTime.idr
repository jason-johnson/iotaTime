module IotaTime.Pattern.OffsetDateTime

import IotaTime.OffsetDateTime
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime
import IotaTime.Pattern.Offset

%default total

||| Combine independently typed local date-time and UTC-offset patterns.
public export
offsetDateTimePattern :
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern localState (CalendarDateTime calendar) ->
  Pattern offsetState Offset ->
  Pattern (localState, offsetState) (OffsetDateTime calendar)
offsetDateTimePattern = pairPattern localDateTime offsetOf atOffset

||| ISO local date-time followed by an ISO offset, for example
||| `2024-04-23T09:00:00+02:00`.
public export
pOffsetDateTime : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Pattern ((DateFields, TimeFields), Offset) (OffsetDateTime calendar)
pOffsetDateTime = offsetDateTimePattern ps pOffset