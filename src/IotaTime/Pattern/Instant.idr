module IotaTime.Pattern.Instant

import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.Instant
import IotaTime.Offset
import IotaTime.OffsetDateTime
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime

%default total

||| A Gregorian date-time pattern adapted to parse and format UTC instants.
export
record InstantPattern state where
  constructor MkInstantPattern
  calendarPattern : Pattern state (CalendarDateTime Gregorian)

calendarDateTimeToInstant : CalendarDateTime Gregorian -> Instant
calendarDateTimeToInstant value = IotaTime.OffsetDateTime.toInstant
  (fromCalendarDateTimeWithOffset value zeroOffset)

||| Build an Instant pattern from a Gregorian CalendarDateTime pattern.
public export
instantPattern : Pattern state (CalendarDateTime Gregorian) ->
                 InstantPattern state
instantPattern = MkInstantPattern

||| Parse an Instant, treating the underlying Gregorian date-time as UTC.
public export
parseInstant : InstantPattern state -> String -> Either PatternError Instant
parseInstant pattern source =
  map calendarDateTimeToInstant
    (IotaTime.Pattern.parse pattern.calendarPattern source)

||| Format an Instant in UTC. Instants outside the Gregorian calendar's
||| supported range return the existing typed calendar conversion error.
public export
formatInstant : InstantPattern state -> Instant ->
                Either CalendarConversionError String
formatInstant pattern value = do
  offsetValue <- IotaTime.OffsetDateTime.fromInstant
    {calendar = Gregorian} zeroOffset value
  Right (IotaTime.Pattern.format pattern.calendarPattern
    (toCalendarDateTime offsetValue))

||| ISO-8601 UTC at whole-second precision: yyyy-MM-ddTHH:mm:ssZ.
public export
pInstant : InstantPattern (DateFields, TimeFields)
pInstant = instantPattern (ps {calendar = Gregorian} <% char 'Z')

||| ISO-8601 UTC with nine fractional digits:
||| yyyy-MM-ddTHH:mm:ss.fffffffffZ.
public export
pInstantNano : InstantPattern (DateFields, TimeFields)
pInstantNano = instantPattern (po {calendar = Gregorian} <% char 'Z')