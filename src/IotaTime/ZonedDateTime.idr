module IotaTime.ZonedDateTime

import public IotaTime.DateTimeZone
import public IotaTime.Duration
import public IotaTime.OffsetDateTime
import public IotaTime.Period

%default total

export
record ZonedDateTimeRep (calendar : Type) (cal : Calendar calendar) where
  constructor MkZonedDateTime
  zonedValue : OffsetDateTime calendar @{cal}
  zonedZone : DateTimeZone

public export
ZonedDateTime : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
ZonedDateTime calendar @{cal} = ZonedDateTimeRep calendar cal

||| Display an instant in a zone using the zone's effective offset. Conversion
||| fails only when the resulting local day is outside the calendar's range.
public export
inZone : {calendar : Type} -> {auto cal : Calendar calendar} ->
         {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
         DateTimeZone -> Instant ->
         Either CalendarConversionError (ZonedDateTime calendar @{cal})
inZone valueZone valueInstant =
  case IotaTime.OffsetDateTime.fromInstant
    (zoneOffsetAt valueZone valueInstant) valueInstant of
      Left error => Left error
      Right value => Right (MkZonedDateTime value valueZone)

public export
zonedOffsetDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                      ZonedDateTime calendar @{cal} ->
                      OffsetDateTime calendar @{cal}
zonedOffsetDateTime = zonedValue

public export
zonedLocalDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     ZonedDateTime calendar @{cal} ->
                     CalendarDateTime calendar @{cal}
zonedLocalDateTime = localDateTime . zonedValue

public export
zonedOffset : {calendar : Type} -> {auto cal : Calendar calendar} ->
              ZonedDateTime calendar @{cal} -> Offset
zonedOffset = offsetOf . zonedValue

public export
zonedInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
               {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
               ZonedDateTime calendar @{cal} -> Instant
zonedInstant = toInstant . zonedValue

public export
zoneOf : {calendar : Type} -> {auto cal : Calendar calendar} ->
         ZonedDateTime calendar @{cal} -> DateTimeZone
zoneOf = zonedZone

||| The complete result of resolving a local date-time into a zone.
public export
data ZonedMapping : (calendar : Type) ->
                    (cal : Calendar calendar) -> Type where
  ZonedSkipped : ZonedMapping calendar cal
  ZonedUnambiguous : ZonedDateTime calendar @{cal} -> ZonedMapping calendar cal
  ZonedAmbiguous : (earliest : ZonedDateTime calendar @{cal}) ->
                   (next : ZonedDateTime calendar @{cal}) ->
                   (additional : List (ZonedDateTime calendar @{cal})) ->
                   ZonedMapping calendar cal

attachZone : {calendar : Type} -> {auto cal : Calendar calendar} ->
             DateTimeZone -> OffsetDateTime calendar @{cal} ->
             ZonedDateTime calendar @{cal}
attachZone valueZone value = MkZonedDateTime value valueZone

attachAll : {calendar : Type} -> {auto cal : Calendar calendar} ->
            DateTimeZone -> List (OffsetDateTime calendar @{cal}) ->
            List (ZonedDateTime calendar @{cal})
attachAll valueZone [] = []
attachAll valueZone (value :: rest) =
  attachZone valueZone value :: attachAll valueZone rest

||| Resolve a local date-time without choosing silently between skipped or
||| ambiguous mappings.
public export
resolveLocal : {calendar : Type} -> {auto cal : Calendar calendar} ->
               {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
               DateTimeZone -> CalendarDateTime calendar @{cal} ->
               ZonedMapping calendar cal
resolveLocal valueZone local = case mapLocal valueZone local of
  Skipped => ZonedSkipped
  Unambiguous value => ZonedUnambiguous (attachZone valueZone value)
  Ambiguous first second rest => ZonedAmbiguous
    (attachZone valueZone first)
    (attachZone valueZone second)
    (attachAll valueZone rest)

||| Change zones while preserving the represented instant.
public export
withZone : {calendar : Type} -> {auto cal : Calendar calendar} ->
           {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
           DateTimeZone -> ZonedDateTime calendar @{cal} ->
           Either CalendarConversionError (ZonedDateTime calendar @{cal})
withZone valueZone value = inZone valueZone (zonedInstant value)

||| Change calendars while preserving the instant and zone.
public export
withCalendar : {source : Type} -> {target : Type} ->
               {auto sourceCal : Calendar source} ->
               {auto targetCal : Calendar target} ->
               {auto sourceRep : HasCalendarDate (CalendarDate source @{sourceCal})} ->
               {auto targetRep : HasCalendarDate (CalendarDate target @{targetCal})} ->
               ZonedDateTime source @{sourceCal} ->
               Either CalendarConversionError (ZonedDateTime target @{targetCal})
withCalendar {target} @{sourceCal} @{targetCal} @{sourceRep} @{targetRep} value =
  inZone {calendar = target} @{targetCal} @{targetRep} value.zonedZone
    (zonedInstant @{sourceCal} @{sourceRep} value)

||| Add elapsed time on the global timeline, then re-evaluate the zone offset.
public export
addZonedDuration : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                   Duration -> ZonedDateTime calendar @{cal} ->
                   Either CalendarConversionError (ZonedDateTime calendar @{cal})
addZonedDuration amount value =
  inZone value.zonedZone (addDuration (zonedInstant value) amount)

||| Subtract elapsed time on the global timeline, then re-evaluate the zone offset.
public export
subtractZonedDuration : {calendar : Type} -> {auto cal : Calendar calendar} ->
                        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                        Duration -> ZonedDateTime calendar @{cal} ->
                        Either CalendarConversionError (ZonedDateTime calendar @{cal})
subtractZonedDuration amount value =
  inZone value.zonedZone (subtractDuration (zonedInstant value) amount)

||| Apply calendar-relative fields to local time, then explicitly resolve the
||| result through the zone. This may be skipped or ambiguous at a transition.
public export
applyZonedPeriod : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                   Period (CalendarDateTime calendar @{cal}) ->
                   ZonedDateTime calendar @{cal} -> ZonedMapping calendar cal
applyZonedPeriod period value = resolveLocal value.zonedZone
  (applyPeriod period (zonedLocalDateTime value))
