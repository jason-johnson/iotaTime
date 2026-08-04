module IotaTime.ZonedDateTime

import public IotaTime.DateTimeZone
import public IotaTime.Duration
import public IotaTime.OffsetDateTime

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
export
inZone : {calendar : Type} -> {auto cal : Calendar calendar} ->
         {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
         DateTimeZone -> Instant ->
         Either CalendarConversionError (ZonedDateTime calendar @{cal})
inZone valueZone valueInstant =
  case IotaTime.OffsetDateTime.fromInstant
    (zoneOffsetAt valueZone valueInstant) valueInstant of
      Left error => Left error
      Right value => Right (MkZonedDateTime value valueZone)

||| HodaTime-compatible instant-first constructor.
public export
fromInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
              {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
              Instant -> TimeZone ->
              Either CalendarConversionError (ZonedDateTime calendar @{cal})
fromInstant valueInstant valueZone = inZone valueZone valueInstant

export
zonedOffsetDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                      ZonedDateTime calendar @{cal} ->
                      OffsetDateTime calendar @{cal}
zonedOffsetDateTime = zonedValue

export
zonedLocalDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     ZonedDateTime calendar @{cal} ->
                     CalendarDateTime calendar @{cal}
zonedLocalDateTime = localDateTime . zonedValue

public export
toCalendarDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     ZonedDateTime calendar @{cal} ->
                     CalendarDateTime calendar @{cal}
toCalendarDateTime = zonedLocalDateTime

public export
toCalendarDate : {calendar : Type} -> {auto cal : Calendar calendar} ->
                 ZonedDateTime calendar @{cal} -> CalendarDate calendar @{cal}
toCalendarDate = datePart . zonedLocalDateTime

public export
toLocalTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
              ZonedDateTime calendar @{cal} -> LocalTime
toLocalTime = localTimeOfDay . zonedLocalDateTime

public export
year : {calendar : Type} -> {auto cal : Calendar calendar} ->
  ZonedDateTime calendar @{cal} -> Year
year = IotaTime.Calendar.year . toCalendarDate

public export
month : {calendar : Type} -> {auto cal : Calendar calendar} ->
   (value : ZonedDateTime calendar @{cal}) ->
   MonthRep @{cal} (IotaTime.ZonedDateTime.year value)
month value = IotaTime.Calendar.month (toCalendarDate value)

public export
day : {calendar : Type} -> {auto cal : Calendar calendar} ->
      ZonedDateTime calendar @{cal} -> DayOfMonth
day = IotaTime.Calendar.day . toCalendarDate

public export
hour : {calendar : Type} -> {auto cal : Calendar calendar} ->
  ZonedDateTime calendar @{cal} -> Hour
hour = IotaTime.LocalTime.hour . toLocalTime

public export
minute : {calendar : Type} -> {auto cal : Calendar calendar} ->
    ZonedDateTime calendar @{cal} -> Minute
minute = IotaTime.LocalTime.minute . toLocalTime

public export
second : {calendar : Type} -> {auto cal : Calendar calendar} ->
    ZonedDateTime calendar @{cal} -> Second
second = IotaTime.LocalTime.second . toLocalTime

public export
nanosecond : {calendar : Type} -> {auto cal : Calendar calendar} ->
        ZonedDateTime calendar @{cal} -> Nanosecond
nanosecond = IotaTime.LocalTime.nanosecond . toLocalTime

export
zonedOffset : {calendar : Type} -> {auto cal : Calendar calendar} ->
              ZonedDateTime calendar @{cal} -> Offset
zonedOffset = offsetOf . zonedValue

export
zonedInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
               {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
               ZonedDateTime calendar @{cal} -> Instant
zonedInstant = IotaTime.OffsetDateTime.toInstant . zonedValue

public export
toInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
            {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
            ZonedDateTime calendar @{cal} -> Instant
toInstant = zonedInstant

export
zoneOf : {calendar : Type} -> {auto cal : Calendar calendar} ->
         ZonedDateTime calendar @{cal} -> DateTimeZone
zoneOf = zonedZone

public export
zoneId : {calendar : Type} -> {auto cal : Calendar calendar} ->
         ZonedDateTime calendar @{cal} -> String
zoneId = IotaTime.DateTimeZone.zoneId . zonedZone

public export
inDst : {calendar : Type} -> {auto cal : Calendar calendar} ->
        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
        ZonedDateTime calendar @{cal} -> Bool
inDst value = isDaylightSavingTime
  (activeTransitionAt value.zonedZone (zonedInstant value))

public export
zoneAbbreviation : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                   ZonedDateTime calendar @{cal} -> String
zoneAbbreviation value = abbreviation
  (activeTransitionAt value.zonedZone (zonedInstant value))

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

||| Return every valid mapping of a local calendar date-time, in instant order.
public export
fromCalendarDateTimeAll : {calendar : Type} -> {auto cal : Calendar calendar} ->
                          {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                          CalendarDateTime calendar @{cal} -> TimeZone ->
                          List (ZonedDateTime calendar @{cal})
fromCalendarDateTimeAll local valueZone = case resolveLocal valueZone local of
  ZonedSkipped => []
  ZonedUnambiguous value => [value]
  ZonedAmbiguous first second rest => first :: second :: rest

public export
data ZonedDateTimeError
  = DateTimeDoesNotExist
  | DateTimeAmbiguous
  | LenientResolutionFailed
  | ZonedCalendarOutOfRange CalendarConversionError

||| Resolve only a unique local mapping. Skipped and ambiguous values are
||| returned as typed errors rather than exceptions.
public export
fromCalendarDateTimeStrictly : {calendar : Type} ->
                               {auto cal : Calendar calendar} ->
                               {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                               CalendarDateTime calendar @{cal} -> TimeZone ->
                               Either ZonedDateTimeError
                                 (ZonedDateTime calendar @{cal})
fromCalendarDateTimeStrictly local valueZone =
  case resolveLocal valueZone local of
    ZonedSkipped => Left DateTimeDoesNotExist
    ZonedUnambiguous value => Right value
    ZonedAmbiguous _ _ _ => Left DateTimeAmbiguous

||| Apply HodaTime's lenient rules: choose the earliest ambiguous mapping and
||| shift skipped values forward by the transition gap.
public export
fromCalendarDateTimeLeniently : {calendar : Type} ->
                                {auto cal : Calendar calendar} ->
                                {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                                CalendarDateTime calendar @{cal} -> TimeZone ->
                                Either ZonedDateTimeError
                                  (ZonedDateTime calendar @{cal})
fromCalendarDateTimeLeniently local valueZone =
  case lenientLocalMapping valueZone local of
    Left error => Left (ZonedCalendarOutOfRange error)
    Right Nothing => Left LenientResolutionFailed
    Right (Just value) => Right (attachZone valueZone value)

||| Change zones while preserving the represented instant.
export
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
export
addZonedDuration : {calendar : Type} -> {auto cal : Calendar calendar} ->
                   {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                   Duration -> ZonedDateTime calendar @{cal} ->
                   Either CalendarConversionError (ZonedDateTime calendar @{cal})
addZonedDuration amount value =
  inZone value.zonedZone (addDuration (zonedInstant value) amount)

||| Add fixed elapsed time, following HodaTime's value-first argument order.
export
add : {calendar : Type} -> {auto cal : Calendar calendar} ->
  {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
  ZonedDateTime calendar @{cal} -> Duration ->
  Either CalendarConversionError (ZonedDateTime calendar @{cal})
add value amount = addZonedDuration amount value

||| Subtract elapsed time on the global timeline, then re-evaluate the zone offset.
export
subtractZonedDuration : {calendar : Type} -> {auto cal : Calendar calendar} ->
                        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                        Duration -> ZonedDateTime calendar @{cal} ->
                        Either CalendarConversionError (ZonedDateTime calendar @{cal})
subtractZonedDuration amount value =
  inZone value.zonedZone (subtractDuration (zonedInstant value) amount)

||| Subtract fixed elapsed time, following HodaTime's value-first argument order.
export
minus : {calendar : Type} -> {auto cal : Calendar calendar} ->
        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
        ZonedDateTime calendar @{cal} -> Duration ->
        Either CalendarConversionError (ZonedDateTime calendar @{cal})
minus value amount = subtractZonedDuration amount value
