module IotaTime.Pattern.ZonedDateTime

import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.DateTimeZone
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime
import IotaTime.Pattern.Scalar
import IotaTime.Period
import IotaTime.ZonedDateTime

%default total

||| A format-only ZonedDateTime pattern. Parsing requires loading a zone and
||| choosing how skipped or ambiguous local times are resolved.
export
record ZonedDateTimePattern state value where
  constructor MkZonedDateTimePattern
  zonedFormatPart : value -> String

||| Build a format-only ZonedDateTime pattern from a local date-time pattern
||| and a function that renders the zone suffix.
public export
zonedDateTimePattern :
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern state (CalendarDateTime calendar) ->
  (ZonedDateTime calendar -> String) ->
  ZonedDateTimePattern state (ZonedDateTime calendar)
zonedDateTimePattern local render = MkZonedDateTimePattern
  (\value => IotaTime.Pattern.format local
    (IotaTime.ZonedDateTime.toCalendarDateTime value) ++ render value)

||| Format a zoned value using its local date-time and rendered zone suffix.
public export
formatZonedDateTime : ZonedDateTimePattern state value -> value -> String
formatZonedDateTime pattern = pattern.zonedFormatPart

||| ISO local date-time followed by a space and the zone ID.
public export
pZonedDateTime : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  ZonedDateTimePattern (DateFields, TimeFields) (ZonedDateTime calendar)
pZonedDateTime = zonedDateTimePattern ps
  (\value => " " ++ IotaTime.ZonedDateTime.zoneId value)

||| ISO local date-time followed by a quoted, escaped zone ID. This form can
||| represent Windows identifiers containing spaces.
public export
pZonedDateTimeQuoted : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  ZonedDateTimePattern (DateFields, TimeFields) (ZonedDateTime calendar)
pZonedDateTimeQuoted = zonedDateTimePattern ps
  (\value => " " ++ IotaTime.Pattern.format pZoneIdQuoted
    (IotaTime.ZonedDateTime.zoneId value))

public export
data ZonedDateTimePatternError providerError resolverError
  = ZonedDateTimeParseError PatternError
  | ZonedDateTimeProviderError providerError
  | ZonedDateTimeResolutionError resolverError

zoneInfoPattern : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Pattern state (CalendarDateTime calendar) ->
  Pattern zoneState String ->
  Pattern (state, zoneState) (CalendarDateTime calendar, String)
zoneInfoPattern local zone = pairPattern fst snd
  (\dateTime, zoneId => (dateTime, zoneId))
  (local <% char ' ') zone

||| Parse using explicit local date-time and zone-ID patterns, then load and
||| resolve the captured zone. This lets protocols choose token or quoted zone
||| syntax in advance.
public export
parseZonedDateTimePatternWith :
  {m : Type -> Type} -> Monad m =>
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern state (CalendarDateTime calendar) ->
  Pattern zoneState String ->
  (String -> m (Either providerError TimeZone)) ->
  (CalendarDateTime calendar -> TimeZone ->
    Either resolverError (ZonedDateTime calendar)) ->
  String ->
  m (Either (ZonedDateTimePatternError providerError resolverError)
    (ZonedDateTime calendar))
parseZonedDateTimePatternWith local zone provider resolver source =
  case IotaTime.Pattern.parse (zoneInfoPattern local zone) source of
    Left error => pure (Left (ZonedDateTimeParseError error))
    Right (dateTime, zoneId) => do
      loaded <- provider zoneId
      pure $ case loaded of
        Left error => Left (ZonedDateTimeProviderError error)
        Right valueZone => case resolver dateTime valueZone of
          Left error => Left (ZonedDateTimeResolutionError error)
          Right value => Right value

||| Parse using a local date-time pattern, load the captured zone ID, and
||| resolve the local value according to the caller's chosen policy.
||| The provider effect is any `Monad`, allowing use from `IO`, effect
||| interpreters, or pure test monads.
public export
parseZonedDateTimeWith :
  {m : Type -> Type} -> Monad m =>
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern state (CalendarDateTime calendar) ->
  (String -> m (Either providerError TimeZone)) ->
  (CalendarDateTime calendar -> TimeZone ->
    Either resolverError (ZonedDateTime calendar)) ->
  String ->
  m (Either (ZonedDateTimePatternError providerError resolverError)
    (ZonedDateTime calendar))
parseZonedDateTimeWith local provider resolver source =
  parseZonedDateTimePatternWith local pZoneIdToken provider resolver source

||| Parse the standard ISO local date-time and zone-ID layout in the provider's
||| monad.
public export
parseStandardZonedDateTime :
  {m : Type -> Type} -> Monad m =>
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  (String -> m (Either providerError TimeZone)) ->
  (CalendarDateTime calendar -> TimeZone ->
    Either resolverError (ZonedDateTime calendar)) ->
  String ->
  m (Either (ZonedDateTimePatternError providerError resolverError)
    (ZonedDateTime calendar))
parseStandardZonedDateTime = parseZonedDateTimeWith ps