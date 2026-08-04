module IotaTime.Pattern.ZonedDateTime

import Data.String
import Data.String.Parser
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.DateTimeZone
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime
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

public export
data ZonedDateTimePatternError providerError resolverError
  = ZonedDateTimeParseError PatternError
  | ZonedDateTimeProviderError providerError
  | ZonedDateTimeResolutionError resolverError

isZoneCharacter : Char -> Bool
isZoneCharacter value =
  value /= ' ' && value /= '\t' && value /= '\n' && value /= '\r'

readZoneToken : List Char -> Maybe (String, Nat)
readZoneToken [] = Nothing
readZoneToken (value :: rest) = if isZoneCharacter value
  then readMore [value] 1 rest
  else Nothing
  where
    readMore : List Char -> Nat -> List Char -> Maybe (String, Nat)
    readMore reversed count (value :: rest) = if isZoneCharacter value
      then readMore (value :: reversed) (S count) rest
      else Just (pack (reverse reversed), count)
    readMore reversed count [] = Just (pack (reverse reversed), count)

zoneTokenPattern : Pattern String String
zoneTokenPattern = MkPattern
  ""
  Right
  (Parser.P (\state =>
    let remaining = strSubstr state.pos (state.maxPos - state.pos) state.input in
    case readZoneToken (unpack remaining) of
      Nothing => pure (Parser.Fail state.pos "zone ID")
      Just (token, consumed) => pure (Parser.OK (Right (const token))
        ({ pos := state.pos + cast consumed } state))))
  id

zoneInfoPattern : {calendar : Type} ->
  {auto patterned : CalendarPattern calendar} ->
  Pattern state (CalendarDateTime calendar) ->
  Pattern (state, String) (CalendarDateTime calendar, String)
zoneInfoPattern local = pairPattern fst snd (\dateTime, zone => (dateTime, zone))
  (local <% char ' ') zoneTokenPattern

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
  case IotaTime.Pattern.parse (zoneInfoPattern local) source of
    Left error => pure (Left (ZonedDateTimeParseError error))
    Right (dateTime, zoneToken) => do
      loaded <- provider zoneToken
      pure $ case loaded of
        Left error => Left (ZonedDateTimeProviderError error)
        Right zone => case resolver dateTime zone of
          Left error => Left (ZonedDateTimeResolutionError error)
          Right value => Right value

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