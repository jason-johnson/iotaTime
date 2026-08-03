module IotaTime.Pattern.ZonedDateTime

import Data.String
import Data.String.Parser
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.DateTimeZone
import IotaTime.Pattern
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.CalendarDateTime
import IotaTime.Pattern.LocalTime
import IotaTime.Period
import IotaTime.ZonedDateTime

%default total

||| A format-only ZonedDateTime pattern. Parsing requires loading a zone and
||| choosing how skipped or ambiguous local times are resolved.
export
record ZonedDateTimePattern state where
  constructor MkZonedDateTimePattern
  localPattern : Pattern state (CalendarDateTime Gregorian)
  renderZone : ZonedDateTime Gregorian -> String

||| Build a format-only ZonedDateTime pattern from a local date-time pattern
||| and a function that renders the zone suffix.
public export
zonedDateTimePattern :
  Pattern state (CalendarDateTime Gregorian) ->
  (ZonedDateTime Gregorian -> String) ->
  ZonedDateTimePattern state
zonedDateTimePattern = MkZonedDateTimePattern

||| Format a zoned value using its local date-time and rendered zone suffix.
public export
formatZonedDateTime : ZonedDateTimePattern state ->
                      ZonedDateTime Gregorian -> String
formatZonedDateTime pattern value =
  IotaTime.Pattern.format pattern.localPattern
    (IotaTime.ZonedDateTime.toCalendarDateTime value) ++
  pattern.renderZone value

||| ISO local date-time followed by a space and the zone ID.
public export
pZonedDateTime : ZonedDateTimePattern (DateFields, TimeFields)
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

zoneInfoPattern : Pattern state (CalendarDateTime Gregorian) ->
  Pattern (state, String) (CalendarDateTime Gregorian, String)
zoneInfoPattern local = pairPattern fst snd (\dateTime, zone => (dateTime, zone))
  (local <% char ' ') zoneTokenPattern

||| Parse using a local date-time pattern, load the captured zone ID, and
||| resolve the local value according to the caller's chosen policy.
public export
parseZonedDateTimeWith :
  Pattern state (CalendarDateTime Gregorian) ->
  (String -> IO (Either providerError TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolverError (ZonedDateTime Gregorian)) ->
  String ->
  IO (Either (ZonedDateTimePatternError providerError resolverError)
    (ZonedDateTime Gregorian))
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

||| Parse the standard ISO local date-time and zone-ID layout.
public export
parseStandardZonedDateTime :
  (String -> IO (Either providerError TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolverError (ZonedDateTime Gregorian)) ->
  String ->
  IO (Either (ZonedDateTimePatternError providerError resolverError)
    (ZonedDateTime Gregorian))
parseStandardZonedDateTime = parseZonedDateTimeWith ps