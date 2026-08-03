module IotaTime.Pattern.Locale

import Data.String
import Data.String.Parser
import IotaTime.Locale
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.Offset
import IotaTime.Pattern.OffsetDateTime
import IotaTime.Pattern.LocalTime
import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.LocalTime
import IotaTime.Offset
import IotaTime.OffsetDateTime
import IotaTime.Period
import IotaTime.ZonedDateTime

%default total

public export
data StrftimeError
  = UnsupportedSpecifier Char
  | DanglingPercent
  | MissingOffsetSpecifier
  | MissingZoneSpecifier

public export
Eq StrftimeError where
  UnsupportedSpecifier left == UnsupportedSpecifier right = left == right
  DanglingPercent == DanglingPercent = True
  MissingOffsetSpecifier == MissingOffsetSpecifier = True
  MissingZoneSpecifier == MissingZoneSpecifier = True
  _ == _ = False

public export
Show StrftimeError where
  show (UnsupportedSpecifier value) =
    "unsupported strftime specifier: %" ++ pack [value]
  show DanglingPercent = "strftime layout ends with a bare %"
  show MissingOffsetSpecifier = "strftime layout has no numeric %z offset"
  show MissingZoneSpecifier = "strftime layout has no %Z zone abbreviation"

data LayoutToken = LiteralToken Char | ConversionToken Char

data LayoutFragment = LiteralRun String | Conversion Char

compositeTokens : Char -> Maybe (List LayoutToken)
compositeTokens 'T' = Just
  [ ConversionToken 'H', LiteralToken ':', ConversionToken 'M'
  , LiteralToken ':', ConversionToken 'S'
  ]
compositeTokens 'R' = Just
  [ ConversionToken 'H', LiteralToken ':', ConversionToken 'M' ]
compositeTokens 'r' = Just
  [ ConversionToken 'I', LiteralToken ':', ConversionToken 'M'
  , LiteralToken ':', ConversionToken 'S', LiteralToken ' '
  , ConversionToken 'p'
  ]
compositeTokens 'F' = Just
  [ ConversionToken 'Y', LiteralToken '-', ConversionToken 'm'
  , LiteralToken '-', ConversionToken 'd'
  ]
compositeTokens 'D' = Just
  [ ConversionToken 'm', LiteralToken '/', ConversionToken 'd'
  , LiteralToken '/', ConversionToken 'y'
  ]
compositeTokens _ = Nothing

tokenize : List Char -> Either StrftimeError (List LayoutToken)
tokenize [] = Right []
tokenize ['%'] = Left DanglingPercent
tokenize ('%' :: specifier :: rest) = do
  suffix <- tokenize rest
  case specifier of
    '%' => Right (LiteralToken '%' :: suffix)
    'n' => Right (LiteralToken '\n' :: suffix)
    't' => Right (LiteralToken '\t' :: suffix)
    _ => case compositeTokens specifier of
      Just expansion => Right (expansion ++ suffix)
      Nothing => Right (ConversionToken specifier :: suffix)
tokenize (value :: rest) = map (LiteralToken value ::) (tokenize rest)

toFragments : List LayoutToken -> List LayoutFragment
toFragments = foldr step []
  where
    step : LayoutToken -> List LayoutFragment -> List LayoutFragment
    step (LiteralToken value) (LiteralRun text :: rest) =
      LiteralRun (pack [value] ++ text) :: rest
    step (LiteralToken value) rest = LiteralRun (pack [value]) :: rest
    step (ConversionToken value) rest = Conversion value :: rest

dateConversion : Locale -> Char ->
                 Either StrftimeError
                   (Pattern DateFields (CalendarDate Gregorian))
dateConversion locale 'Y' = Right (pyyyy {calendar = Gregorian})
dateConversion locale 'y' = Right (pyy {calendar = Gregorian})
dateConversion locale 'm' = Right (pMM {calendar = Gregorian})
dateConversion locale 'd' = Right (pdd {calendar = Gregorian})
dateConversion locale 'e' = Right (pdaySpace {calendar = Gregorian})
dateConversion locale 'B' = Right (pMMMM' locale)
dateConversion locale 'b' = Right (pMMM' locale)
dateConversion locale 'h' = Right (pMMM' locale)
dateConversion locale 'A' = Right (pdddd' locale)
dateConversion locale 'a' = Right (pddd' locale)
dateConversion _ value = Left (UnsupportedSpecifier value)

fragmentPattern : Pattern state value ->
                  (Char -> Either StrftimeError (Pattern state value)) ->
                  LayoutFragment -> Either StrftimeError (Pattern state value)
fragmentPattern template _ (LiteralRun text) =
  Right (literalField template text)
fragmentPattern _ conversion (Conversion value) = conversion value

assemble : Pattern state value ->
           (Char -> Either StrftimeError (Pattern state value)) ->
           List LayoutFragment -> Either StrftimeError (Pattern state value)
assemble template _ [] = Right (literalField template "")
assemble template conversion [fragment] =
  fragmentPattern template conversion fragment
assemble template conversion (fragment :: rest) = do
  first <- fragmentPattern template conversion fragment
  remaining <- assemble template conversion rest
  Right (first <+> remaining)

public export
compileDatePattern : Locale -> String ->
                     Either StrftimeError
                       (Pattern DateFields (CalendarDate Gregorian))
compileDatePattern locale layout = do
  tokens <- tokenize (unpack layout)
  assemble (pyyyy {calendar = Gregorian}) (dateConversion locale)
    (toFragments tokens)

public export
localeDatePattern : Locale ->
                    Either StrftimeError
                      (Pattern DateFields (CalendarDate Gregorian))
localeDatePattern locale = compileDatePattern locale (rawDateFormat locale)

timeConversion : Locale -> Char ->
                 Either StrftimeError (Pattern TimeFields LocalTime)
timeConversion _ 'H' = Right pHH
timeConversion _ 'I' = Right phh
timeConversion _ 'l' = Right phhSpace
timeConversion _ 'M' = Right pmm
timeConversion _ 'S' = Right pss
timeConversion locale 'p' = Right (ppp' locale)
timeConversion _ value = Left (UnsupportedSpecifier value)

public export
compileTimePattern : Locale -> String ->
                     Either StrftimeError (Pattern TimeFields LocalTime)
compileTimePattern locale layout = do
  tokens <- tokenize (unpack layout)
  assemble pHH (timeConversion locale) (toFragments tokens)

public export
localeTimePattern : Locale ->
                    Either StrftimeError (Pattern TimeFields LocalTime)
localeTimePattern locale = compileTimePattern locale (rawTimeFormat locale)

public export
record DateTimeFields where
  constructor MkDateTimeFields
  parsedDateFields : DateFields
  parsedTimeFields : TimeFields

initialDateTimeFields : DateTimeFields
initialDateTimeFields = MkDateTimeFields
  (pyyyy {calendar = Gregorian}).initialState pHH.initialState

finishDateTime : DateTimeFields ->
                 Either PatternError (CalendarDateTime Gregorian)
finishDateTime fields = do
  date <- (pyyyy {calendar = Gregorian}).finish fields.parsedDateFields
  time <- pHH.finish fields.parsedTimeFields
  Right (on time date)

liftDateUpdate : (DateFields -> DateFields) ->
                 DateTimeFields -> DateTimeFields
liftDateUpdate update fields =
  { parsedDateFields := update fields.parsedDateFields } fields

liftTimeUpdate : (TimeFields -> TimeFields) ->
                 DateTimeFields -> DateTimeFields
liftTimeUpdate update fields =
  { parsedTimeFields := update fields.parsedTimeFields } fields

liftDatePattern : Pattern DateFields (CalendarDate Gregorian) ->
                  Pattern DateTimeFields (CalendarDateTime Gregorian)
liftDatePattern pattern = MkPattern
  initialDateTimeFields
  finishDateTime
  (map (map liftDateUpdate) pattern.parsePart)
  (pattern.formatPart . datePart)

liftTimePattern : Pattern TimeFields LocalTime ->
                  Pattern DateTimeFields (CalendarDateTime Gregorian)
liftTimePattern pattern = MkPattern
  initialDateTimeFields
  finishDateTime
  (map (map liftTimeUpdate) pattern.parsePart)
  (pattern.formatPart . localTimeOfDay)

dateTimeConversion : Locale -> Char ->
                     Either StrftimeError
                       (Pattern DateTimeFields (CalendarDateTime Gregorian))
dateTimeConversion locale value = case dateConversion locale value of
  Right pattern => Right (liftDatePattern pattern)
  Left _ => map liftTimePattern (timeConversion locale value)

isZoneSpecifier : Char -> Bool
isZoneSpecifier value = value == 'Z' || value == 'z'

trimTrailingSpaces : String -> String
trimTrailingSpaces = pack . reverse . dropSpaces . reverse . unpack
  where
    dropSpaces : List Char -> List Char
    dropSpaces (' ' :: rest) = dropSpaces rest
    dropSpaces values = values

stripZones : List LayoutFragment -> List LayoutFragment
stripZones [] = []
stripZones (LiteralRun text :: Conversion value :: rest) =
  if isZoneSpecifier value
    then let trimmed = trimTrailingSpaces text in
      if trimmed == ""
        then stripZones rest
        else LiteralRun trimmed :: stripZones rest
    else LiteralRun text :: stripZones (Conversion value :: rest)
stripZones (LiteralRun text :: rest) =
  LiteralRun text :: stripZones rest
stripZones (Conversion value :: rest) =
  if isZoneSpecifier value
    then stripZones rest
    else Conversion value :: stripZones rest

public export
compileDateTimePattern : Locale -> String ->
                         Either StrftimeError
                           (Pattern DateTimeFields
                             (CalendarDateTime Gregorian))
compileDateTimePattern locale layout = do
  tokens <- tokenize (unpack layout)
  assemble (liftDatePattern (pyyyy {calendar = Gregorian}))
    (dateTimeConversion locale)
    (stripZones (toFragments tokens))

public export
localeDateTimePattern : Locale ->
                        Either StrftimeError
                          (Pattern DateTimeFields
                            (CalendarDateTime Gregorian))
localeDateTimePattern locale =
  compileDateTimePattern locale (rawDateTimeFormat locale)

trailingLiterals : List LayoutFragment -> Either StrftimeError String
trailingLiterals [] = Right ""
trailingLiterals (LiteralRun text :: rest) =
  map (text ++) (trailingLiterals rest)
trailingLiterals (Conversion value :: rest) =
  Left (UnsupportedSpecifier value)

splitOffset : List LayoutFragment ->
              Either StrftimeError (List LayoutFragment, String)
splitOffset [] = Left MissingOffsetSpecifier
splitOffset (Conversion 'z' :: rest) =
  map (\trailing => ([], trailing)) (trailingLiterals rest)
splitOffset (fragment :: rest) = do
  (before, trailing) <- splitOffset rest
  Right (fragment :: before, trailing)

public export
compileOffsetDateTimePattern : Locale -> String ->
  Either StrftimeError
    (Pattern (DateTimeFields, Offset) (OffsetDateTime Gregorian))
compileOffsetDateTimePattern locale layout = do
  tokens <- tokenize (unpack layout)
  (before, trailing) <- splitOffset (toFragments tokens)
  localPattern <- assemble (liftDatePattern (pyyyy {calendar = Gregorian}))
    (dateTimeConversion locale) before
  Right (offsetDateTimePattern localPattern
    (pOffsetCompact <% string trailing))

public export
localeOffsetDateTimePattern : Locale ->
  Either StrftimeError
    (Pattern (DateTimeFields, Offset) (OffsetDateTime Gregorian))
localeOffsetDateTimePattern locale =
  compileOffsetDateTimePattern locale (rawDateTimeFormat locale)

public export
data ZonedPatternError providerError resolverError
  = ZonedLayoutError StrftimeError
  | ZonedParseError PatternError
  | ZonedProviderError providerError
  | ZonedResolutionError resolverError

stripPrefix : List Char -> List Char -> Maybe (List Char)
stripPrefix [] values = Just values
stripPrefix (expected :: expectedRest) (actual :: values) =
  if expected == actual then stripPrefix expectedRest values else Nothing
stripPrefix _ _ = Nothing

removeSuffix : List Char -> List Char -> Maybe (List Char)
removeSuffix values suffix =
  map reverse (stripPrefix (reverse suffix) (reverse values))

isZoneCharacter : Char -> Bool
isZoneCharacter value =
  value /= ' ' && value /= '\t' && value /= '\n' && value /= '\r'

zoneTokenPattern : String -> Pattern String String
zoneTokenPattern trailing = MkPattern
  ""
  Right
  (Parser.P (\state =>
    let remaining = strSubstr state.pos (state.maxPos - state.pos) state.input in
    case removeSuffix (unpack remaining) (unpack trailing) of
      Nothing => pure (Parser.Fail state.pos "zone abbreviation")
      Just [] => pure (Parser.Fail state.pos "zone abbreviation")
      Just token => if all isZoneCharacter token
        then pure (Parser.OK (Right (const (pack token)))
          ({ pos := state.maxPos } state))
        else pure (Parser.Fail state.pos "zone abbreviation")))
  id

splitZone : List LayoutFragment ->
            Either StrftimeError (List LayoutFragment, String)
splitZone [] = Left MissingZoneSpecifier
splitZone (Conversion 'Z' :: rest) =
  map (\trailing => ([], trailing)) (trailingLiterals rest)
splitZone (fragment :: rest) = do
  (before, trailing) <- splitZone rest
  Right (fragment :: before, trailing)

zoneInfoPattern : Locale -> String ->
  Either StrftimeError
    (Pattern (DateTimeFields, String) (CalendarDateTime Gregorian, String))
zoneInfoPattern locale layout = do
  tokens <- tokenize (unpack layout)
  (before, trailing) <- splitZone (toFragments tokens)
  localPattern <- assemble (liftDatePattern (pyyyy {calendar = Gregorian}))
    (dateTimeConversion locale) before
  Right (pairPattern fst snd (\local, zone => (local, zone))
    localPattern (zoneTokenPattern trailing))

||| Parse a locale %Z layout, load the captured zone, and resolve local time.
public export
parseZonedDateTime :
  (String -> IO (Either providerError TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolverError (ZonedDateTime Gregorian)) ->
  Locale -> String ->
  IO (Either (ZonedPatternError providerError resolverError)
    (ZonedDateTime Gregorian))
parseZonedDateTime provider resolver locale source =
  case zoneInfoPattern locale (rawDateTimeFormat locale) of
    Left error => pure (Left (ZonedLayoutError error))
    Right pattern => case IotaTime.Pattern.parse pattern source of
      Left error => pure (Left (ZonedParseError error))
      Right (local, zoneToken) => do
        loaded <- provider zoneToken
        pure $ case loaded of
          Left error => Left (ZonedProviderError error)
          Right zone => case resolver local zone of
            Left error => Left (ZonedResolutionError error)
            Right value => Right value
