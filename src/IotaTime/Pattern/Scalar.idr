module IotaTime.Pattern.Scalar

import Data.String
import Data.String.Parser
import IotaTime.Calendar
import IotaTime.Instant
import IotaTime.Pattern

%default total

||| An arbitrary-precision instant encoded as nanoseconds relative to the
||| iotaTime epoch.
public export
pInstantNanoseconds : Pattern Integer Instant
pInstantNanoseconds = MkPattern
  0
  (Right . fromNanosecondsSinceEpoch)
  pSignedInteger.parsePart
  (show . toNanosecondsSinceEpoch)

finishCalendarDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     Integer -> Either PatternError
                       (CalendarDate calendar @{cal})
finishCalendarDays {calendar} @{cal} value =
  case choose (isValidDays {calendar} @{cal} value) of
    Left valid => Right (fromDays {calendar} @{cal} value @{valid})
    Right _ => Left (InvalidValue
      (calendarName {calendar} @{cal} ++ " day count is out of range"))

||| A calendar date encoded as its absolute day count relative to the iotaTime
||| epoch. The expected calendar type is supplied by the pattern itself.
public export
pCalendarDays : {calendar : Type} -> {auto cal : Calendar calendar} ->
                Pattern Integer (CalendarDate calendar @{cal})
pCalendarDays {calendar} @{cal} = MkPattern
  0
  (finishCalendarDays {calendar} @{cal})
  pSignedInteger.parsePart
  (show . toDays {calendar} @{cal})

isZoneTokenCharacter : Char -> Bool
isZoneTokenCharacter value =
  value /= ' ' && value /= '\t' && value /= '\n' && value /= '\r'

readZoneToken : List Char -> Maybe (String, Nat)
readZoneToken [] = Nothing
readZoneToken (value :: remaining) =
  if isZoneTokenCharacter value
    then go [value] 1 remaining
    else Nothing
  where
    go : List Char -> Nat -> List Char -> Maybe (String, Nat)
    go found count (value :: remaining) =
      if isZoneTokenCharacter value
        then go (value :: found) (S count) remaining
        else Just (pack (reverse found), count)
    go found count [] = Just (pack (reverse found), count)

zoneTokenParser : PatternParser (Either PatternError (String -> String))
zoneTokenParser = Parser.P (\state =>
  let remaining = unpack
        (strSubstr state.pos (state.maxPos - state.pos) state.input)
   in case readZoneToken remaining of
        Nothing => pure (Parser.Fail state.pos "zone ID token")
        Just (value, consumed) => pure (Parser.OK (Right (const value))
          ({ pos := state.pos + cast consumed } state)))

||| A non-empty zone identifier containing no whitespace. This covers IANA
||| identifiers and is directly composable with surrounding literals.
public export
pZoneIdToken : Pattern String String
pZoneIdToken = MkPattern "" Right zoneTokenParser id

escapeZoneId : List Char -> List Char
escapeZoneId [] = []
escapeZoneId ('\\' :: remaining) = '\\' :: '\\' :: escapeZoneId remaining
escapeZoneId ('"' :: remaining) = '\\' :: '"' :: escapeZoneId remaining
escapeZoneId ('\n' :: remaining) = '\\' :: 'n' :: escapeZoneId remaining
escapeZoneId ('\r' :: remaining) = '\\' :: 'r' :: escapeZoneId remaining
escapeZoneId ('\t' :: remaining) = '\\' :: 't' :: escapeZoneId remaining
escapeZoneId (value :: remaining) = value :: escapeZoneId remaining

readQuotedZoneId : List Char -> Maybe (String, Nat)
readQuotedZoneId ('"' :: remaining) = go [] 1 remaining
  where
    go : List Char -> Nat -> List Char -> Maybe (String, Nat)
    go [] _ ('"' :: _) = Nothing
    go found count ('"' :: _) = Just (pack (reverse found), S count)
    go found count ('\\' :: '"' :: rest) = go ('"' :: found) (count + 2) rest
    go found count ('\\' :: '\\' :: rest) = go ('\\' :: found) (count + 2) rest
    go found count ('\\' :: 'n' :: rest) = go ('\n' :: found) (count + 2) rest
    go found count ('\\' :: 'r' :: rest) = go ('\r' :: found) (count + 2) rest
    go found count ('\\' :: 't' :: rest) = go ('\t' :: found) (count + 2) rest
    go _ _ ('\\' :: _) = Nothing
    go _ _ ('\n' :: _) = Nothing
    go _ _ ('\r' :: _) = Nothing
    go _ _ ('\t' :: _) = Nothing
    go found count (value :: rest) = go (value :: found) (S count) rest
    go _ _ [] = Nothing
readQuotedZoneId _ = Nothing

quotedZoneIdParser : PatternParser (Either PatternError (String -> String))
quotedZoneIdParser = Parser.P (\state =>
  let remaining = unpack
        (strSubstr state.pos (state.maxPos - state.pos) state.input)
   in case readQuotedZoneId remaining of
        Nothing => pure (Parser.Fail state.pos "quoted zone ID")
        Just (value, consumed) => pure (Parser.OK (Right (const value))
          ({ pos := state.pos + cast consumed } state)))

||| A quoted, escaped, non-empty zone identifier. This form represents Windows
||| identifiers containing spaces as well as quote, backslash, and whitespace
||| characters when an application needs them.
public export
pZoneIdQuoted : Pattern String String
pZoneIdQuoted = MkPattern
  ""
  Right
  quotedZoneIdParser
  (\value => "\"" ++ pack (escapeZoneId (unpack value)) ++ "\"")