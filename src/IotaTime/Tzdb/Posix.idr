module IotaTime.Tzdb.Posix

import public IotaTime.DateTimeZone
import Data.List

%default total

public export
data PosixTzError
  = ExpectedIdentifier
  | ExpectedNumber
  | ExpectedCharacter Char
  | InvalidTime Integer Integer Integer
  | PosixOffsetOutOfRange Integer
  | PosixRuleOutOfRange RecurrenceRuleError
  | MissingDaylightRules
  | UnexpectedTrailingInput String

public export
data PosixZone
  = PosixFixed TransitionInfo
  | PosixRecurring ZoneRecurrence

Parser : Type -> Type
Parser value = List Char -> Either PosixTzError (value, List Char)

isIdentifierLetter : Char -> Bool
isIdentifierLetter value =
  (value >= 'A' && value <= 'Z') || (value >= 'a' && value <= 'z')

isDecimalDigit : Char -> Bool
isDecimalDigit value = value >= '0' && value <= '9'

spanChars : (Char -> Bool) -> List Char -> (List Char, List Char)
spanChars predicate [] = ([], [])
spanChars predicate values@(value :: rest) =
  if predicate value
    then let (matching, remaining) = spanChars predicate rest
          in (value :: matching, remaining)
    else ([], values)

parseIdentifier : Parser String
parseIdentifier ('<' :: rest) =
  let (name, remaining) = spanChars (/= '>') rest
   in case remaining of
        '>' :: after => if null name
          then Left ExpectedIdentifier
          else Right (pack name, after)
        _ => Left (ExpectedCharacter '>')
parseIdentifier input =
  let (name, remaining) = spanChars isIdentifierLetter input
   in if length name >= 3
        then Right (pack name, remaining)
        else Left ExpectedIdentifier

parseDigits : Parser Integer
parseDigits input =
  let (digits, remaining) = spanChars isDecimalDigit input
   in if null digits
        then Left ExpectedNumber
        else Right (foldl (\value, digit =>
          value * 10 + cast digit - cast '0') 0 digits, remaining)

parseOptionalComponent : List Char -> Either PosixTzError (Integer, List Char)
parseOptionalComponent (':' :: rest) = parseDigits rest
parseOptionalComponent input = Right (0, input)

parseSignedTime : Parser Integer
parseSignedTime input = do
  let (sign, unsigned) = case input of
        '-' :: rest => (-1, rest)
        '+' :: rest => (1, rest)
        _ => (1, input)
  (hours, afterHours) <- parseDigits unsigned
  (minutes, afterMinutes) <- parseOptionalComponent afterHours
  (seconds, remaining) <- parseOptionalComponent afterMinutes
  if minutes > 59 || seconds > 59
    then Left (InvalidTime hours minutes seconds)
    else Right (sign * (hours * 3600 + minutes * 60 + seconds), remaining)

parseOffset : Parser Offset
parseOffset input = do
  (posixSeconds, remaining) <- parseSignedTime input
  let utcSeconds = negate posixSeconds
  case refineOffsetSeconds utcSeconds of
    Left _ => Left (PosixOffsetOutOfRange utcSeconds)
    Right value => Right (value, remaining)

expect : Char -> List Char -> Either PosixTzError (List Char)
expect wanted (actual :: rest) =
  if actual == wanted then Right rest else Left (ExpectedCharacter wanted)
expect wanted [] = Left (ExpectedCharacter wanted)

parseMode : List Char -> (TransitionTimeMode, List Char)
parseMode ('s' :: rest) = (StandardTime, rest)
parseMode ('u' :: rest) = (UniversalTime, rest)
parseMode ('g' :: rest) = (UniversalTime, rest)
parseMode ('z' :: rest) = (UniversalTime, rest)
parseMode ('w' :: rest) = (WallTime, rest)
parseMode input = (WallTime, input)

parseRuleTime : List Char -> Either PosixTzError
  ((Integer, TransitionTimeMode), List Char)
parseRuleTime ('/' :: rest) = do
  (seconds, afterTime) <- parseSignedTime rest
  let (mode, remaining) = parseMode afterTime
  Right ((seconds, mode), remaining)
parseRuleTime input = Right ((7200, WallTime), input)

mapRuleError : Either RecurrenceRuleError RecurrenceRule ->
               Either PosixTzError RecurrenceRule
mapRuleError (Left error) = Left (PosixRuleOutOfRange error)
mapRuleError (Right value) = Right value

parseMonthRule : Parser RecurrenceRule
parseMonthRule input = do
  (month, afterMonth) <- parseDigits input
  afterFirstDot <- expect '.' afterMonth
  (week, afterWeek) <- parseDigits afterFirstDot
  afterSecondDot <- expect '.' afterWeek
  (weekday, afterWeekday) <- parseDigits afterSecondDot
  ((seconds, mode), remaining) <- parseRuleTime afterWeekday
  rule <- mapRuleError (monthWeekDayRule month week weekday seconds mode)
  Right (rule, remaining)

parseJulianRule : Bool -> Parser RecurrenceRule
parseJulianRule withoutLeap input = do
  (day, afterDay) <- parseDigits input
  ((seconds, mode), remaining) <- parseRuleTime afterDay
  rule <- mapRuleError (if withoutLeap
    then julianWithoutLeapRule day seconds mode
    else julianWithLeapRule day seconds mode)
  Right (rule, remaining)

parseRule : Parser RecurrenceRule
parseRule ('M' :: rest) = parseMonthRule rest
parseRule ('J' :: rest) = parseJulianRule True rest
parseRule input = parseJulianRule False input

parseDaylight : String -> Offset -> String -> List Char ->
                Either PosixTzError (PosixZone, List Char)
parseDaylight standardName standardOffset daylightName input = do
  (daylightOffset, afterOffset) <- case input of
    ',' :: _ => defaultOffset
    [] => defaultOffset
    _ => parseOffset input
  afterStartComma <- expect ',' afterOffset
  (startRule, afterStart) <- parseRule afterStartComma
  afterEndComma <- expect ',' afterStart
  (endRule, remaining) <- parseRule afterEndComma
  let standardInfo = transitionInfo standardOffset False standardName
  let daylightInfo = transitionInfo daylightOffset True daylightName
  Right (PosixRecurring
    (zoneRecurrence standardInfo daylightInfo startRule endRule), remaining)
  where
    defaultOffset : Either PosixTzError (Offset, List Char)
    defaultOffset = case refineOffsetSeconds
      (totalOffsetSeconds standardOffset + 3600) of
        Left _ => Left (PosixOffsetOutOfRange
          (totalOffsetSeconds standardOffset + 3600))
        Right value => Right (value, input)

||| Parse one complete POSIX TZ string from a TZif footer.
public export
parsePosixZone : String -> Either PosixTzError PosixZone
parsePosixZone source = do
  (standardName, afterStandardName) <- parseIdentifier (unpack source)
  (standardOffset, afterStandardOffset) <- parseOffset afterStandardName
  case afterStandardOffset of
    [] => Right (PosixFixed
      (transitionInfo standardOffset False standardName))
    _ => do
      (daylightName, afterDaylightName) <- parseIdentifier afterStandardOffset
      if null afterDaylightName
        then Left MissingDaylightRules
        else do
          (zone, remaining) <- parseDaylight standardName standardOffset
            daylightName afterDaylightName
          case remaining of
            [] => Right zone
            _ => Left (UnexpectedTrailingInput (pack remaining))