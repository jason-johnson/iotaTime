module IotaTime.Pattern.Offset

import Data.String.Parser
import IotaTime.Offset
import IotaTime.Pattern

%default total

digit : PatternParser Char
digit = Parser.satisfy (\value => value >= '0' && value <= '9') <?> "digit"

twoDigits : PatternParser Integer
twoDigits = do
  tens <- digit
  units <- digit
  pure ((cast tens - cast '0') * 10 + cast units - cast '0')

offsetValue : String -> Bool -> PatternParser (Either PatternError Offset)
offsetValue separator withSeconds = do
  sign <- (Parser.char '-' *> pure (-1)) <|> (Parser.char '+' *> pure 1)
  valueHours <- twoDigits
  ignore (Parser.string separator)
  valueMinutes <- twoDigits
  valueSeconds <- if withSeconds
    then Parser.string separator *> twoDigits
    else pure 0
  if valueMinutes > 59 || valueSeconds > 59
    then pure (Left (InvalidValue "invalid offset component"))
    else pure (case refineOffsetSeconds
      (sign * (valueHours * 3600 + valueMinutes * 60 + valueSeconds)) of
        Left _ => Left (InvalidValue "offset is outside -18:00 to +18:00")
        Right value => Right value)

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

padTwo : Integer -> String
padTwo value =
  let shown = show value
      width = length (unpack shown)
   in if width >= 2 then shown else zeros (2 `minus` width) ++ shown

renderOffset : String -> Bool -> Offset -> String
renderOffset separator withSeconds value =
  let totalSeconds = totalOffsetSeconds value
      magnitude = abs totalSeconds
      valueHours = magnitude `div` 3600
      valueMinutes = magnitude `div` 60 `mod` 60
      valueSeconds = magnitude `mod` 60
      suffix = if withSeconds
        then separator ++ padTwo valueSeconds
        else ""
  in (if totalSeconds < 0 then "-" else "+") ++ padTwo valueHours ++
      separator ++ padTwo valueMinutes ++ suffix

offsetPattern : String -> Bool -> Pattern Offset Offset
offsetPattern separator withSeconds = MkPattern
  zeroOffset
  Right
  (map (map const) (offsetValue separator withSeconds))
  (renderOffset separator withSeconds)

||| ISO-8601 signed hours and minutes, such as +02:00 or -05:30.
public export
pOffset : Pattern Offset Offset
pOffset = offsetPattern ":" False

||| Signed hours, minutes, and seconds, such as -05:30:15.
public export
pOffsetFull : Pattern Offset Offset
pOffsetFull = offsetPattern ":" True

||| The ISO-8601 offset pattern with Z used for UTC.
public export
pOffsetZ : Pattern Offset Offset
pOffsetZ = MkPattern
  zeroOffset
  Right
  ((Parser.char 'Z' *> pure (Right (const zeroOffset))) <|>
    map (map const) (offsetValue ":" False))
  (\value => if totalOffsetSeconds value == 0
    then "Z"
    else renderOffset ":" False value)

||| The strftime %z form, such as +0200 or -0530.
public export
pOffsetCompact : Pattern Offset Offset
pOffsetCompact = offsetPattern "" False