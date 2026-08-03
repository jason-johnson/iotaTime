module IotaTime.Pattern

import Data.String.Parser
import Data.String

%default total

public export
data PatternError
  = UnexpectedEnd Integer String
  | UnexpectedCharacter Integer String Char
  | InvalidNumber Integer String
  | ValueOutOfRange String Integer Integer Integer
  | TrailingInput Integer String
  | InvalidValue String

public export
PatternParser : Type -> Type
PatternParser = Parser.Parser

public export
record Pattern state value where
  constructor MkPattern
  initialState : state
  finish : state -> Either PatternError value
  parsePart : PatternParser (Either PatternError (state -> state))
  formatPart : value -> String

public export
record LiteralPattern where
  constructor MkLiteralPattern
  literalText : String

public export
string : String -> LiteralPattern
string = MkLiteralPattern

public export
char : Char -> LiteralPattern
char value = MkLiteralPattern (pack [value])

public export
literalField : Pattern state value -> String -> Pattern state value
literalField template text = MkPattern
  template.initialState
  template.finish
  (do
    ignore (Parser.string text)
    pure (Right id))
  (const text)

public export
appendLiteral : Pattern state value -> LiteralPattern -> Pattern state value
appendLiteral pattern literal = MkPattern
  pattern.initialState
  pattern.finish
  (do
    result <- pattern.parsePart
    case result of
      Left error => pure (Left error)
      Right update => do
        ignore (Parser.string literal.literalText)
        pure (Right update))
  (\value => pattern.formatPart value ++ literal.literalText)

export infixl 7 <%

public export
(<%) : Pattern state value -> LiteralPattern -> Pattern state value
(<%) = appendLiteral

public export
Semigroup (Pattern state value) where
  left <+> right = MkPattern
    left.initialState
    left.finish
    (do
      resultLeft <- left.parsePart
      case resultLeft of
        Left error => pure (Left error)
        Right updateLeft => do
          resultRight <- right.parsePart
          pure (map (\updateRight => updateRight . updateLeft) resultRight))
    (\value => left.formatPart value ++ right.formatPart value)

public export
format : Pattern state value -> value -> String
format pattern = pattern.formatPart

structuralError : String -> Int -> String -> Either PatternError value
structuralError source position expected =
  if position >= strLength source
    then Left (UnexpectedEnd (cast position) expected)
    else case unpack (strSubstr position 1 source) of
      actual :: _ => Left (UnexpectedCharacter (cast position) expected actual)
      [] => Left (UnexpectedEnd (cast position) expected)

public export
parseWith : Pattern state value -> state -> String -> Either PatternError value
parseWith pattern start source =
  let initial = Parser.S source 0 (strLength source)
  in case runIdentity (pattern.parsePart.runParser initial) of
        Parser.Fail position expected => structuralError source position expected
        Parser.OK result final => case result of
          Left error => Left error
          Right update => if final.pos == final.maxPos
            then pattern.finish (update start)
            else Left (TrailingInput (cast final.pos)
              (strSubstr final.pos (final.maxPos - final.pos) source))

public export
parse : Pattern state value -> String -> Either PatternError value
parse pattern = parseWith pattern pattern.initialState

decimalDigit : PatternParser Char
decimalDigit = Parser.satisfy (\value => value >= '0' && value <= '9')
  <?> "digit"

fixedDigits : Nat -> PatternParser (List Char)
fixedDigits Z = pure []
fixedDigits (S width) = [| decimalDigit :: fixedDigits width |]

upToDigits : Nat -> PatternParser (List Char)
upToDigits Z = pure []
upToDigits (S maximum) = do
  next <- Parser.optional decimalDigit
  case next of
    Nothing => pure []
    Just digit => map (digit ::) (upToDigits maximum)

variableDigits : Nat -> PatternParser (List Char)
variableDigits Z = Parser.fail "digit"
variableDigits (S maximum) = [| decimalDigit :: upToDigits maximum |]

currentPosition : PatternParser Int
currentPosition = Parser.P (\state => pure (Parser.OK state.pos state))

readDigits : List Char -> Integer
readDigits = foldl (\value, digit => value * 10 + cast digit - cast '0') 0

numberPart : (width : Nat) -> (maximumWidth : Nat) ->
             (minimum : Integer) -> (maximum : Integer) ->
             PatternParser (Either PatternError Integer)
numberPart width maximumWidth minimum maximum = do
  position <- currentPosition
  digits <- if width <= 1
    then variableDigits maximumWidth
    else fixedDigits width
  let value = readDigits digits
  pure (if value >= minimum && value <= maximum
    then Right value
    else Left (ValueOutOfRange (pack digits) minimum maximum (cast position)))

public export
numberUpdatePart : (Integer -> state -> state) ->
                   (width : Nat) -> (maximumWidth : Nat) ->
                   (minimum : Integer) -> (maximum : Integer) ->
                   PatternParser (Either PatternError (state -> state))
numberUpdatePart setter width maximumWidth minimum maximum =
  map (map setter) (numberPart width maximumWidth minimum maximum)

caseInsensitive : String -> PatternParser ()
caseInsensitive value = consume (unpack value)
  where
    consume : List Char -> PatternParser ()
    consume [] = pure ()
    consume (expected :: rest) = do
      ignore (Parser.satisfy
        (\actual => Prelude.toLower actual == Prelude.toLower expected))
      consume rest

namedChoice : List (String, field) -> PatternParser field
namedChoice [] = Parser.fail "named field"
namedChoice ((name, value) :: rest) =
  (caseInsensitive name *> pure value) <|> namedChoice rest

public export
namedUpdatePart : List (String, field) -> (field -> state -> state) ->
                  PatternParser (Either PatternError (state -> state))
namedUpdatePart choices setter = map (Right . setter) (namedChoice choices)

public export
namedConsumePart : List String ->
                   PatternParser (Either PatternError (state -> state))
namedConsumePart names = map (const (Right id))
  (namedChoice (map (\name => (name, ())) names))

public export
spaceNumberUpdatePart : (Integer -> state -> state) ->
                        (maximumWidth : Nat) ->
                        (minimum : Integer) -> (maximum : Integer) ->
                        PatternParser (Either PatternError (state -> state))
spaceNumberUpdatePart setter maximumWidth minimum maximum = do
  ignore (Parser.optional (Parser.char ' '))
  map (map setter) (numberPart 1 maximumWidth minimum maximum)
