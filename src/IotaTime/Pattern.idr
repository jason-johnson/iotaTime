module IotaTime.Pattern

import Data.List
import Data.String.Parser
import Data.String

%default total

||| A failure encountered while parsing or refining a patterned value.
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

||| A bidirectional textual representation of a value.
|||
||| `state` accumulates fields during parsing. `finish` validates that state and
||| constructs the value, while `formatPart` projects text from an existing value.
public export
record Pattern state value where
  constructor MkPattern
  initialState : state
  finish : state -> Either PatternError value
  parsePart : PatternParser (Either PatternError (state -> state))
  formatPart : value -> String

||| Literal text that can be appended to a pattern with `<%`.
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

export
literalField : Pattern state value -> String -> Pattern state value
literalField template text = MkPattern
  template.initialState
  template.finish
  (do
    ignore (Parser.string text)
    pure (Right id))
  (const text)

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

pairUpdate : (leftState -> leftState) -> (rightState -> rightState) ->
             (leftState, rightState) -> (leftState, rightState)
pairUpdate updateLeft updateRight (leftState, rightState) =
  (updateLeft leftState, updateRight rightState)

export
pairPattern : (combined -> left) -> (combined -> right) ->
              (left -> right -> combined) ->
              Pattern leftState left -> Pattern rightState right ->
              Pattern (leftState, rightState) combined
pairPattern leftOf rightOf combine left right = MkPattern
  (left.initialState, right.initialState)
  (\(leftState, rightState) => do
    leftValue <- left.finish leftState
    rightValue <- right.finish rightState
    Right (combine leftValue rightValue))
  (do
    parsedLeft <- left.parsePart
    case parsedLeft of
      Left error => pure (Left error)
      Right updateLeft => do
        parsedRight <- right.parsePart
        pure (map (pairUpdate updateLeft) parsedRight))
  (\value => left.formatPart (leftOf value) ++ right.formatPart (rightOf value))

||| Format a value using the supplied pattern.
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

||| Parse an entire string using an explicit initial field state.
|||
||| This is useful for partial patterns whose omitted fields should come from
||| caller policy rather than the pattern's built-in defaults.
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

||| Parse an entire string using a pattern's default initial state.
public export
parse : Pattern state value -> String -> Either PatternError value
parse pattern = parseWith pattern pattern.initialState

isPatternDecimalDigit : Char -> Bool
isPatternDecimalDigit value = value >= '0' && value <= '9'

patternDigitValue : Char -> Integer
patternDigitValue value = cast value - cast '0'

readUnsignedInteger : Integer -> Nat -> List Char ->
                      Maybe (Integer, Nat)
readUnsignedInteger found count (value :: remaining) =
  if isPatternDecimalDigit value
    then readUnsignedInteger
      (found * 10 + patternDigitValue value) (S count) remaining
    else if count == 0 then Nothing else Just (found, count)
readUnsignedInteger found count [] =
  if count == 0 then Nothing else Just (found, count)

signedIntegerParser : PatternParser (Either PatternError (Integer -> Integer))
signedIntegerParser = Parser.P (\state =>
  let remaining = unpack
        (strSubstr state.pos (state.maxPos - state.pos) state.input)
      (negative, signWidth, digits) = case remaining of
        '-' :: rest => (True, 1, rest)
        rest => (False, 0, rest)
   in case readUnsignedInteger 0 0 digits of
        Nothing => pure (Parser.Fail state.pos "signed integer")
        Just (magnitude, digitWidth) =>
          let consumed = signWidth + digitWidth
              value = if negative then negate magnitude else magnitude
           in pure (Parser.OK (Right (const value))
                ({ pos := state.pos + cast consumed } state)))

||| An arbitrary-precision signed decimal integer. Formatting is canonical;
||| parsing also accepts leading zeroes and negative zero.
public export
pSignedInteger : Pattern Integer Integer
pSignedInteger = MkPattern 0 Right signedIntegerParser show

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

export
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
namedChoice choices = choose (sortBy longerFirst (filter nonEmpty choices))
  where
    nonEmpty : (String, field) -> Bool
    nonEmpty (name, _) = name /= ""

    longerFirst : (String, field) -> (String, field) -> Ordering
    longerFirst (left, _) (right, _) =
      compare (length (unpack right)) (length (unpack left))

    choose : List (String, field) -> PatternParser field
    choose [] = Parser.fail "named field"
    choose ((name, value) :: rest) =
      (caseInsensitive name *> pure value) <|> choose rest

export
namedUpdatePart : List (String, field) -> (field -> state -> state) ->
                  PatternParser (Either PatternError (state -> state))
namedUpdatePart choices setter = map (Right . setter) (namedChoice choices)

export
namedConsumePart : List String ->
                   PatternParser (Either PatternError (state -> state))
namedConsumePart names = map (const (Right id))
  (namedChoice (map (\name => (name, ())) names))

export
spaceNumberUpdatePart : (Integer -> state -> state) ->
                        (maximumWidth : Nat) ->
                        (minimum : Integer) -> (maximum : Integer) ->
                        PatternParser (Either PatternError (state -> state))
spaceNumberUpdatePart setter maximumWidth minimum maximum = do
  ignore (Parser.optional (Parser.char ' '))
  map (map setter) (numberPart 1 maximumWidth minimum maximum)
