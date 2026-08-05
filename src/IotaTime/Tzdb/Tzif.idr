module IotaTime.Tzdb.Tzif

import public Data.Bits
import public IotaTime.DateTimeZone
import Data.List
import Derive.Prelude

%language ElabReflection

%default total

public export
data TzifVersion = Version1 | Version2 | Version3 | Version4

%runElab derive `{TzifVersion} [Eq]

public export
data TzifError
  = UnexpectedEnd
  | InvalidMagic
  | UnsupportedVersion Bits8
  | InvalidSecondHeader
  | MissingTransitionType
  | InvalidTransitionTypeIndex Integer
  | InvalidAbbreviationIndex Integer
  | UnterminatedAbbreviation Integer
  | InvalidUtcOffset Integer
  | InvalidFooter
  | TrailingData

public export
record TzifData where
  constructor MkTzifData
  version : TzifVersion
  initialTransition : TransitionInfo
  transitions : List (Instant, TransitionInfo)
  posixFooter : Maybe String

record Header where
  constructor MkHeader
  headerVersion : TzifVersion
  isGmtCount : Nat
  isStdCount : Nat
  leapCount : Nat
  transitionCount : Nat
  typeCount : Nat
  abbreviationCount : Nat

record RawType where
  constructor MkRawType
  rawOffset : Integer
  rawInDst : Bool
  rawAbbreviationIndex : Integer

takeBytes : Nat -> List Bits8 -> Either TzifError (List Bits8, List Bits8)
takeBytes Z bytes = Right ([], bytes)
takeBytes (S count) [] = Left UnexpectedEnd
takeBytes (S count) (byte :: bytes) = do
  (taken, remaining) <- takeBytes count bytes
  Right (byte :: taken, remaining)

byteValue : Bits8 -> Integer
byteValue = cast

unsignedBigEndian : List Bits8 -> Integer
unsignedBigEndian = foldl (\value, byte => value * 256 + byteValue byte) 0

signedBigEndian : Nat -> List Bits8 -> Integer
signedBigEndian width bytes =
  let unsigned = unsignedBigEndian bytes
   in let signBoundary = power 2 (pred (8 * width))
     in let modulus = power 2 (8 * width)
       in if unsigned >= signBoundary then unsigned - modulus else unsigned
  where
    power : Integer -> Nat -> Integer
    power base Z = 1
    power base (S exponent) = base * power base exponent

readUnsigned32 : List Bits8 -> Either TzifError (Integer, List Bits8)
readUnsigned32 bytes = do
  (value, remaining) <- takeBytes 4 bytes
  Right (unsignedBigEndian value, remaining)

readSigned : Nat -> List Bits8 -> Either TzifError (Integer, List Bits8)
readSigned width bytes = do
  (value, remaining) <- takeBytes width bytes
  Right (signedBigEndian width value, remaining)

readCounts : List Bits8 -> Either TzifError
  ((Nat, Nat, Nat, Nat, Nat, Nat), List Bits8)
readCounts bytes = do
  (gmt, afterGmt) <- readUnsigned32 bytes
  (standard, afterStandard) <- readUnsigned32 afterGmt
  (leaps, afterLeaps) <- readUnsigned32 afterStandard
  (transitions, afterTransitions) <- readUnsigned32 afterLeaps
  (types, afterTypes) <- readUnsigned32 afterTransitions
  (abbreviations, remaining) <- readUnsigned32 afterTypes
  Right ((cast gmt, cast standard, cast leaps, cast transitions,
    cast types, cast abbreviations), remaining)

parseVersion : Bits8 -> Either TzifError TzifVersion
parseVersion 0 = Right Version1
parseVersion 49 = Right Version1
parseVersion 50 = Right Version2
parseVersion 51 = Right Version3
parseVersion 52 = Right Version4
parseVersion value = Left (UnsupportedVersion value)

parseHeader : List Bits8 -> Either TzifError (Header, List Bits8)
parseHeader bytes = do
  (magic, afterMagic) <- takeBytes 4 bytes
  if magic /= [84, 90, 105, 102]
    then Left InvalidMagic
    else do
      (versionByte, afterVersion) <- case the (List Bits8) afterMagic of
        [] => Left UnexpectedEnd
        value :: rest => Right (value, rest)
      parsedVersion <- parseVersion versionByte
      (_, afterReserved) <- takeBytes 15 afterVersion
      ((gmt, standard, leaps, transitionValues, types, abbreviations), remaining) <-
        readCounts afterReserved
      Right (MkHeader parsedVersion gmt standard leaps transitionValues
        types abbreviations, remaining)

readManySigned : Nat -> Nat -> List Bits8 ->
                 Either TzifError (List Integer, List Bits8)
readManySigned Z width bytes = Right ([], bytes)
readManySigned (S count) width bytes = do
  (value, afterValue) <- readSigned width bytes
  (values, remaining) <- readManySigned count width afterValue
  Right (value :: values, remaining)

readManyBytes : Nat -> List Bits8 -> Either TzifError (List Integer, List Bits8)
readManyBytes count bytes = do
  (values, remaining) <- takeBytes count bytes
  Right (map byteValue values, remaining)

readRawTypes : Nat -> List Bits8 -> Either TzifError (List RawType, List Bits8)
readRawTypes Z bytes = Right ([], bytes)
readRawTypes (S count) bytes = do
  (offset, afterOffset) <- readSigned 4 bytes
  (inDst, abbreviationIndex, afterType) <-
    case the (List Bits8) afterOffset of
      dst :: index :: rest => Right (dst /= 0, byteValue index, rest)
      _ => Left UnexpectedEnd
  (types, remaining) <- readRawTypes count afterType
  Right (MkRawType offset inDst abbreviationIndex :: types, remaining)

indexList : Integer -> List value -> Maybe value
indexList index values = if index < 0 then Nothing else go (cast index) values
  where
    go : Nat -> List value -> Maybe value
    go Z (value :: _) = Just value
    go (S index) (_ :: rest) = go index rest
    go _ [] = Nothing

abbreviationAt : Integer -> List Bits8 -> Either TzifError String
abbreviationAt index bytes = case indexList index bytes of
  Nothing => Left (InvalidAbbreviationIndex index)
  Just _ => collect (drop (cast index) bytes)
  where
    collect : List Bits8 -> Either TzifError String
    collect [] = Left (UnterminatedAbbreviation index)
    collect (0 :: _) = Right ""
    collect (byte :: rest) = map (strCons (cast byte)) (collect rest)

toTransitionInfo : List Bits8 -> RawType -> Either TzifError TransitionInfo
toTransitionInfo abbreviations raw = do
  valueOffset <- case refineOffsetSeconds raw.rawOffset of
    Left _ => Left (InvalidUtcOffset raw.rawOffset)
    Right offset => Right offset
  valueAbbreviation <- abbreviationAt raw.rawAbbreviationIndex abbreviations
  Right (transitionInfo valueOffset raw.rawInDst valueAbbreviation)

chooseInitial : List (RawType, TransitionInfo) -> Either TzifError TransitionInfo
chooseInitial [] = Left MissingTransitionType
chooseInitial values@((_, first) :: _) = Right (findStandard values first)
  where
    findStandard : List (RawType, TransitionInfo) -> TransitionInfo -> TransitionInfo
    findStandard [] fallback = fallback
    findStandard ((raw, info) :: rest) fallback =
      if raw.rawInDst then findStandard rest fallback else info

buildTransitions : List Integer -> List Integer -> List TransitionInfo ->
                   Either TzifError (List (Instant, TransitionInfo))
buildTransitions [] [] types = Right []
buildTransitions (instant :: instants) (index :: indices) types = do
  info <- case indexList index types of
    Nothing => Left (InvalidTransitionTypeIndex index)
    Just value => Right value
  remaining <- buildTransitions instants indices types
  Right ((fromSecondsSinceUnixEpoch instant, info) :: remaining)
buildTransitions _ _ _ = Left UnexpectedEnd

annotateSavings : Maybe Offset -> List (Instant, TransitionInfo) ->
                  List (Instant, TransitionInfo)
annotateSavings standardOffset [] = []
annotateSavings standardOffset ((instant, info) :: rest) =
  if isDaylightSavingTime info
    then let annotated = case standardOffset of
               Nothing => info
               Just standard => transitionInfoWithSavings (utcOffset info)
                 (minusClamped (utcOffset info) standard) (abbreviation info)
          in (instant, annotated) :: annotateSavings standardOffset rest
    else let annotated = transitionInfoWithSavings
               (utcOffset info) IotaTime.Offset.empty (abbreviation info)
          in (instant, annotated) ::
               annotateSavings (Just (utcOffset info)) rest

parsePayload : Nat -> Header -> List Bits8 ->
               Either TzifError ((TransitionInfo, List (Instant, TransitionInfo)),
                 List Bits8)
parsePayload width header bytes = do
  (instants, afterInstants) <- readManySigned header.transitionCount width bytes
  (indices, afterIndices) <- readManyBytes header.transitionCount afterInstants
  (rawTypes, afterTypes) <- readRawTypes header.typeCount afterIndices
  (abbreviations, afterAbbreviations) <- takeBytes header.abbreviationCount afterTypes
  (_, remaining) <- takeBytes
    (header.leapCount * (width + 4) + header.isStdCount + header.isGmtCount)
    afterAbbreviations
  infos <- traverse (toTransitionInfo abbreviations) rawTypes
  initial <- chooseInitial (zip rawTypes infos)
  parsedTransitions <- buildTransitions instants indices infos
  let initialStandard = if isDaylightSavingTime initial
        then Nothing
        else Just (utcOffset initial)
  Right ((initial, annotateSavings initialStandard parsedTransitions), remaining)

skipPayload : Nat -> Header -> List Bits8 -> Either TzifError (List Bits8)
skipPayload width header bytes = do
  let size = header.transitionCount * width + header.transitionCount +
    header.typeCount * 6 + header.abbreviationCount +
    header.leapCount * (width + 4) + header.isStdCount + header.isGmtCount
  (_, remaining) <- takeBytes size bytes
  Right remaining

bytesToString : List Bits8 -> String
bytesToString = pack . map cast

parseFooter : List Bits8 -> Either TzifError (Maybe String)
parseFooter [] = Right Nothing
parseFooter (10 :: rest) = case reverse rest of
  10 :: reversedFooter => Right (Just (bytesToString (reverse reversedFooter)))
  _ => Left InvalidFooter
parseFooter _ = Left InvalidFooter

||| Decode a complete TZif file. POSIX future rules are retained as text and
||| are not silently approximated by the final explicit transition.
public export
parseTzif : List Bits8 -> Either TzifError TzifData
parseTzif bytes = do
  (firstHeader, afterFirstHeader) <- parseHeader bytes
  case firstHeader.headerVersion of
    Version1 => do
      ((initial, parsedTransitions), remaining) <-
        parsePayload 4 firstHeader afterFirstHeader
      case remaining of
        [] => Right (MkTzifData Version1 initial parsedTransitions Nothing)
        _ => Left TrailingData
    expectedVersion => do
      afterFirstPayload <- skipPayload 4 firstHeader afterFirstHeader
      (secondHeader, afterSecondHeader) <- parseHeader afterFirstPayload
      if secondHeader.headerVersion /= expectedVersion
        then Left InvalidSecondHeader
        else do
          ((initial, parsedTransitions), remaining) <-
            parsePayload 8 secondHeader afterSecondHeader
          footer <- parseFooter remaining
          Right (MkTzifData expectedVersion initial parsedTransitions footer)