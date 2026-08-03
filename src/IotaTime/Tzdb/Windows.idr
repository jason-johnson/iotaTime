module IotaTime.Tzdb.Windows

import public IotaTime.DateTimeZone

%default total

||| Recurring SYSTEMTIME fields used by Windows TZI registry values.
public export
record WindowsTransitionDate where
  constructor MkWindowsTransitionDate
  year : Integer
  month : Integer
  week : Integer
  weekday : Integer
  hour : Integer
  minute : Integer
  second : Integer

||| The recurring portion of a Windows REG_TZI_FORMAT value. Bias fields are
||| minutes added to local time to obtain UTC, following Windows conventions.
public export
record WindowsZoneRule where
  constructor MkWindowsZoneRule
  biasMinutes : Integer
  standardBiasMinutes : Integer
  daylightBiasMinutes : Integer
  standardName : String
  daylightName : String
  daylightStart : WindowsTransitionDate
  standardStart : WindowsTransitionDate

||| A Dynamic DST registry value effective from January 1 of its year.
public export
record WindowsDynamicRule where
  constructor MkWindowsDynamicRule
  effectiveYear : Integer
  dynamicRule : WindowsZoneRule

||| Platform-neutral data read from one Windows time-zone registry key.
public export
record WindowsRegistryZone where
  constructor MkWindowsRegistryZone
  registryZoneId : String
  registryStandardName : String
  registryDaylightName : String
  registryDefaultTzi : List Bits8
  registryDynamicTzi : List (Integer, List Bits8)

public export
data WindowsZoneError
  = WindowsOffsetOutOfRange Integer
  | WindowsTimeOutOfRange Integer Integer Integer
  | WindowsTransitionMillisecondsUnsupported Integer
  | WindowsAbsoluteTransitionUnsupported Integer
  | IncompleteWindowsDaylightRule
  | WindowsTziLength Integer
  | WindowsRecurrenceError RecurrenceRuleError

public export
data WindowsTimeZoneError
  = InvalidWindowsRule WindowsZoneError
  | InvalidWindowsTransitions DateTimeZoneError
  | DynamicYearsNotStrictlyIncreasing

public export
data WindowsRegistryError
  = InvalidDefaultTzi WindowsZoneError
  | InvalidDynamicTzi Integer WindowsZoneError
  | InvalidRegistryTimeZone WindowsTimeZoneError

byteValue : Bits8 -> Integer
byteValue = cast

unsignedLittleEndian : List Bits8 -> Integer
unsignedLittleEndian bytes = go 1 bytes
  where
    go : Integer -> List Bits8 -> Integer
    go multiplier [] = 0
    go multiplier (byte :: rest) =
      byteValue byte * multiplier + go (multiplier * 256) rest

signedLittleEndian32 : List Bits8 -> Integer
signedLittleEndian32 bytes =
  let unsigned = unsignedLittleEndian bytes
   in if unsigned >= 2147483648 then unsigned - 4294967296 else unsigned

takeBytes : Nat -> List Bits8 -> Maybe (List Bits8, List Bits8)
takeBytes Z bytes = Just ([], bytes)
takeBytes (S count) [] = Nothing
takeBytes (S count) (byte :: rest) = do
  (taken, remaining) <- takeBytes count rest
  Just (byte :: taken, remaining)

readLittleEndian : Nat -> List Bits8 -> Maybe (Integer, List Bits8)
readLittleEndian width bytes = do
  (value, remaining) <- takeBytes width bytes
  Just (unsignedLittleEndian value, remaining)

readSigned32 : List Bits8 -> Maybe (Integer, List Bits8)
readSigned32 bytes = do
  (value, remaining) <- takeBytes 4 bytes
  Just (signedLittleEndian32 value, remaining)

readTransitionDate : List Bits8 -> Maybe
  (WindowsTransitionDate, Integer, List Bits8)
readTransitionDate bytes = do
  (year, afterYear) <- readLittleEndian 2 bytes
  (month, afterMonth) <- readLittleEndian 2 afterYear
  (weekday, afterWeekday) <- readLittleEndian 2 afterMonth
  (week, afterWeek) <- readLittleEndian 2 afterWeekday
  (hour, afterHour) <- readLittleEndian 2 afterWeek
  (minute, afterMinute) <- readLittleEndian 2 afterHour
  (second, afterSecond) <- readLittleEndian 2 afterMinute
  (milliseconds, remaining) <- readLittleEndian 2 afterSecond
  Just (MkWindowsTransitionDate year month week weekday hour minute second,
    milliseconds, remaining)

||| Decode a binary Windows REG_TZI_FORMAT value. Display names are stored in
||| separate registry values and are supplied explicitly.
public export
parseWindowsTzi : String -> String -> List Bits8 ->
                  Either WindowsZoneError WindowsZoneRule
parseWindowsTzi standardName daylightName bytes =
  if length bytes /= 44
    then Left (WindowsTziLength (cast (length bytes)))
    else case decode bytes of
      Nothing => Left (WindowsTziLength (cast (length bytes)))
      Just (bias, standardBias, daylightBias,
            standardDate, standardMilliseconds,
            daylightDate, daylightMilliseconds) =>
        if standardMilliseconds /= 0
          then Left (WindowsTransitionMillisecondsUnsupported
            standardMilliseconds)
        else if daylightMilliseconds /= 0
          then Left (WindowsTransitionMillisecondsUnsupported
            daylightMilliseconds)
        else Right (MkWindowsZoneRule bias standardBias daylightBias
          standardName daylightName daylightDate standardDate)
  where
    decode : List Bits8 -> Maybe
      (Integer, Integer, Integer, WindowsTransitionDate, Integer,
       WindowsTransitionDate, Integer)
    decode source = do
      (bias, afterBias) <- readSigned32 source
      (standardBias, afterStandardBias) <- readSigned32 afterBias
      (daylightBias, afterDaylightBias) <- readSigned32 afterStandardBias
      (standardDate, standardMilliseconds, afterStandard) <-
        readTransitionDate afterDaylightBias
      (daylightDate, daylightMilliseconds, remaining) <-
        readTransitionDate afterStandard
      case remaining of
        [] => Just (bias, standardBias, daylightBias,
          standardDate, standardMilliseconds,
          daylightDate, daylightMilliseconds)
        _ => Nothing

windowsOffset : Integer -> Either WindowsZoneError Offset
windowsOffset bias =
  let seconds = negate (bias * 60)
   in case refineOffsetSeconds seconds of
        Left _ => Left (WindowsOffsetOutOfRange seconds)
        Right value => Right value

transitionSeconds : WindowsTransitionDate -> Either WindowsZoneError Integer
transitionSeconds transition =
  if transition.hour < 0 || transition.hour > 23 ||
     transition.minute < 0 || transition.minute > 59 ||
     transition.second < 0 || transition.second > 59
    then Left (WindowsTimeOutOfRange transition.hour transition.minute
      transition.second)
    else Right (transition.hour * 3600 + transition.minute * 60 +
      transition.second)

windowsRule : WindowsTransitionDate -> Either WindowsZoneError RecurrenceRule
windowsRule transition = do
  if transition.year /= 0
    then Left (WindowsAbsoluteTransitionUnsupported transition.year)
    else Right ()
  seconds <- transitionSeconds transition
  case monthWeekDayRule transition.month transition.week transition.weekday
    seconds WallTime of
      Left error => Left (WindowsRecurrenceError error)
      Right value => Right value

||| Convert one recurring Windows TZI value into shared zone recurrence rules.
export
windowsZoneRecurrence : WindowsZoneRule ->
                        Either WindowsZoneError ZoneRecurrence
windowsZoneRecurrence rule = do
  standardOffset <- windowsOffset
    (rule.biasMinutes + rule.standardBiasMinutes)
  daylightOffset <- windowsOffset
    (rule.biasMinutes + rule.daylightBiasMinutes)
  start <- windowsRule rule.daylightStart
  end <- windowsRule rule.standardStart
  Right (zoneRecurrence
    (transitionInfo standardOffset False rule.standardName)
    (transitionInfo daylightOffset True rule.daylightName)
    start end)

||| Validate Windows TZI data and construct an invariant-preserving zone.
public export
windowsRecurringTimeZone : String -> TransitionInfo ->
                           List (Instant, TransitionInfo) -> WindowsZoneRule ->
                           Either WindowsTimeZoneError TimeZone
windowsRecurringTimeZone valueId initial transitions rule = do
  recurrence <- case windowsZoneRecurrence rule of
    Left error => Left (InvalidWindowsRule error)
    Right value => Right value
  case refineRecurringDateTimeZone valueId initial transitions recurrence of
    Left error => Left (InvalidWindowsTransitions error)
    Right value => Right value

noDaylightTransitions : WindowsZoneRule -> Bool
noDaylightTransitions rule =
  rule.daylightStart.month == 0 && rule.standardStart.month == 0

incompleteDaylightTransitions : WindowsZoneRule -> Bool
incompleteDaylightTransitions rule =
  (rule.daylightStart.month == 0) /= (rule.standardStart.month == 0)

windowsZoneEra : WindowsZoneRule -> Either WindowsZoneError
  (TransitionInfo, Maybe ZoneRecurrence)
windowsZoneEra rule =
  if incompleteDaylightTransitions rule
    then Left IncompleteWindowsDaylightRule
  else do
    standardOffset <- windowsOffset
      (rule.biasMinutes + rule.standardBiasMinutes)
    let standardInfo = transitionInfo standardOffset False rule.standardName
    if noDaylightTransitions rule
      then Right (standardInfo, Nothing)
      else map (\recurrence => (standardInfo, Just recurrence))
        (windowsZoneRecurrence rule)

||| Validate a complete Windows TZI value. Month-zero transition dates describe
||| a fixed standard-offset zone; paired nonzero dates describe recurrence.
public export
windowsTimeZone : String -> WindowsZoneRule ->
                  Either WindowsTimeZoneError TimeZone
windowsTimeZone valueId rule =
  case windowsZoneEra rule of
    Left error => Left (InvalidWindowsRule error)
    Right (standardInfo, Nothing) =>
      Right (fixedDateTimeZone valueId (utcOffset standardInfo))
    Right (standardInfo, Just recurrence) =>
      case refineRecurringDateTimeZone valueId standardInfo [] recurrence of
        Left error => Left (InvalidWindowsTransitions error)
        Right value => Right value

dynamicYearsValid : List WindowsDynamicRule -> Bool
dynamicYearsValid [] = True
dynamicYearsValid (first :: rest) = go first.effectiveYear rest
  where
    go : Integer -> List WindowsDynamicRule -> Bool
    go previous [] = True
    go previous (next :: remaining) =
      previous < next.effectiveYear && go next.effectiveYear remaining

gregorianDays : Integer -> Integer -> Integer -> Integer
gregorianDays year month day =
  let shiftedYear = if month <= 2 then year - 1 else year
   in let era = shiftedYear `div` 400
     in let yearOfEra = shiftedYear - era * 400
       in let shiftedMonth = month + if month > 2 then -3 else 9
         in let dayOfYear = (153 * shiftedMonth + 2) `div` 5 + day - 1
           in let dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 -
                yearOfEra `div` 100 + dayOfYear
             in era * 146097 + dayOfEra - 730485

yearStart : Integer -> Instant
yearStart year = fromNanosecondsSinceEpoch
  (gregorianDays year 1 1 * 86400 * 1000000000)

dynamicEraSpecs : Bool -> List WindowsDynamicRule -> Either WindowsZoneError
  (List (Maybe Instant, TransitionInfo, Maybe ZoneRecurrence))
dynamicEraSpecs first [] = Right []
dynamicEraSpecs first (entry :: rest) = do
  (initial, recurrence) <- windowsZoneEra entry.dynamicRule
  remaining <- dynamicEraSpecs False rest
  let boundary = if first then Nothing else Just (yearStart entry.effectiveYear)
  Right ((boundary, initial, recurrence) :: remaining)

||| Construct a zone from Windows Dynamic DST values. When values are present,
||| the first applies without a lower bound and the last remains in force.
public export
windowsDynamicTimeZone : String -> WindowsZoneRule -> List WindowsDynamicRule ->
                         Either WindowsTimeZoneError TimeZone
windowsDynamicTimeZone valueId defaultRule [] = windowsTimeZone valueId defaultRule
windowsDynamicTimeZone valueId defaultRule dynamicRules =
  if not (dynamicYearsValid dynamicRules)
    then Left DynamicYearsNotStrictlyIncreasing
    else do
      specs <- case dynamicEraSpecs True dynamicRules of
        Left error => Left (InvalidWindowsRule error)
        Right value => Right value
      case refineTimeZoneEras valueId specs of
        Left error => Left (InvalidWindowsTransitions error)
        Right value => Right value

parseDynamicTzi : String -> String -> List (Integer, List Bits8) ->
                  Either WindowsRegistryError (List WindowsDynamicRule)
parseDynamicTzi standardName daylightName [] = Right []
parseDynamicTzi standardName daylightName ((year, bytes) :: rest) = do
  rule <- case parseWindowsTzi standardName daylightName bytes of
    Left error => Left (InvalidDynamicTzi year error)
    Right value => Right value
  remaining <- parseDynamicTzi standardName daylightName rest
  Right (MkWindowsDynamicRule year rule :: remaining)

||| Convert registry bytes captured by a native adapter into a validated zone.
public export
windowsRegistryTimeZone : WindowsRegistryZone ->
                          Either WindowsRegistryError TimeZone
windowsRegistryTimeZone registry = do
  defaultRule <- case parseWindowsTzi registry.registryStandardName
    registry.registryDaylightName registry.registryDefaultTzi of
      Left error => Left (InvalidDefaultTzi error)
      Right value => Right value
  dynamicRules <- parseDynamicTzi registry.registryStandardName
    registry.registryDaylightName registry.registryDynamicTzi
  case windowsDynamicTimeZone registry.registryZoneId defaultRule dynamicRules of
    Left error => Left (InvalidRegistryTimeZone error)
    Right value => Right value