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

public export
data WindowsZoneError
  = WindowsOffsetOutOfRange Integer
  | WindowsTimeOutOfRange Integer Integer Integer
  | WindowsAbsoluteTransitionUnsupported Integer
  | IncompleteWindowsDaylightRule
  | WindowsRecurrenceError RecurrenceRuleError

public export
data WindowsTimeZoneError
  = InvalidWindowsRule WindowsZoneError
  | InvalidWindowsTransitions DateTimeZoneError

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

||| Validate a complete Windows TZI value. Month-zero transition dates describe
||| a fixed standard-offset zone; paired nonzero dates describe recurrence.
public export
windowsTimeZone : String -> WindowsZoneRule ->
                  Either WindowsTimeZoneError TimeZone
windowsTimeZone valueId rule =
  if incompleteDaylightTransitions rule
    then Left (InvalidWindowsRule IncompleteWindowsDaylightRule)
  else if noDaylightTransitions rule
    then case windowsOffset (rule.biasMinutes + rule.standardBiasMinutes) of
      Left error => Left (InvalidWindowsRule error)
      Right offset => Right (fixedDateTimeZone valueId offset)
    else do
      initialOffset <- case windowsOffset
        (rule.biasMinutes + rule.standardBiasMinutes) of
          Left error => Left (InvalidWindowsRule error)
          Right value => Right value
      windowsRecurringTimeZone valueId
        (transitionInfo initialOffset False rule.standardName) [] rule