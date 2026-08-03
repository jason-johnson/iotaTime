module IotaTime.Tzdb.Windows

import public IotaTime.DateTimeZone

%default total

||| Recurring SYSTEMTIME fields used by Windows TZI registry values.
public export
record WindowsTransitionDate where
  constructor MkWindowsTransitionDate
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