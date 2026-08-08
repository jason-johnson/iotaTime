module IotaTime.OffsetDateTime

import public IotaTime.Calendar
import public IotaTime.CalendarDateTime
import public IotaTime.Instant
import public IotaTime.LocalTime
import public IotaTime.Offset

%default total

nanosecondsPerSecond : Integer
nanosecondsPerSecond = 1000000000

nanosecondsPerDay : Integer
nanosecondsPerDay = 86400 * nanosecondsPerSecond

daysOf : {dateType : Type} -> {auto rep : HasCalendarDate dateType} ->
         dateType -> Integer
daysOf @{rep} = calendarDays @{rep}

acceptsDays : {dateType : Type} -> {auto rep : HasCalendarDate dateType} ->
              Integer -> Bool
acceptsDays @{rep} = acceptsCalendarDays @{rep}

dateFromDays : {dateType : Type} -> {auto rep : HasCalendarDate dateType} ->
               (days : Integer) -> {auto 0 valid : So (acceptsDays @{rep} days)} ->
               dateType
dateFromDays @{rep} days @{valid} = calendarDateFromDays @{rep} days @{valid}

dateTypeName : {dateType : Type} -> {auto rep : HasCalendarDate dateType} -> String
dateTypeName @{rep} = calendarDateName @{rep}

export
record OffsetDateTimeRep (calendar : Type) (cal : Calendar calendar) where
  constructor MkOffsetDateTime
  localValue : CalendarDateTime calendar @{cal}
  offsetValue : Offset

public export
OffsetDateTime : (calendar : Type) -> {auto cal : Calendar calendar} -> Type
OffsetDateTime calendar @{cal} = OffsetDateTimeRep calendar cal

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  Eq (CalendarDateTime calendar @{cal}) =>
  Eq (OffsetDateTimeRep calendar cal) where
  left == right =
    left.localValue == right.localValue && left.offsetValue == right.offsetValue

||| Associate a calendar-local date and time with its displacement from UTC.
export
atOffset : {calendar : Type} -> {auto cal : Calendar calendar} ->
           CalendarDateTime calendar @{cal} -> Offset ->
           OffsetDateTime calendar @{cal}
atOffset = MkOffsetDateTime

||| HodaTime-compatible constructor from a local calendar date-time and offset.
public export
fromCalendarDateTimeWithOffset : {calendar : Type} ->
                                 {auto cal : Calendar calendar} ->
                                 CalendarDateTime calendar @{cal} -> Offset ->
                                 OffsetDateTime calendar @{cal}
fromCalendarDateTimeWithOffset = atOffset

export
localDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                OffsetDateTime calendar @{cal} ->
                CalendarDateTime calendar @{cal}
localDateTime = localValue

public export
toCalendarDateTime : {calendar : Type} -> {auto cal : Calendar calendar} ->
                     OffsetDateTime calendar @{cal} ->
                     CalendarDateTime calendar @{cal}
toCalendarDateTime = localDateTime

export
offsetOf : {calendar : Type} -> {auto cal : Calendar calendar} ->
           OffsetDateTime calendar @{cal} -> Offset
offsetOf = offsetValue

public export
offset : {calendar : Type} -> {auto cal : Calendar calendar} ->
         OffsetDateTime calendar @{cal} -> Offset
offset = offsetOf

||| Extracting the local date-time after construction returns the supplied value.
public export
offsetDateTimeLocalPart :
  {calendar : Type} -> {auto cal : Calendar calendar} ->
  (valueDateTime : CalendarDateTime calendar @{cal}) ->
  (valueOffset : Offset) ->
  toCalendarDateTime @{cal}
    (fromCalendarDateTimeWithOffset @{cal} valueDateTime valueOffset) = valueDateTime
offsetDateTimeLocalPart _ _ = Refl

||| Extracting the offset after construction returns the supplied offset.
public export
offsetDateTimeOffsetPart :
  {calendar : Type} -> {auto cal : Calendar calendar} ->
  (valueDateTime : CalendarDateTime calendar @{cal}) ->
  (valueOffset : Offset) ->
  offset @{cal}
    (fromCalendarDateTimeWithOffset @{cal} valueDateTime valueOffset) = valueOffset
offsetDateTimeOffsetPart _ _ = Refl

||| Reconstructing an offset date-time from its projections is exact.
public export
offsetDateTimeRoundTrip :
  {calendar : Type} -> {auto cal : Calendar calendar} ->
  (value : OffsetDateTime calendar @{cal}) ->
  fromCalendarDateTimeWithOffset @{cal}
    (toCalendarDateTime @{cal} value) (offset @{cal} value) = value
offsetDateTimeRoundTrip (MkOffsetDateTime _ _) = Refl

localNanoseconds : LocalTime -> Integer
localNanoseconds value =
  (((hourValue (hour value) * 60 + minuteValue (minute value)) * 60 +
    secondValue (second value)) * nanosecondsPerSecond) +
    nanosecondValue (nanosecond value)

localTimeFromNanoseconds : Integer -> LocalTime
localTimeFromNanoseconds value = localTime
  (hourFromInteger (value `div` (3600 * nanosecondsPerSecond)))
  (minuteFromInteger (value `div` (60 * nanosecondsPerSecond) `mod` 60))
  (secondFromInteger (value `div` nanosecondsPerSecond `mod` 60))
  (nanosecondFromInteger (value `mod` nanosecondsPerSecond))

||| Resolve an offset date-time to its unique point on the global timeline.
public export
toInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
            {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
            OffsetDateTime calendar @{cal} -> Instant
toInstant @{cal} @{rep} value = fromNanosecondsSinceEpoch
  (daysOf @{rep} (datePart value.localValue) * nanosecondsPerDay +
   localNanoseconds (localTimeOfDay value.localValue) -
   totalOffsetSeconds value.offsetValue * nanosecondsPerSecond)

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  HasCalendarDate (CalendarDate calendar @{cal}) =>
  Eq (CalendarDate calendar @{cal}) =>
  Ord (OffsetDateTimeRep calendar cal) where
  compare left right = case compare
    (toInstant left) (toInstant right) of
      EQ => compare left.offsetValue right.offsetValue
      ordering => ordering

public export
{calendar : Type} -> {cal : Calendar calendar} ->
  HasCalendarDate (CalendarDate calendar @{cal}) =>
  Show (OffsetDateTimeRep calendar cal) where
  show value = "fromInstantWithOffset (" ++
    show (toInstant value) ++ ") (" ++
    show value.offsetValue ++ ")"

||| Display an instant using a calendar and offset. Conversion can fail only
||| when the resulting local day lies outside the calendar's supported range.
export
fromInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
              {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
              Offset -> Instant ->
              Either CalendarConversionError (OffsetDateTime calendar @{cal})
fromInstant @{cal} @{rep} valueOffset valueInstant =
  let localNanos = toNanosecondsSinceEpoch valueInstant +
        totalOffsetSeconds valueOffset * nanosecondsPerSecond
      valueDays = localNanos `div` nanosecondsPerDay
      nanosWithinDay = localNanos `mod` nanosecondsPerDay
   in case choose (acceptsDays @{rep} valueDays) of
        Left valid => Right (MkOffsetDateTime
          (on (localTimeFromNanoseconds nanosWithinDay)
            (dateFromDays @{rep} valueDays @{valid}))
          valueOffset)
        Right _ => Left
          (TargetCalendarOutOfRange (dateTypeName @{rep}) valueDays)

||| HodaTime-compatible constructor with instant-first argument order.
public export
fromInstantWithOffset : {calendar : Type} -> {auto cal : Calendar calendar} ->
                        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                        Instant -> Offset ->
                        Either CalendarConversionError
                          (OffsetDateTime calendar @{cal})
fromInstantWithOffset valueInstant valueOffset =
  fromInstant valueOffset valueInstant

||| Change the displayed offset while preserving the represented instant.
public export
withOffset : {calendar : Type} -> {auto cal : Calendar calendar} ->
             {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
             Offset -> OffsetDateTime calendar @{cal} ->
             Either CalendarConversionError (OffsetDateTime calendar @{cal})
withOffset valueOffset value = fromInstant valueOffset (toInstant value)

||| Change the calendar while preserving the local time, offset, and instant.
public export
withCalendar : {source : Type} -> {target : Type} ->
               {auto sourceCal : Calendar source} ->
               {auto targetCal : Calendar target} ->
               {auto sourceRep : HasCalendarDate (CalendarDate source @{sourceCal})} ->
               {auto targetRep : HasCalendarDate (CalendarDate target @{targetCal})} ->
               OffsetDateTime source @{sourceCal} ->
               Either CalendarConversionError (OffsetDateTime target @{targetCal})
withCalendar @{sourceCal} @{targetCal} @{sourceRep} @{targetRep} value =
  case IotaTime.CalendarDateTime.withCalendar
    @{sourceCal} @{targetCal} @{sourceRep} @{targetRep} value.localValue of
      Left error => Left error
      Right converted => Right (MkOffsetDateTime converted value.offsetValue)
