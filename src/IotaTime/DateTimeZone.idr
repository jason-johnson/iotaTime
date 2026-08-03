module IotaTime.DateTimeZone

import public Data.So
import public IotaTime.Instant
import public IotaTime.Offset
import public IotaTime.OffsetDateTime

%default total

export
record TransitionInfo where
  constructor MkTransitionInfo
  storedUtcOffset : Offset
  storedInDst : Bool
  storedAbbreviation : String

||| Describe the zone state effective over a timeline segment.
public export
transitionInfo : Offset -> Bool -> String -> TransitionInfo
transitionInfo = MkTransitionInfo

public export
utcOffset : TransitionInfo -> Offset
utcOffset = storedUtcOffset

public export
isDaylightSavingTime : TransitionInfo -> Bool
isDaylightSavingTime = storedInDst

public export
abbreviation : TransitionInfo -> String
abbreviation = storedAbbreviation

export
record ZoneTransition where
  constructor MkZoneTransition
  transitionInstant : Instant
  transitionInfo : TransitionInfo

public export
data TransitionTimeMode = WallTime | StandardTime | UniversalTime

export
data RecurrenceDay
  = JulianWithoutLeap Integer
  | JulianWithLeap Integer
  | MonthWeekDay Integer Integer Integer

public export
data RecurrenceRuleError
  = JulianDayOutOfRange Integer
  | MonthOutOfRange Integer
  | WeekOutOfRange Integer
  | WeekdayOutOfRange Integer

export
record RecurrenceRule where
  constructor MkRecurrenceRule
  recurrenceDay : RecurrenceDay
  recurrenceSeconds : Integer
  recurrenceMode : TransitionTimeMode

||| Validate a one-based Julian day that omits February 29.
public export
julianWithoutLeapRule : Integer -> Integer -> TransitionTimeMode ->
                         Either RecurrenceRuleError RecurrenceRule
julianWithoutLeapRule day seconds mode =
  if day >= 1 && day <= 365
    then Right (MkRecurrenceRule (JulianWithoutLeap day) seconds mode)
    else Left (JulianDayOutOfRange day)

||| Validate a zero-based Julian day that includes February 29.
public export
julianWithLeapRule : Integer -> Integer -> TransitionTimeMode ->
                      Either RecurrenceRuleError RecurrenceRule
julianWithLeapRule day seconds mode =
  if day >= 0 && day <= 365
    then Right (MkRecurrenceRule (JulianWithLeap day) seconds mode)
    else Left (JulianDayOutOfRange day)

||| Validate an Mm.w.d POSIX transition day.
public export
monthWeekDayRule : Integer -> Integer -> Integer -> Integer ->
                   TransitionTimeMode -> Either RecurrenceRuleError RecurrenceRule
monthWeekDayRule month week weekday seconds mode =
  if month < 1 || month > 12 then Left (MonthOutOfRange month)
  else if week < 1 || week > 5 then Left (WeekOutOfRange week)
  else if weekday < 0 || weekday > 6 then Left (WeekdayOutOfRange weekday)
  else Right (MkRecurrenceRule (MonthWeekDay month week weekday) seconds mode)

export
record ZoneRecurrence where
  constructor MkZoneRecurrence
  standardTransition : TransitionInfo
  daylightTransition : TransitionInfo
  daylightStart : RecurrenceRule
  standardStart : RecurrenceRule

||| Construct recurring standard/daylight rules from validated transition days.
public export
zoneRecurrence : TransitionInfo -> TransitionInfo -> RecurrenceRule ->
                 RecurrenceRule -> ZoneRecurrence
zoneRecurrence = MkZoneRecurrence

export
record RecurrenceEra where
  constructor MkRecurrenceEra
  eraStart : Maybe Instant
  eraInitialTransition : TransitionInfo
  eraRecurrence : Maybe ZoneRecurrence

export
record DateTimeZoneRep where
  constructor MkDateTimeZone
  storedZoneId : String
  initialTransition : TransitionInfo
  transitions : List ZoneTransition
  recurrenceEras : List RecurrenceEra

public export
DateTimeZone : Type
DateTimeZone = DateTimeZoneRep

||| HodaTime-compatible name for a date-time zone.
public export
TimeZone : Type
TimeZone = DateTimeZone

public export
areZoneTransitionsAfter : Integer -> List (Integer, TransitionInfo) -> Bool
areZoneTransitionsAfter previous [] = True
areZoneTransitionsAfter previous ((instant, _) :: rest) =
  previous < instant && areZoneTransitionsAfter instant rest

||| Whether transition instants are strictly increasing.
public export
isValidZoneTransitions : List (Integer, TransitionInfo) -> Bool
isValidZoneTransitions [] = True
isValidZoneTransitions ((instant, _) :: rest) =
  areZoneTransitionsAfter instant rest

toTransitions : List (Integer, TransitionInfo) -> List ZoneTransition
toTransitions [] = []
toTransitions ((instant, valueInfo) :: rest) =
  MkZoneTransition (fromNanosecondsSinceEpoch instant) valueInfo ::
  toTransitions rest

||| Construct a fixed-offset zone.
public export
fixedDateTimeZone : String -> Offset -> DateTimeZone
fixedDateTimeZone valueId valueOffset =
  MkDateTimeZone valueId (transitionInfo valueOffset False valueId) [] []

||| Construct a transition zone from statically known, strictly increasing
||| nanosecond instants and the offsets effective from those instants onward.
public export
dateTimeZone : (valueId : String) -> (valueInitialInfo : TransitionInfo) ->
               (valueTransitions : List (Integer, TransitionInfo)) ->
               {auto 0 valid : So (isValidZoneTransitions valueTransitions)} ->
               DateTimeZone
dateTimeZone valueId valueInitialInfo valueTransitions =
  MkDateTimeZone valueId valueInitialInfo (toTransitions valueTransitions) []

public export
data DateTimeZoneError
  = TransitionsNotStrictlyIncreasing
  | RecurrenceErasNotStrictlyIncreasing
  | MissingRecurrenceEra

runtimeTransitionsValid : List (Instant, TransitionInfo) -> Bool
runtimeTransitionsValid [] = True
runtimeTransitionsValid ((instant, _) :: rest) = go instant rest
  where
    go : Instant -> List (Instant, TransitionInfo) -> Bool
    go previous [] = True
    go previous ((next, _) :: remaining) =
      previous < next && go next remaining

toRuntimeTransitions : List (Instant, TransitionInfo) -> List ZoneTransition
toRuntimeTransitions [] = []
toRuntimeTransitions ((instant, valueInfo) :: rest) =
  MkZoneTransition instant valueInfo :: toRuntimeTransitions rest

||| Validate transition data learned at runtime.
public export
refineDateTimeZone : String -> TransitionInfo -> List (Instant, TransitionInfo) ->
                     Either DateTimeZoneError DateTimeZone
refineDateTimeZone valueId valueInitialInfo valueTransitions =
  if runtimeTransitionsValid valueTransitions
    then Right (MkDateTimeZone valueId valueInitialInfo
      (toRuntimeTransitions valueTransitions) [])
    else Left TransitionsNotStrictlyIncreasing

||| Validate explicit transitions and attach recurring rules used after them.
public export
refineRecurringDateTimeZone : String -> TransitionInfo ->
                              List (Instant, TransitionInfo) -> ZoneRecurrence ->
                              Either DateTimeZoneError DateTimeZone
refineRecurringDateTimeZone valueId valueInitialInfo valueTransitions recurrence =
  if runtimeTransitionsValid valueTransitions
    then let (boundary, initial) = finalExplicit valueInitialInfo valueTransitions
          in Right (MkDateTimeZone valueId valueInitialInfo
            (toRuntimeTransitions valueTransitions)
            [MkRecurrenceEra boundary initial (Just recurrence)])
    else Left TransitionsNotStrictlyIncreasing
  where
    finalExplicit : TransitionInfo -> List (Instant, TransitionInfo) ->
                    (Maybe Instant, TransitionInfo)
    finalExplicit initial [] = (Nothing, initial)
    finalExplicit initial ((instant, info) :: rest) = go instant info rest
      where
        go : Instant -> TransitionInfo -> List (Instant, TransitionInfo) ->
             (Maybe Instant, TransitionInfo)
        go instant info [] = (Just instant, info)
        go instant info ((next, nextInfo) :: remaining) =
          go next nextInfo remaining

public export
zoneId : DateTimeZone -> String
zoneId = storedZoneId

recurrenceNanosecondsPerSecond : Integer
recurrenceNanosecondsPerSecond = 1000000000

recurrenceSecondsPerDay : Integer
recurrenceSecondsPerDay = 86400

isGregorianLeapYear : Integer -> Bool
isGregorianLeapYear year =
  year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)

daysFromGregorianCivil : Integer -> Integer -> Integer -> Integer
daysFromGregorianCivil year month day =
  let shiftedYear = if month <= 2 then year - 1 else year
   in let era = shiftedYear `div` 400
     in let yearOfEra = shiftedYear - era * 400
       in let shiftedMonth = month + if month > 2 then -3 else 9
         in let dayOfYear = (153 * shiftedMonth + 2) `div` 5 + day - 1
           in let dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 -
                yearOfEra `div` 100 + dayOfYear
             in era * 146097 + dayOfEra - 730485

gregorianYearFromDays : Integer -> Integer
gregorianYearFromDays days =
  let shifted = days + 730485
   in let era = shifted `div` 146097
     in let dayOfEra = shifted - era * 146097
       in let yearOfEra = (dayOfEra - dayOfEra `div` 1460 +
            dayOfEra `div` 36524 - dayOfEra `div` 146096) `div` 365
         in let partialYear = yearOfEra + era * 400
           in let dayOfYear = dayOfEra - (365 * yearOfEra +
                yearOfEra `div` 4 - yearOfEra `div` 100)
             in let shiftedMonth = (5 * dayOfYear + 2) `div` 153
               in let month = shiftedMonth + if shiftedMonth < 10 then 3 else -9
                 in partialYear + if month <= 2 then 1 else 0

daysInGregorianMonth : Integer -> Integer -> Integer
daysInGregorianMonth year 2 = if isGregorianLeapYear year then 29 else 28
daysInGregorianMonth year month =
  if month == 4 || month == 6 || month == 9 || month == 11 then 30 else 31

recurrenceDayInYear : Integer -> RecurrenceDay -> Integer
recurrenceDayInYear year (JulianWithoutLeap day) =
  daysFromGregorianCivil year 1 1 + day - 1 +
    if isGregorianLeapYear year && day >= 60 then 1 else 0
recurrenceDayInYear year (JulianWithLeap day) =
  daysFromGregorianCivil year 1 1 + day
recurrenceDayInYear year (MonthWeekDay month week weekday) =
  let first = daysFromGregorianCivil year month 1
   in let firstWeekday = (first + 3) `mod` 7
     in let candidate = first + (weekday - firstWeekday) `mod` 7 + 7 * (week - 1)
       in if candidate >= first + daysInGregorianMonth year month
            then candidate - 7
            else candidate

transitionAdjustment : ZoneRecurrence -> TransitionInfo ->
                       TransitionTimeMode -> Integer
transitionAdjustment recurrence before UniversalTime = 0
transitionAdjustment recurrence before StandardTime =
  totalOffsetSeconds (utcOffset recurrence.standardTransition)
transitionAdjustment recurrence before WallTime =
  totalOffsetSeconds (utcOffset before)

ruleInstant : ZoneRecurrence -> Integer -> TransitionInfo -> RecurrenceRule -> Instant
ruleInstant recurrence year before rule =
  let day = recurrenceDayInYear year rule.recurrenceDay
   in let adjustment = transitionAdjustment recurrence before rule.recurrenceMode
     in fromNanosecondsSinceEpoch
          ((day * recurrenceSecondsPerDay + rule.recurrenceSeconds - adjustment) *
            recurrenceNanosecondsPerSecond)

recurrenceEvents : ZoneRecurrence -> Integer -> List (Instant, TransitionInfo)
recurrenceEvents recurrence year =
  order
    (ruleInstant recurrence year recurrence.standardTransition
      recurrence.daylightStart, recurrence.daylightTransition)
    (ruleInstant recurrence year recurrence.daylightTransition
      recurrence.standardStart, recurrence.standardTransition)
  where
    order : (Instant, TransitionInfo) -> (Instant, TransitionInfo) ->
            List (Instant, TransitionInfo)
    order first@(firstInstant, _) second@(secondInstant, _) =
      if firstInstant <= secondInstant then [first, second] else [second, first]

recurringTransitionAt : ZoneRecurrence -> Maybe Instant -> TransitionInfo ->
                        Instant -> TransitionInfo
recurringTransitionAt recurrence cutoff initial query =
  chooseLatest cutoff initial events
  where
    queryDay = toNanosecondsSinceEpoch query `div`
      (recurrenceSecondsPerDay * recurrenceNanosecondsPerSecond)
    queryYear = gregorianYearFromDays queryDay
    events = recurrenceEvents recurrence (queryYear - 1) ++
      recurrenceEvents recurrence queryYear ++
      recurrenceEvents recurrence (queryYear + 1)

    afterCutoff : Maybe Instant -> Instant -> Bool
    afterCutoff Nothing event = True
    afterCutoff (Just boundary) event = event > boundary

    chooseLatest : Maybe Instant -> TransitionInfo ->
                   List (Instant, TransitionInfo) -> TransitionInfo
    chooseLatest boundary current [] = current
    chooseLatest boundary current ((event, info) :: rest) =
      if afterCutoff boundary event && event <= query
        then chooseLatest (Just event) info rest
        else chooseLatest boundary current rest

eraInitial : Maybe Instant -> ZoneRecurrence -> TransitionInfo
eraInitial Nothing recurrence = recurrence.standardTransition
eraInitial (Just start) recurrence = recurringTransitionAt recurrence Nothing
  recurrence.standardTransition start

eraSpecsValid : List (Maybe Instant, ZoneRecurrence) -> Bool
eraSpecsValid [] = False
eraSpecsValid ((start, _) :: rest) = go start rest
  where
    go : Maybe Instant -> List (Maybe Instant, ZoneRecurrence) -> Bool
    go previous [] = True
    go previous ((next, _) :: remaining) = case (previous, next) of
      (_, Nothing) => False
      (Nothing, Just next) => go (Just next) remaining
      (Just previous, Just next) =>
        previous < next && go (Just next) remaining

toRecurrenceEras : List (Maybe Instant, ZoneRecurrence) -> List RecurrenceEra
toRecurrenceEras [] = []
toRecurrenceEras ((start, recurrence) :: rest) =
  MkRecurrenceEra start (eraInitial start recurrence) (Just recurrence) ::
  toRecurrenceEras rest

zoneEraSpecsValid : List (Maybe Instant, TransitionInfo, Maybe ZoneRecurrence) -> Bool
zoneEraSpecsValid [] = False
zoneEraSpecsValid ((start, _, _) :: rest) = go start rest
  where
    go : Maybe Instant ->
         List (Maybe Instant, TransitionInfo, Maybe ZoneRecurrence) -> Bool
    go previous [] = True
    go previous ((next, _, _) :: remaining) = case (previous, next) of
      (_, Nothing) => False
      (Nothing, Just next) => go (Just next) remaining
      (Just previous, Just next) =>
        previous < next && go (Just next) remaining

toZoneEras : List (Maybe Instant, TransitionInfo, Maybe ZoneRecurrence) ->
             List RecurrenceEra
toZoneEras [] = []
toZoneEras ((start, initial, recurrence) :: rest) =
  MkRecurrenceEra start computed recurrence :: toZoneEras rest
  where
    computed : TransitionInfo
    computed = case recurrence of
      Nothing => initial
      Just value => case start of
        Nothing => initial
        Just boundary => recurringTransitionAt value Nothing initial boundary

||| Validate ordered fixed or recurring eras for a platform adapter.
export
refineTimeZoneEras : String ->
  List (Maybe Instant, TransitionInfo, Maybe ZoneRecurrence) ->
  Either DateTimeZoneError TimeZone
refineTimeZoneEras valueId specs =
  if zoneEraSpecsValid specs
    then case toZoneEras specs of
      [] => Left MissingRecurrenceEra
      first :: eras => Right (MkDateTimeZone valueId
        first.eraInitialTransition [] (first :: eras))
    else case specs of
      [] => Left MissingRecurrenceEra
      _ => Left RecurrenceErasNotStrictlyIncreasing

||| Validate ordered recurrence eras. An initial `Nothing` boundary applies
||| without a lower timeline bound; subsequent boundaries must increase.
export
refineRecurrenceErasDateTimeZone : String ->
  List (Maybe Instant, ZoneRecurrence) -> Either DateTimeZoneError TimeZone
refineRecurrenceErasDateTimeZone valueId specs =
  if eraSpecsValid specs
    then case toRecurrenceEras specs of
      [] => Left MissingRecurrenceEra
      first :: eras => Right (MkDateTimeZone valueId
        first.eraInitialTransition [] (first :: eras))
    else case specs of
      [] => Left MissingRecurrenceEra
      _ => Left RecurrenceErasNotStrictlyIncreasing

activeRecurrenceEra : List RecurrenceEra -> TransitionInfo -> Instant ->
                      TransitionInfo
activeRecurrenceEra eras fallback query = case select Nothing eras of
  Nothing => fallback
  Just era => case era.eraRecurrence of
    Nothing => era.eraInitialTransition
    Just recurrence => recurringTransitionAt recurrence era.eraStart
      era.eraInitialTransition query
  where
    startsBy : Maybe Instant -> Instant -> Bool
    startsBy Nothing query = True
    startsBy (Just start) query = start <= query

    select : Maybe RecurrenceEra -> List RecurrenceEra -> Maybe RecurrenceEra
    select selected [] = selected
    select selected (era :: rest) =
      if startsBy era.eraStart query
        then select (Just era) rest
        else selected

public export
activeTransitionAt : TimeZone -> Instant -> TransitionInfo
activeTransitionAt valueZone valueInstant = go
  valueZone.initialTransition valueZone.transitions
  where
    go : TransitionInfo -> List ZoneTransition -> TransitionInfo
    go current [] = activeRecurrenceEra valueZone.recurrenceEras current valueInstant
    go current (transition :: rest) =
      if valueInstant < transition.transitionInstant
        then current
        else go transition.transitionInfo rest

public export
zoneOffsetAt : TimeZone -> Instant -> Offset
zoneOffsetAt valueZone = utcOffset . activeTransitionAt valueZone

addUnique : Offset -> List Offset -> List Offset
addUnique value [] = [value]
addUnique value (current :: rest) =
  if value == current then current :: rest
  else current :: addUnique value rest

zoneOffsets : DateTimeZone -> List Offset
zoneOffsets valueZone =
  recurrenceOffsets valueZone.recurrenceEras valueZone.transitions
  where
    go : List Offset -> List ZoneTransition -> List Offset
    go values [] = values
    go values (transition :: rest) =
      go (addUnique (utcOffset transition.transitionInfo) values) rest

    addEraOffsets : List Offset -> List RecurrenceEra -> List Offset
    addEraOffsets offsets [] = offsets
    addEraOffsets offsets (era :: eras) = case era.eraRecurrence of
      Nothing => addEraOffsets
        (addUnique (utcOffset era.eraInitialTransition) offsets) eras
      Just recurrence => addEraOffsets
        (addUnique (utcOffset recurrence.daylightTransition)
          (addUnique (utcOffset recurrence.standardTransition) offsets)) eras

    recurrenceOffsets : List RecurrenceEra -> List ZoneTransition -> List Offset
    recurrenceOffsets [] transitions =
      go [utcOffset valueZone.initialTransition] transitions
    recurrenceOffsets eras transitions =
      go (addEraOffsets [utcOffset valueZone.initialTransition] eras) transitions

insertByInstant : {calendar : Type} -> {auto cal : Calendar calendar} ->
                  {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                  OffsetDateTime calendar @{cal} ->
                  List (OffsetDateTime calendar @{cal}) ->
                  List (OffsetDateTime calendar @{cal})
insertByInstant value [] = [value]
insertByInstant value (current :: rest) =
  if toInstant value <= toInstant current
    then value :: current :: rest
    else current :: insertByInstant value rest

mappingCandidates : {calendar : Type} -> {auto cal : Calendar calendar} ->
                    {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                    DateTimeZone -> CalendarDateTime calendar @{cal} ->
                    List (OffsetDateTime calendar @{cal})
mappingCandidates valueZone local = go (zoneOffsets valueZone)
  where
    go : List Offset -> List (OffsetDateTime calendar @{cal})
    go [] = []
    go (valueOffset :: rest) =
      let candidate = atOffset local valueOffset
          remaining = go rest
       in if zoneOffsetAt valueZone (toInstant candidate) == valueOffset
            then insertByInstant candidate remaining
            else remaining

nanosecondsPerSecond : Integer
nanosecondsPerSecond = 1000000000

findLenientGapMapping : {calendar : Type} -> {auto cal : Calendar calendar} ->
                        {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                        CalendarDateTime calendar @{cal} -> TransitionInfo ->
                        List ZoneTransition ->
                        Either CalendarConversionError
                          (Maybe (OffsetDateTime calendar @{cal}))
findLenientGapMapping local current [] = Right Nothing
findLenientGapMapping local current (transition :: rest) =
  let beforeOffset = utcOffset current
      afterOffset = utcOffset transition.transitionInfo
      beforeSeconds = totalOffsetSeconds beforeOffset
      afterSeconds = totalOffsetSeconds afterOffset
      candidate = atOffset local beforeOffset
      candidateInstant = toInstant candidate
      transitionNanos = toNanosecondsSinceEpoch transition.transitionInstant
      gapNanos = (afterSeconds - beforeSeconds) * nanosecondsPerSecond
      candidateNanos = toNanosecondsSinceEpoch candidateInstant
   in if afterSeconds > beforeSeconds &&
         candidateNanos >= transitionNanos &&
         candidateNanos < transitionNanos + gapNanos
        then map Just shifted
        else findLenientGapMapping local transition.transitionInfo rest
  where
    shifted : Either CalendarConversionError (OffsetDateTime calendar @{cal})
    shifted = IotaTime.OffsetDateTime.fromInstant
      (utcOffset transition.transitionInfo) (toInstant (atOffset local (utcOffset current)))

findLenientGapByOffsets : {calendar : Type} -> {auto cal : Calendar calendar} ->
                          {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                          TimeZone -> CalendarDateTime calendar @{cal} -> List Offset ->
                          Either CalendarConversionError
                            (Maybe (OffsetDateTime calendar @{cal}))
findLenientGapByOffsets valueZone local [] = Right Nothing
findLenientGapByOffsets valueZone local (before :: rest) =
  let candidate = atOffset local before
      candidateInstant = toInstant candidate
      after = zoneOffsetAt valueZone candidateInstant
   in if totalOffsetSeconds after > totalOffsetSeconds before
        then map Just (IotaTime.OffsetDateTime.fromInstant after candidateInstant)
        else findLenientGapByOffsets valueZone local rest

export
lenientLocalMapping : {calendar : Type} -> {auto cal : Calendar calendar} ->
                      {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                      TimeZone -> CalendarDateTime calendar @{cal} ->
                      Either CalendarConversionError
                        (Maybe (OffsetDateTime calendar @{cal}))
lenientLocalMapping valueZone local = case mappingCandidates valueZone local of
  [] => do
    explicit <- findLenientGapMapping local valueZone.initialTransition
      valueZone.transitions
    case explicit of
      Just value => Right (Just value)
      Nothing => findLenientGapByOffsets valueZone local (zoneOffsets valueZone)
  first :: _ => Right (Just first)

||| The complete result of mapping one local date-time into a zone. The
||| ambiguous case retains every valid instant, including pathological zone
||| data that creates more than the usual two candidates.
public export
data LocalMapping : (calendar : Type) ->
                    (cal : Calendar calendar) -> Type where
  Skipped : LocalMapping calendar cal
  Unambiguous : OffsetDateTime calendar @{cal} -> LocalMapping calendar cal
  Ambiguous : (earliest : OffsetDateTime calendar @{cal}) ->
              (next : OffsetDateTime calendar @{cal}) ->
              (additional : List (OffsetDateTime calendar @{cal})) ->
              LocalMapping calendar cal

public export
mapLocal : {calendar : Type} -> {auto cal : Calendar calendar} ->
           {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
           DateTimeZone -> CalendarDateTime calendar @{cal} ->
           LocalMapping calendar cal
mapLocal valueZone local = case mappingCandidates valueZone local of
  [] => Skipped
  [value] => Unambiguous value
  first :: second :: rest => Ambiguous first second rest
