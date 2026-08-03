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

export
record DateTimeZoneRep where
  constructor MkDateTimeZone
  storedZoneId : String
  initialTransition : TransitionInfo
  transitions : List ZoneTransition

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
  MkDateTimeZone valueId (transitionInfo valueOffset False valueId) []

||| Construct a transition zone from statically known, strictly increasing
||| nanosecond instants and the offsets effective from those instants onward.
public export
dateTimeZone : (valueId : String) -> (valueInitialInfo : TransitionInfo) ->
               (valueTransitions : List (Integer, TransitionInfo)) ->
               {auto 0 valid : So (isValidZoneTransitions valueTransitions)} ->
               DateTimeZone
dateTimeZone valueId valueInitialInfo valueTransitions =
  MkDateTimeZone valueId valueInitialInfo (toTransitions valueTransitions)

public export
data DateTimeZoneError = TransitionsNotStrictlyIncreasing

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
      (toRuntimeTransitions valueTransitions))
    else Left TransitionsNotStrictlyIncreasing

public export
zoneId : DateTimeZone -> String
zoneId = storedZoneId

public export
activeTransitionAt : TimeZone -> Instant -> TransitionInfo
activeTransitionAt valueZone valueInstant = go
  valueZone.initialTransition valueZone.transitions
  where
    go : TransitionInfo -> List ZoneTransition -> TransitionInfo
    go current [] = current
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
zoneOffsets valueZone = go
  [utcOffset valueZone.initialTransition] valueZone.transitions
  where
    go : List Offset -> List ZoneTransition -> List Offset
    go values [] = values
    go values (transition :: rest) =
      go (addUnique (utcOffset transition.transitionInfo) values) rest

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

export
lenientLocalMapping : {calendar : Type} -> {auto cal : Calendar calendar} ->
                      {auto rep : HasCalendarDate (CalendarDate calendar @{cal})} ->
                      TimeZone -> CalendarDateTime calendar @{cal} ->
                      Either CalendarConversionError
                        (Maybe (OffsetDateTime calendar @{cal}))
lenientLocalMapping valueZone local = case mappingCandidates valueZone local of
  [] => findLenientGapMapping local valueZone.initialTransition
    valueZone.transitions
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
