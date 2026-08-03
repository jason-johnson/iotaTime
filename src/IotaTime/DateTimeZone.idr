module IotaTime.DateTimeZone

import public Data.So
import public IotaTime.Instant
import public IotaTime.Offset
import public IotaTime.OffsetDateTime

%default total

export
record ZoneTransition where
  constructor MkZoneTransition
  transitionInstant : Instant
  transitionOffset : Offset

export
record DateTimeZoneRep where
  constructor MkDateTimeZone
  storedZoneId : String
  initialOffset : Offset
  transitions : List ZoneTransition

public export
DateTimeZone : Type
DateTimeZone = DateTimeZoneRep

public export
areZoneTransitionsAfter : Integer -> List (Integer, Offset) -> Bool
areZoneTransitionsAfter previous [] = True
areZoneTransitionsAfter previous ((instant, _) :: rest) =
  previous < instant && areZoneTransitionsAfter instant rest

||| Whether transition instants are strictly increasing.
public export
isValidZoneTransitions : List (Integer, Offset) -> Bool
isValidZoneTransitions [] = True
isValidZoneTransitions ((instant, _) :: rest) =
  areZoneTransitionsAfter instant rest

toTransitions : List (Integer, Offset) -> List ZoneTransition
toTransitions [] = []
toTransitions ((instant, valueOffset) :: rest) =
  MkZoneTransition (fromNanosecondsSinceEpoch instant) valueOffset ::
  toTransitions rest

||| Construct a fixed-offset zone.
public export
fixedDateTimeZone : String -> Offset -> DateTimeZone
fixedDateTimeZone valueId valueOffset =
  MkDateTimeZone valueId valueOffset []

||| Construct a transition zone from statically known, strictly increasing
||| nanosecond instants and the offsets effective from those instants onward.
public export
dateTimeZone : (valueId : String) -> (valueInitialOffset : Offset) ->
               (valueTransitions : List (Integer, Offset)) ->
               {auto 0 valid : So (isValidZoneTransitions valueTransitions)} ->
               DateTimeZone
dateTimeZone valueId valueInitialOffset valueTransitions =
  MkDateTimeZone valueId valueInitialOffset (toTransitions valueTransitions)

public export
data DateTimeZoneError = TransitionsNotStrictlyIncreasing

runtimeTransitionsValid : List (Instant, Offset) -> Bool
runtimeTransitionsValid [] = True
runtimeTransitionsValid ((instant, _) :: rest) = go instant rest
  where
    go : Instant -> List (Instant, Offset) -> Bool
    go previous [] = True
    go previous ((next, _) :: remaining) =
      previous < next && go next remaining

toRuntimeTransitions : List (Instant, Offset) -> List ZoneTransition
toRuntimeTransitions [] = []
toRuntimeTransitions ((instant, valueOffset) :: rest) =
  MkZoneTransition instant valueOffset :: toRuntimeTransitions rest

||| Validate transition data learned at runtime.
public export
refineDateTimeZone : String -> Offset -> List (Instant, Offset) ->
                     Either DateTimeZoneError DateTimeZone
refineDateTimeZone valueId valueInitialOffset valueTransitions =
  if runtimeTransitionsValid valueTransitions
    then Right (MkDateTimeZone valueId valueInitialOffset
      (toRuntimeTransitions valueTransitions))
    else Left TransitionsNotStrictlyIncreasing

public export
zoneId : DateTimeZone -> String
zoneId = storedZoneId

public export
zoneOffsetAt : DateTimeZone -> Instant -> Offset
zoneOffsetAt valueZone valueInstant = go
  valueZone.initialOffset valueZone.transitions
  where
    go : Offset -> List ZoneTransition -> Offset
    go current [] = current
    go current (transition :: rest) =
      if valueInstant < transition.transitionInstant
        then current
        else go transition.transitionOffset rest

addUnique : Offset -> List Offset -> List Offset
addUnique value [] = [value]
addUnique value (current :: rest) =
  if value == current then current :: rest
  else current :: addUnique value rest

zoneOffsets : DateTimeZone -> List Offset
zoneOffsets valueZone = go [valueZone.initialOffset] valueZone.transitions
  where
    go : List Offset -> List ZoneTransition -> List Offset
    go values [] = values
    go values (transition :: rest) =
      go (addUnique transition.transitionOffset values) rest

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
