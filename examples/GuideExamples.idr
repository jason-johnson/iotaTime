module GuideExamples

import IotaTime

leapDay : CalendarDate Gregorian
leapDay = calendarDate 29 February 2020

runtimeLeapDay : Either GregorianDateError (CalendarDate Gregorian)
runtimeLeapDay = refineDate 29 February 2020

start : Instant
start = fromSecondsSinceUnixEpoch 0

elapsed : Duration
elapsed = fromMinutes 90

finish : Instant
finish = add start elapsed

checked : Duration
checked = difference finish start

combinedDuration : Duration
combinedDuration = add (fromHours 1) (fromMinutes 30)

boundedWindow : Interval
boundedWindow = interval 0 5400000000000

runtimeWindow : Either IntervalError Interval
runtimeWindow = refineInterval start finish

windowContainsStart : Bool
windowContainsStart = contains boundedWindow start

centralEuropeanOffset : Offset
centralEuropeanOffset = fromHours 1

runtimeOffset : Either OffsetError Offset
runtimeOffset = refineOffsetSeconds 19800

late : LocalTime
late = localTime 23 30 0 0

runtimeTime : Either LocalTimeError LocalTime
runtimeTime = refineLocalTime 9 30 0 0

endOfMonth : CalendarDateTime Gregorian
endOfMonth = at (calendarDate 31 January 2000) late

calendarAndClockPeriod : Period (CalendarDateTime Gregorian)
calendarAndClockPeriod = months 1 <+> hours 2

advanced : CalendarDateTime Gregorian
advanced = applyPeriod calendarAndClockPeriod endOfMonth

fixedOffsetDateTime : OffsetDateTime Gregorian
fixedOffsetDateTime = fromCalendarDateTimeWithOffset
  endOfMonth centralEuropeanOffset

fixedOffsetInstant : Instant
fixedOffsetInstant = toInstant fixedOffsetDateTime

displayAtUtc : Either CalendarConversionError (OffsetDateTime Gregorian)
displayAtUtc = withOffset (fromHours 0) fixedOffsetDateTime

calendarDifference : Period (CalendarDate Gregorian)
calendarDifference = between {calendar = Gregorian}
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)

exactDayDifference : Period (CalendarDate Gregorian)
exactDayDifference = betweenDays {calendar = Gregorian}
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)

explicitDayDifference : Period (CalendarDate Gregorian)
explicitDayDifference = betweenWith {calendar = Gregorian}
  (MkDateDifferencePolicy DaysOnly ClampToMonth)
  (calendarDate 31 January 2025) (calendarDate 30 March 2025)

gregorianChristmas : CalendarDate Gregorian
gregorianChristmas = calendarDate 25 December 2024

julianChristmas : Either CalendarConversionError (CalendarDate Julian)
julianChristmas = withCalendar gregorianChristmas

astronomicalNowruz1404 : CalendarDate Persian
astronomicalNowruz1404 = calendarDate 1 PersianMonths.Farvardin 1404

arithmeticNowruz1404 : CalendarDate PersianArithmetic
arithmeticNowruz1404 =
  arithmeticCalendarDate 1 PersianMonths.Farvardin 1404

simplePersianRuntime : Either PersianDateError (CalendarDate PersianSimple)
simplePersianRuntime =
  refineSimpleDate 30 PersianMonths.Esfand 1404

cachedProvider : IO TimeZoneProvider
cachedProvider = cachedTimeZoneProvider
  defaultTimeZoneCachePolicy systemTimeZoneProvider

cachedZurich : IO (Either TzdbError TimeZone)
cachedZurich = do
  provider <- cachedProvider
  timeZoneWith provider "Europe/Zurich"

systemZurich : IO (Either TzdbError TimeZone)
systemZurich = timeZone "Europe/Zurich"

resolveEndOfMonth : TimeZone -> Either ZonedDateTimeError
  (ZonedDateTime Gregorian)
resolveEndOfMonth = fromCalendarDateTimeStrictly endOfMonth

zonedEpoch : TimeZone -> Either CalendarConversionError
  (ZonedDateTime Gregorian)
zonedEpoch = fromInstant start

sameInstantIn : TimeZone -> ZonedDateTime Gregorian ->
  Either CalendarConversionError (ZonedDateTime Gregorian)
sameInstantIn zone value = fromInstant (toInstant value) zone

current : IO Instant
current = getCurrentInstant systemClock

deterministicClock : FixedClock
deterministicClock = fixedClock start

readDeterministicClock : IO Instant
readDeterministicClock = getCurrentInstant deterministicClock

windowsSnapshotProvider : IO (Either TzdbError TimeZoneProvider)
windowsSnapshotProvider = windowsSnapshotTimeZoneProvider

roundTripText : String
roundTripText = format (pR {calendar = Gregorian}) (calendarDate 3 March 2020)

roundTripDate : Either PatternError (CalendarDate Gregorian)
roundTripDate = parse (pR {calendar = Gregorian}) "2020-03-03"

customDatePattern : Pattern DateFields (CalendarDate Gregorian)
customDatePattern =
  ((pyyyy {calendar = Gregorian} <% char '/') <+>
   (pMM {calendar = Gregorian} <% char '/')) <+>
  pdd {calendar = Gregorian}

signedIntegerWire : Pattern Integer Integer
signedIntegerWire = pSignedInteger <% string "i"

instantNanosecondWire : Pattern Integer Instant
instantNanosecondWire = pInstantNanoseconds <% string "ns"

offsetWire : Pattern Offset Offset
offsetWire = pOffsetFull

civilIslamicDayWire : Pattern Integer (CalendarDate CivilIslamicBcl)
civilIslamicDayWire = pCalendarDays {calendar = CivilIslamicBcl}

ianaZoneWire : Pattern String String
ianaZoneWire = pZoneIdToken

windowsZoneWire : Pattern String String
windowsZoneWire = pZoneIdQuoted

quotedZonedPattern :
  ZonedDateTimePattern (DateFields, TimeFields) (ZonedDateTime Gregorian)
quotedZonedPattern = zonedDateTimePattern ps
  (\value => " " ++ format pZoneIdQuoted (zoneId value))

parseQuotedWindowsZone :
  (String -> IO (Either error TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolutionError (ZonedDateTime Gregorian)) ->
  IO (Either (ZonedDateTimePatternError error resolutionError)
    (ZonedDateTime Gregorian))
parseQuotedWindowsZone provider resolver =
  parseZonedDateTimePatternWith ps pZoneIdQuoted provider resolver
    "1970-01-01T00:00:00 \"Eastern Standard Time\""

germanDate : Either StrftimeError
  (Pattern DateFields (CalendarDate Gregorian))
germanDate = localeDatePattern deDE
