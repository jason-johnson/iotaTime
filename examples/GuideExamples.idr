module GuideExamples

import IotaTime

leapDay : CalendarDate Gregorian
leapDay = calendarDate 29 February 2020

start : Instant
start = fromSecondsSinceUnixEpoch 0

elapsed : Duration
elapsed = IotaTime.Duration.fromMinutes 90

finish : Instant
finish = IotaTime.Instant.add start elapsed

checked : Duration
checked = difference finish start

late : LocalTime
late = localTime 23 30 0 0

endOfMonth : CalendarDateTime Gregorian
endOfMonth = at (calendarDate 31 January 2000) late

advanced : CalendarDateTime Gregorian
advanced = applyPeriod (months 1 <+> hours 2) endOfMonth

gregorianChristmas : CalendarDate Gregorian
gregorianChristmas = calendarDate 25 December 2024

julianChristmas : Either CalendarConversionError (CalendarDate Julian)
julianChristmas = IotaTime.Calendar.withCalendar gregorianChristmas

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
quotedZonedPattern = zonedDateTimePattern {calendar = Gregorian} ps
  (\value => " " ++ format pZoneIdQuoted (zoneId value))

parseQuotedWindowsZone :
  (String -> IO (Either error TimeZone)) ->
  (CalendarDateTime Gregorian -> TimeZone ->
    Either resolutionError (ZonedDateTime Gregorian)) ->
  IO (Either (ZonedDateTimePatternError error resolutionError)
    (ZonedDateTime Gregorian))
parseQuotedWindowsZone provider resolver =
  parseZonedDateTimePatternWith {calendar = Gregorian}
    ps pZoneIdQuoted provider resolver
    "1970-01-01T00:00:00 \"Eastern Standard Time\""

germanDate : Either StrftimeError
  (Pattern DateFields (CalendarDate Gregorian))
germanDate = localeDatePattern deDE
