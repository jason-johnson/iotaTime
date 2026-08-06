module Test.PatternScalar

import IotaTime
import Test.Support

roundTrips : Eq value => Pattern state value -> value -> Bool
roundTrips pattern value =
  case IotaTime.Pattern.parse pattern (IotaTime.Pattern.format pattern value) of
    Left _ => False
    Right actual => actual == value

calendarRoundTrips : {calendar : Type} ->
                     {auto cal : Calendar calendar} ->
                     Eq (CalendarDate calendar @{cal}) =>
                     CalendarDate calendar @{cal} -> Bool
calendarRoundTrips {calendar} @{cal} =
  roundTrips (pCalendarDays {calendar} @{cal})

patternScalarCases : List RuntimeCase
patternScalarCases =
  [ MkRuntimeCase "signed integer patterns preserve arbitrary precision"
      (roundTrips pSignedInteger
        999999999999999999999999999999999999 &&
      roundTrips pSignedInteger
        (-999999999999999999999999999999999999))
  , MkRuntimeCase "instant nanosecond patterns have no calendar range limit"
      (roundTrips pInstantNanoseconds
        (fromNanosecondsSinceEpoch
          999999999999999999999999999999999999) &&
      roundTrips pInstantNanoseconds
        (fromNanosecondsSinceEpoch
          (-999999999999999999999999999999999999)))
  , MkRuntimeCase "scalar instant patterns compose with protocol literals"
      (let pattern = pInstantNanoseconds <% string "ns" in
        IotaTime.Pattern.format pattern
          (fromNanosecondsSinceEpoch (-42)) == "-42ns" &&
        case IotaTime.Pattern.parse pattern "-42ns" of
          Right value => value == fromNanosecondsSinceEpoch (-42)
          Left _ => False)
  , MkRuntimeCase "full offset pattern preserves every whole second"
      (roundTrips pOffsetFull (IotaTime.Offset.fromSeconds (-64800)) &&
      roundTrips pOffsetFull (IotaTime.Offset.fromSeconds 19815) &&
      roundTrips pOffsetFull (IotaTime.Offset.fromSeconds 64800))
  , MkRuntimeCase "Gregorian and Julian day-count patterns round-trip"
      (calendarRoundTrips {calendar = Gregorian}
        (calendarDate 29 February 2000) &&
      calendarRoundTrips {calendar = Julian}
        (julianDate 29 JulianMonths.February 1900))
  , MkRuntimeCase "Coptic and Persian day-count patterns round-trip"
      (calendarRoundTrips {calendar = Coptic}
        (copticDate 6 CopticMonths.PiKogiEnavot 1731) &&
      calendarRoundTrips {calendar = Persian}
        (persianDate 30 PersianMonths.Esfand 1403))
  , MkRuntimeCase "Hebrew numbering remains in the selected calendar type"
      (calendarRoundTrips {calendar = HebrewCivil}
        (hebrewDate 1 5784 HebrewMonths.AdarI) &&
      calendarRoundTrips {calendar = HebrewScriptural}
        (hebrewDate' {numbering = Scriptural}
          1 5784 HebrewMonths.AdarI))
  , MkRuntimeCase "Islamic epoch and pattern remain in the selected type"
      (calendarRoundTrips {calendar = IslamicBase15}
        (islamicDate' {pattern = Base15}
          30 IslamicMonths.DhulHijjah 15) &&
      calendarRoundTrips {calendar = CivilIslamicBase16}
        (civilIslamicDate 30 IslamicMonths.DhulHijjah 16) &&
      case IotaTime.Pattern.parse
        (pCalendarDays {calendar = CivilIslamicBase16}) "-503165" of
          Right date => date ==
            civilIslamicDate 1 IslamicMonths.Muharram 1
          Left _ => False)
  , MkRuntimeCase "calendar day patterns enforce the selected range"
      (case IotaTime.Pattern.parse
        (pCalendarDays {calendar = Gregorian}) "-999999999" of
          Left (InvalidValue _) => True
          _ => False)
  , MkRuntimeCase "zone token patterns represent IANA identifiers"
      (roundTrips pZoneIdToken "America/Argentina/Buenos_Aires" &&
      case IotaTime.Pattern.parse pZoneIdToken "Eastern Standard Time" of
        Left _ => True
        Right _ => False)
  , MkRuntimeCase "quoted zone patterns represent Windows identifiers"
      (IotaTime.Pattern.format pZoneIdQuoted "Eastern Standard Time" ==
        "\"Eastern Standard Time\"" &&
      roundTrips pZoneIdQuoted "Eastern Standard Time")
  , MkRuntimeCase "quoted zone patterns escape protocol-sensitive text"
      (roundTrips pZoneIdQuoted "Custom \\\"Zone\\\"\tName")
  , MkRuntimeCase "quoted zone patterns reject raw control whitespace"
      (case IotaTime.Pattern.parse pZoneIdQuoted "\"Custom\tZone\"" of
        Left _ => True
        Right _ => False)
  ]

export
run : IO Bool
run = runSuite "scalar pattern tests" patternScalarCases