module Test.Locale

import Data.Vect
import IotaTime
import IotaTime.Locale.Windows.Platform
import System.Info
import Test.Support

parsesAs : Pattern DateFields (CalendarDate Gregorian) -> String ->
           CalendarDate Gregorian -> Bool
parsesAs pattern source expected = case IotaTime.Pattern.parse pattern source of
  Left _ => False
  Right actual => calendarDays actual == calendarDays expected

germanDate : Pattern DateFields (CalendarDate Gregorian)
germanDate = ((pdd <% char ' ') <+> (pMMMM' deDE <% char ' ')) <+> pyyyy

germanWeekdayDate : Pattern DateFields (CalendarDate Gregorian)
germanWeekdayDate = (((pdddd' deDE <% string ", ") <+> (pdd <% char ' ')) <+>
  (pMMMM' deDE <% char ' ')) <+> pyyyy

localeFormatsAs : Locale -> CalendarDate Gregorian -> String -> Bool
localeFormatsAs locale date expected = case localeDatePattern locale of
  Left _ => False
  Right pattern => IotaTime.Pattern.format pattern date == expected

localeParsesAs : Locale -> String -> CalendarDate Gregorian -> Bool
localeParsesAs locale source expected = case localeDatePattern locale of
  Left _ => False
  Right pattern => parsesAs pattern source expected

localeTimeFormatsAs : Locale -> LocalTime -> String -> Bool
localeTimeFormatsAs locale time expected = case localeTimePattern locale of
  Left _ => False
  Right pattern => IotaTime.Pattern.format pattern time == expected

localeTimeParsesAs : Locale -> String -> LocalTime -> Bool
localeTimeParsesAs locale source expected = case localeTimePattern locale of
  Left _ => False
  Right pattern => case IotaTime.Pattern.parse pattern source of
    Left _ => False
    Right actual => actual == expected

sameDateTime : CalendarDateTime Gregorian ->
               CalendarDateTime Gregorian -> Bool
sameDateTime left right =
  calendarDays (datePart left) == calendarDays (datePart right) &&
  localTimeOfDay left == localTimeOfDay right

sameOffsetDateTime : OffsetDateTime Gregorian ->
                     OffsetDateTime Gregorian -> Bool
sameOffsetDateTime left right =
  sameDateTime (localDateTime left) (localDateTime right) &&
  offsetOf left == offsetOf right

localeDateTimeFormatsAs : Locale -> CalendarDateTime Gregorian ->
                          String -> Bool
localeDateTimeFormatsAs locale value expected =
  case localeDateTimePattern locale of
    Left _ => False
    Right pattern => IotaTime.Pattern.format pattern value == expected

localeDateTimeParsesAs : Locale -> String ->
                         CalendarDateTime Gregorian -> Bool
localeDateTimeParsesAs locale source expected =
  case localeDateTimePattern locale of
    Left _ => False
    Right pattern => case IotaTime.Pattern.parse pattern source of
      Left _ => False
      Right actual => sameDateTime actual expected

hasStrftimeError : StrftimeError -> Either StrftimeError value -> Bool
hasStrftimeError expected (Left actual) = actual == expected
hasStrftimeError _ (Right _) = False

localeCases : List RuntimeCase
localeCases =
  [ MkRuntimeCase "built-in locale identifiers are stable"
      (localeId enUS == "en_US" && localeId deDE == "de_DE" &&
       localeId jaJP == "ja_JP")
  , MkRuntimeCase "German locale tables use Gregorian ordering"
      (index 2 (monthNames deDE) == "März" &&
       index 2 (dayNames deDE) == "Dienstag" &&
       amName deDE == "" && pmName deDE == "")
  , MkRuntimeCase "German name patterns format calendar dates"
      (IotaTime.Pattern.format (pMMMM' deDE) (calendarDate 3 March 2020) ==
        "März" &&
       IotaTime.Pattern.format (pMMM' deDE) (calendarDate 3 March 2020) ==
        "Mär" &&
       IotaTime.Pattern.format (pdddd' deDE) (calendarDate 3 March 2020) ==
        "Dienstag")
  , MkRuntimeCase "German month names parse case-insensitively"
      (parsesAs germanDate "03 märz 2020" (calendarDate 3 March 2020))
  , MkRuntimeCase "locale weekdays consume without date validation"
      (parsesAs germanWeekdayDate "Montag, 03 März 2020"
        (calendarDate 3 March 2020))
  , MkRuntimeCase "Japanese month names retain multibyte text"
      (IotaTime.Pattern.format (pMMMM' jaJP) (calendarDate 3 March 2020) ==
        "3月")
  , MkRuntimeCase "US locale date layout is month-first"
      (localeFormatsAs enUS (calendarDate 15 March 2020) "03/15/2020" &&
       localeParsesAs enUS "03/15/2020" (calendarDate 15 March 2020))
  , MkRuntimeCase "German locale date layout is day-first"
      (localeFormatsAs deDE (calendarDate 15 March 2020) "15.03.2020" &&
       localeParsesAs deDE "15.03.2020" (calendarDate 15 March 2020))
  , MkRuntimeCase "Japanese locale date layout preserves separators"
      (localeFormatsAs jaJP (calendarDate 15 March 2020) "2020年03月15日" &&
       localeParsesAs jaJP "2020年03月15日" (calendarDate 15 March 2020))
  , MkRuntimeCase "date layout compiler expands composites and literals"
      (case compileDatePattern enUS "Date: %F %% %A" of
        Left _ => False
        Right pattern =>
          IotaTime.Pattern.format pattern (calendarDate 15 March 2020) ==
            "Date: 2020-03-15 % Sunday" &&
          parsesAs pattern "Date: 2020-03-15 % Sunday"
            (calendarDate 15 March 2020))
  , MkRuntimeCase "date layout compiler rejects unsupported fields"
      (hasStrftimeError (UnsupportedSpecifier 'Q')
        (compileDatePattern enUS "%Q"))
  , MkRuntimeCase "date layout compiler rejects a dangling percent"
      (hasStrftimeError DanglingPercent
        (compileDatePattern enUS "%Y-%"))
  , MkRuntimeCase "US locale time layout uses a 12-hour period"
      (localeTimeFormatsAs enUS (localTime 13 24 35 0) "01:24:35 PM" &&
       localeTimeParsesAs enUS "01:24:35 PM" (localTime 13 24 35 0))
  , MkRuntimeCase "German locale time layout uses 24-hour time"
      (localeTimeFormatsAs deDE (localTime 13 24 35 0) "13:24:35" &&
       localeTimeParsesAs deDE "13:24:35" (localTime 13 24 35 0))
  , MkRuntimeCase "Japanese locale time layout preserves separators"
      (localeTimeFormatsAs jaJP (localTime 13 24 35 0) "13時24分35秒" &&
       localeTimeParsesAs jaJP "13時24分35秒" (localTime 13 24 35 0))
  , MkRuntimeCase "time compiler supports short and space-padded layouts"
      (case (compileTimePattern enUS "%R", compileTimePattern enUS "%l:%M") of
        (Right short, Right padded) =>
          IotaTime.Pattern.format short (localTime 13 24 0 0) == "13:24" &&
          IotaTime.Pattern.format padded (localTime 13 24 0 0) == " 1:24"
        _ => False)
  , MkRuntimeCase "time compiler rejects date-only specifiers"
      (hasStrftimeError (UnsupportedSpecifier 'Y')
        (compileTimePattern enUS "%Y"))
  , MkRuntimeCase "US locale date-time drops the zone and round-trips"
      (let value = on (localTime 13 24 35 0)
            (calendarDate 15 March 2020) in
        localeDateTimeFormatsAs enUS value
          "Sun 15 Mar 2020 01:24:35 PM" &&
        localeDateTimeParsesAs enUS
          "Sun 15 Mar 2020 01:24:35 PM" value)
  , MkRuntimeCase "German locale date-time drops the zone and round-trips"
      (let value = on (localTime 13 24 35 0)
            (calendarDate 15 March 2020) in
        localeDateTimeFormatsAs deDE value "So 15 Mär 2020 13:24:35" &&
        localeDateTimeParsesAs deDE "So 15 Mär 2020 13:24:35" value)
  , MkRuntimeCase "Japanese locale date-time round-trips"
      (let value = on (localTime 13 24 35 0)
            (calendarDate 15 March 2020) in
        localeDateTimeFormatsAs jaJP value "2020年03月15日 13時24分35秒" &&
        localeDateTimeParsesAs jaJP "2020年03月15日 13時24分35秒" value)
  , MkRuntimeCase "date-time fields parse independently of field order"
      (case compileDateTimePattern enUS "%H:%M %F" of
        Left _ => False
        Right pattern =>
          let expected = on (localTime 13 24 0 0)
                (calendarDate 15 March 2020) in
            IotaTime.Pattern.format pattern expected == "13:24 2020-03-15" &&
            case IotaTime.Pattern.parse pattern "13:24 2020-03-15" of
              Left _ => False
              Right actual => sameDateTime actual expected)
  , MkRuntimeCase "date-time compiler drops numeric zones"
      (case compileDateTimePattern enUS "%F %T %z" of
        Left _ => False
        Right pattern =>
          IotaTime.Pattern.format pattern
            (on (localTime 13 24 35 0) (calendarDate 15 March 2020)) ==
              "2020-03-15 13:24:35")
  , MkRuntimeCase "date-time compiler rejects unsupported fields"
      (hasStrftimeError (UnsupportedSpecifier 'Q')
        (compileDateTimePattern enUS "%F %Q"))
  , MkRuntimeCase "locale offset date-time compiler round-trips percent-z"
      (case compileOffsetDateTimePattern enUS "%F %T %z end" of
        Left _ => False
        Right pattern =>
          let expected = atOffset
                (on (localTime 13 24 35 0) (calendarDate 15 March 2020))
                (fromHours 2) in
            IotaTime.Pattern.format pattern expected ==
              "2020-03-15 13:24:35 +0200 end" &&
            case IotaTime.Pattern.parse pattern
              "2020-03-15 13:24:35 +0200 end" of
                Left _ => False
                Right actual => sameOffsetDateTime actual expected)
  , MkRuntimeCase "locale offset date-time requires percent-z"
      (hasStrftimeError MissingOffsetSpecifier
        (localeOffsetDateTimePattern deDE))
  , MkRuntimeCase "locale offset rejects fields following percent-z"
      (hasStrftimeError (UnsupportedSpecifier 'Y')
        (compileOffsetDateTimePattern enUS "%F %z %Y"))
  , MkRuntimeCase "Windows pictures translate field widths"
      (windowsPictureToStrftime "dddd, dd MMMM yyyy HH:mm:ss tt" ==
        "%A, %d %B %Y %H:%M:%S %p" &&
       windowsPictureToStrftime "ddd d MMM yy h:m:s t" ==
        "%a %e %b %y %I:%M:%S %p")
  , MkRuntimeCase "Windows pictures preserve quoted literals"
      (windowsPictureToStrftime "yyyy'年'MM'月'dd'日'" ==
        "%Y年%m月%d日" &&
       windowsPictureToStrftime "hh 'o''clock' tt 100%" ==
        "%I o'clock %p 100%%")
  ]

patternsCompile : Locale -> Bool
patternsCompile locale =
  case (localeDatePattern locale, localeTimePattern locale,
        localeDateTimePattern locale) of
    (Right _, Right _, Right _) => True
    _ => False

nativeLocaleCases : IO (List RuntimeCase)
nativeLocaleCases = if isWindows
  then pure []
  else do
    cLocale <- localeByName "C"
    configured <- currentLocale
    missing <- localeByName "iotatime_LOCALE_DOES_NOT_EXIST"
    pure
      [ MkRuntimeCase "native Unix C locale has complete English names"
          (case cLocale of
            Left _ => False
            Right locale =>
              index 0 (monthNames locale) == "January" &&
              index 0 (dayNames locale) == "Sunday" &&
              patternsCompile locale)
      , MkRuntimeCase "native Unix current locale layouts compile"
          (case configured of
            Left _ => False
            Right locale => localeId locale /= "" && patternsCompile locale)
      , MkRuntimeCase "native Unix missing locale returns a typed failure"
          (case missing of
            Left (LocaleNotFound name) =>
              name == "iotatime_LOCALE_DOES_NOT_EXIST"
            _ => False)
      ]

utcProvider : String -> IO (Either String TimeZone)
utcProvider "UTC" = pure (Right (fixedDateTimeZone "UTC" zeroOffset))
utcProvider name = pure (Left ("unknown zone: " ++ name))

strictResolver : CalendarDateTime Gregorian -> TimeZone ->
                 Either ZonedDateTimeError (ZonedDateTime Gregorian)
strictResolver = fromCalendarDateTimeStrictly

rejectingResolver : CalendarDateTime Gregorian -> TimeZone ->
                    Either String (ZonedDateTime Gregorian)
rejectingResolver local zone = Left "resolution rejected"

zonedLocaleCases : IO (List RuntimeCase)
zonedLocaleCases = do
  resolved <- parseZonedDateTime utcProvider strictResolver enUS
    "Sun 15 Mar 2020 01:24:35 PM UTC"
  zoneless <- parseZonedDateTime utcProvider strictResolver jaJP
    "2020年03月15日 13時24分35秒"
  unknown <- parseZonedDateTime utcProvider strictResolver enUS
    "Sun 15 Mar 2020 01:24:35 PM EDT"
  rejected <- parseZonedDateTime utcProvider rejectingResolver enUS
    "Sun 15 Mar 2020 01:24:35 PM UTC"
  malformed <- parseZonedDateTime utcProvider strictResolver enUS
    "Sun 15 Mar 2020 01:24:35 PM UTC extra"
  pure
    [ MkRuntimeCase "locale percent-Z resolves through provider and resolver"
        (case resolved of
          Left _ => False
          Right value =>
            zoneId value == "UTC" &&
            sameDateTime (toCalendarDateTime value)
              (on (localTime 13 24 35 0) (calendarDate 15 March 2020)))
    , MkRuntimeCase "locale zoned parser rejects a zoneless layout"
        (case zoneless of
          Left (ZonedLayoutError MissingZoneSpecifier) => True
          _ => False)
    , MkRuntimeCase "locale zoned parser preserves provider failures"
        (case unknown of
          Left (ZonedProviderError "unknown zone: EDT") => True
          _ => False)
    , MkRuntimeCase "locale zoned parser preserves resolver failures"
        (case rejected of
          Left (ZonedResolutionError "resolution rejected") => True
          _ => False)
    , MkRuntimeCase "locale zoned parser rejects whitespace in zone tokens"
        (case malformed of
          Left (ZonedParseError _) => True
          _ => False)
    ]

export
run : IO Bool
run = do
  nativeCases <- nativeLocaleCases
  zonedCases <- zonedLocaleCases
  runSuite "locale tests" (localeCases ++ nativeCases ++ zonedCases)
