module Test.Locale

import Data.Vect
import IotaTime
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
  ]

export
run : IO Bool
run = runSuite "locale tests" localeCases
