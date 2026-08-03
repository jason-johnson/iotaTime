module IotaTime.Locale

import Data.Vect
import IotaTime.Locale.Unix.Platform
import IotaTime.Locale.Windows.Platform
import System
import System.Info

%default total

export
record Locale where
  constructor MkLocale
  storedLocaleId : String
  storedMonthNames : Vect 12 String
  storedMonthNamesShort : Vect 12 String
  storedDayNames : Vect 7 String
  storedDayNamesShort : Vect 7 String
  storedAmName : String
  storedPmName : String
  storedRawDateFormat : String
  storedRawTimeFormat : String
  storedRawDateTimeFormat : String

public export
localeId : Locale -> String
localeId = storedLocaleId

public export
monthNames : Locale -> Vect 12 String
monthNames = storedMonthNames

public export
monthNamesShort : Locale -> Vect 12 String
monthNamesShort = storedMonthNamesShort

public export
dayNames : Locale -> Vect 7 String
dayNames = storedDayNames

public export
dayNamesShort : Locale -> Vect 7 String
dayNamesShort = storedDayNamesShort

public export
amName : Locale -> String
amName = storedAmName

public export
pmName : Locale -> String
pmName = storedPmName

export
rawDateFormat : Locale -> String
rawDateFormat = storedRawDateFormat

export
rawTimeFormat : Locale -> String
rawTimeFormat = storedRawTimeFormat

export
rawDateTimeFormat : Locale -> String
rawDateTimeFormat = storedRawDateTimeFormat

public export
data LocaleError
  = LocaleNotFound String
  | LocalePlatformError String

public export
Eq LocaleError where
  LocaleNotFound left == LocaleNotFound right = left == right
  LocalePlatformError left == LocalePlatformError right = left == right
  _ == _ = False

public export
Show LocaleError where
  show (LocaleNotFound name) = "locale is not installed: " ++ name
  show (LocalePlatformError message) = message

fromUnixData : String -> UnixLocaleData -> Locale
fromUnixData valueId localeData = MkLocale
  valueId
  (localeMonthNames localeData)
  (localeMonthNamesShort localeData)
  (localeDayNames localeData)
  (localeDayNamesShort localeData)
  (localeAmName localeData)
  (localePmName localeData)
  (localeDateFormat localeData)
  (localeTimeFormat localeData)
  (localeDateTimeFormat localeData)

unixLocaleByName : String -> IO (Either LocaleError Locale)
unixLocaleByName name = do
  loaded <- loadUnixLocaleData name
  pure (case loaded of
    Nothing => Left (LocaleNotFound name)
    Just localeData => Right (fromUnixData name localeData))

currentLocaleId : IO String
currentLocaleId = do
  all <- getEnv "LC_ALL"
  time <- getEnv "LC_TIME"
  language <- getEnv "LANG"
  pure (firstPresent [all, time, language])
  where
    firstPresent : List (Maybe String) -> String
    firstPresent [] = "C"
    firstPresent (Nothing :: rest) = firstPresent rest
    firstPresent (Just "" :: rest) = firstPresent rest
    firstPresent (Just value :: rest) = value

unixCurrentLocale : IO (Either LocaleError Locale)
unixCurrentLocale = do
  valueId <- currentLocaleId
  loaded <- loadUnixLocaleData ""
  case loaded of
    Just localeData => pure (Right (fromUnixData valueId localeData))
    Nothing => do
      fallback <- loadUnixLocaleData "C"
      pure (case fallback of
        Just localeData => Right (fromUnixData "C" localeData)
        Nothing => Left (LocalePlatformError
          "native Unix locale access could not load the POSIX C locale"))

fromWindowsData : WindowsLocaleData -> Locale
fromWindowsData localeData =
  let dateFormat = IotaTime.Locale.Windows.Platform.localeDateFormat localeData
      timeFormat = IotaTime.Locale.Windows.Platform.localeTimeFormat localeData
   in MkLocale
        (localeIdentifier localeData)
        (IotaTime.Locale.Windows.Platform.localeMonthNames localeData)
        (IotaTime.Locale.Windows.Platform.localeMonthNamesShort localeData)
        (IotaTime.Locale.Windows.Platform.localeDayNames localeData)
        (IotaTime.Locale.Windows.Platform.localeDayNamesShort localeData)
        (IotaTime.Locale.Windows.Platform.localeAmName localeData)
        (IotaTime.Locale.Windows.Platform.localePmName localeData)
        dateFormat
        timeFormat
        (dateFormat ++ " " ++ timeFormat)

windowsLocaleByName : String -> IO (Either LocaleError Locale)
windowsLocaleByName name = do
  loaded <- loadWindowsLocaleData False name
  pure (case loaded of
    Nothing => Left (LocaleNotFound name)
    Just localeData => Right (fromWindowsData localeData))

windowsCurrentLocale : IO (Either LocaleError Locale)
windowsCurrentLocale = do
  loaded <- loadWindowsLocaleData True ""
  pure (case loaded of
    Nothing => Left (LocalePlatformError
      "native Windows locale access could not load the user locale")
    Just localeData => Right (fromWindowsData localeData))

||| Read a named locale from the operating system locale database.
public export
localeByName : String -> IO (Either LocaleError Locale)
localeByName name = if isWindows
  then windowsLocaleByName name
  else unixLocaleByName name

||| Read the locale selected by LC_ALL, LC_TIME, or LANG.
public export
currentLocale : IO (Either LocaleError Locale)
currentLocale = if isWindows
  then windowsCurrentLocale
  else unixCurrentLocale

public export
enUS : Locale
enUS = MkLocale
  "en_US"
  [ "January", "February", "March", "April", "May", "June"
  , "July", "August", "September", "October", "November", "December"
  ]
  [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
  , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ]
  [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
  , "Saturday"
  ]
  [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
  "AM"
  "PM"
  "%m/%d/%Y"
  "%r"
  "%a %d %b %Y %r %Z"

public export
deDE : Locale
deDE = MkLocale
  "de_DE"
  [ "Januar", "Februar", "März", "April", "Mai", "Juni"
  , "Juli", "August", "September", "Oktober", "November", "Dezember"
  ]
  [ "Jan", "Feb", "Mär", "Apr", "Mai", "Jun"
  , "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"
  ]
  [ "Sonntag", "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"
  , "Samstag"
  ]
  [ "So", "Mo", "Di", "Mi", "Do", "Fr", "Sa" ]
  ""
  ""
  "%d.%m.%Y"
  "%T"
  "%a %d %b %Y %T %Z"

public export
jaJP : Locale
jaJP = MkLocale
  "ja_JP"
  [ "1月", "2月", "3月", "4月", "5月", "6月"
  , "7月", "8月", "9月", "10月", "11月", "12月"
  ]
  [ "1月", "2月", "3月", "4月", "5月", "6月"
  , "7月", "8月", "9月", "10月", "11月", "12月"
  ]
  [ "日曜日", "月曜日", "火曜日", "水曜日", "木曜日", "金曜日", "土曜日" ]
  [ "日", "月", "火", "水", "木", "金", "土" ]
  "午前"
  "午後"
  "%Y年%m月%d日"
  "%H時%M分%S秒"
  "%Y年%m月%d日 %H時%M分%S秒"
