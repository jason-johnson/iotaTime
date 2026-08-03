module IotaTime.Locale

import Data.Vect

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
