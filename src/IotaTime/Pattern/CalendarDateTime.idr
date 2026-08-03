module IotaTime.Pattern.CalendarDateTime

import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.LocalTime
import IotaTime.Pattern
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.LocalTime

%default total

public export
calendarDateTimePattern :
  Pattern dateState (CalendarDate Gregorian) ->
  Pattern timeState LocalTime ->
  Pattern (dateState, timeState) (CalendarDateTime Gregorian)
calendarDateTimePattern = pairPattern datePart localTimeOfDay
  (\date, time => on time date)

public export
ps : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
ps = calendarDateTimePattern (pR <% char 'T') pT

public export
po : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
po = calendarDateTimePattern (pR <% char 'T') pr

public export
pf : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
pf = calendarDateTimePattern (pD <% char ' ') pt

public export
pF : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
pF = calendarDateTimePattern (pD <% char ' ') pT

public export
pg : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
pg = calendarDateTimePattern (pd <% char ' ') pt

public export
pG : Pattern (DateFields, TimeFields) (CalendarDateTime Gregorian)
pG = calendarDateTimePattern (pd <% char ' ') pT