module IotaTime.Pattern.CalendarDateTime

import IotaTime.Calendar
import IotaTime.Calendar.Gregorian
import IotaTime.CalendarDateTime
import IotaTime.LocalTime
import IotaTime.Pattern
import IotaTime.Pattern.Calendar
import IotaTime.Pattern.CalendarDate
import IotaTime.Pattern.LocalTime

%default total

public export
calendarDateTimePattern :
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern dateState (CalendarDate calendar) ->
  Pattern timeState LocalTime ->
  Pattern (dateState, timeState) (CalendarDateTime calendar)
calendarDateTimePattern = pairPattern datePart localTimeOfDay
  (\date, time => on time date)

public export
ps : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
ps = calendarDateTimePattern (pR <% char 'T') pT

public export
po : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
po = calendarDateTimePattern (pR <% char 'T') pr

public export
pf : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pf = calendarDateTimePattern (pD <% char ' ') pt

public export
pF : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pF = calendarDateTimePattern (pD <% char ' ') pT

public export
pg : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pg = calendarDateTimePattern (pd <% char ' ') pt

public export
pG : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pG = calendarDateTimePattern (pd <% char ' ') pT