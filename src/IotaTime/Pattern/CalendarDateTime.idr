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

||| Combine independently typed calendar-date and local-time patterns.
public export
calendarDateTimePattern :
  {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern dateState (CalendarDate calendar) ->
  Pattern timeState LocalTime ->
  Pattern (dateState, timeState) (CalendarDateTime calendar)
calendarDateTimePattern = pairPattern datePart localTimeOfDay
  (\date, time => on time date)

||| Sortable ISO local date-time: `yyyy-MM-ddTHH:mm:ss`.
public export
ps : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
ps = calendarDateTimePattern (pR <% char 'T') pT

||| Sortable ISO local date-time with nine fractional digits.
public export
po : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
po = calendarDateTimePattern (pR <% char 'T') pr

||| Full date with a short 12-hour local time.
public export
pf : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pf = calendarDateTimePattern (pD <% char ' ') pt

||| Full date with a long 24-hour local time.
public export
pF : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pF = calendarDateTimePattern (pD <% char ' ') pT

||| Short date with a short 12-hour local time.
public export
pg : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pg = calendarDateTimePattern (pd <% char ' ') pt

||| Short date with a long 24-hour local time.
public export
pG : {calendar : Type} -> {auto patterned : CalendarPattern calendar} ->
  Pattern (DateFields, TimeFields) (CalendarDateTime calendar)
pG = calendarDateTimePattern (pd <% char ' ') pT