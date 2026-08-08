module GuidedMeeting

import IotaTime

meetingDate : CalendarDate Gregorian
meetingDate = IotaTime.Calendar.Gregorian.calendarDate 23 April 2024

meetingTime : LocalTime
meetingTime = localTime 9 0 0 0

meeting : CalendarDateTime Gregorian
meeting = at meetingDate meetingTime

export
run : IO ()
run = do
  zurichResult <- timeZone "Europe/Zurich"
  newYorkResult <- timeZone "America/New_York"
  case (zurichResult, newYorkResult) of
    (Right zurich, Right newYork) =>
      case fromCalendarDateTimeStrictly meeting zurich of
        Left _ => putStrLn "The meeting time is skipped or ambiguous"
        Right here =>
          case IotaTime.ZonedDateTime.fromInstant
            (IotaTime.ZonedDateTime.toInstant here) newYork of
            Left _ => putStrLn "The instant is outside the Gregorian range"
            Right there => do
              let display = zonedDateTimePattern pF
                    (\value => " " ++ zoneAbbreviation value)
              putStrLn (formatZonedDateTime display here)
              putStrLn (formatZonedDateTime display there)
    _ => putStrLn "Could not load both time zones"
