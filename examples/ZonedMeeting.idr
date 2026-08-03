module ZonedMeeting

import IotaTime

meetingText : String
meetingText = "2024-04-23T09:00:00 Europe/Zurich"

main : IO ()
main = do
  result <- parseStandardZonedDateTime {calendar = Gregorian} timeZone
    fromCalendarDateTimeStrictly meetingText
  case result of
    Left _ => putStrLn "Could not resolve the meeting time"
    Right meeting => putStrLn (formatZonedDateTime pZonedDateTime meeting)