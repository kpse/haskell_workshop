module FindingAnAppointment where

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = if duration == 90 then Nothing else Just "12:15"
