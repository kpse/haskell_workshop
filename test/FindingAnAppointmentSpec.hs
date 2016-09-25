module FindingAnAppointmentSpec where
import FindingAnAppointment (getStartTime)
import Test.Hspec

spec = describe "Finding an appointment" $ do
  it "Example from description" $ do
    let schedules =
          [ [("09:00", "11:30"), ("13:30", "16:00"), ("16:00", "17:30"), ("17:45", "19:00")]
          , [("09:15", "12:00"), ("14:00", "16:30"), ("17:00", "17:30")]
          , [("11:30", "12:15"), ("15:00", "16:30"), ("17:45", "19:00")]
          ]
    getStartTime schedules 60 `shouldBe` Just "12:15"
    getStartTime schedules 90 `shouldBe` Nothing
