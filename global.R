source("Assets/LoadPackages.R")
source("Assets/DraftFunctions.R")
source("Assets/DraftDataLoad.R")
source("Assets/DraftConfig.R")

draftFile <- "Data/FFDraftData.RData"

fantraxDraftFile <- "FantraxDraftResults"
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")