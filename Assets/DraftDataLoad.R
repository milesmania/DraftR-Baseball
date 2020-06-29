
## Get Player Data ####
download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")

#accessed from:  https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=fangraphsdc
getFangraphData(download_location)
pitchers <- read.csv("Data/FanGraphs-Pitchers.csv",stringsAsFactors = F); colnames(pitchers)[1] <- "Name"
hitters <- read.csv("Data/FanGraphs-Hitters.csv",stringsAsFactors = F); colnames(hitters)[1] <- "Name"

#accessed from: http://crunchtimebaseball.com/baseball_map.html
masterFileURL <- "http://crunchtimebaseball.com/master.csv"
masterFile <- "Data/baseball_map.csv"
masterFile.ModifiedDate <- as.Date(file.info(masterFile)$mtime)
if(Sys.Date()-masterFile.ModifiedDate > 2){
  download.file(masterFileURL,destfile=masterFile,method="libcurl")
}
master <- read.csv(masterFile,stringsAsFactors = F)

## Get Draft Data ####

draftFile <- "Data/FFDraftData.RData"

fantraxDraftFile <- "FantraxDraftResults"

draftData <- loadData(fantraxDraftFile)