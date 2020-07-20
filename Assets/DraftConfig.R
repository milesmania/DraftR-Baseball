config <- setConfigTxt()
stopifnot(userName!="[ENTER-USERNAME]",passWord!="[ENTER-PASSWORD]", leagueId!="[ENTER-LEAGUEID]", MyTeam!="[ENTER-MYTEAM]")

teams <- getTeamsFantrax(fantraxDraftFile)

rosterPositions <- c('C-1','1B-1','2B-1','3B-1','SS-1','OF-1','OF-2','OF-3','SP-1','SP-2','RP-1','RP-2','P-1','BE-1','BE-2','BE-3')

fantraxPlayerFile <- 'Fantrax-Players'

fantraxPlayerFileLocation <- paste0("Data/",fantraxPlayerFile,".csv")
fantraxPlayerFile.ModifiedDate <- as.Date(file.info(fantraxPlayerFileLocation)$mtime)

if(Sys.Date()-fantraxPlayerFile.ModifiedDate > 0){
  fantraxPlayerData <- getFantraxPlayerData(userName,passWord,leagueId,fantraxPlayerFile,download_location)
}else{
  fantraxPlayerData <- read.csv(fantraxPlayerFileLocation, stringsAsFactors = FALSE)
}
