
gLatestFile <- function(sourceDir,fPattern,wildcard=TRUE){
  # list of contents
  #fPattern <- "FantraxDraftResults*"
  #sourceDir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  if(wildcard) fPattern <- paste0("*",fPattern,"*")
  Files <- list.files(sourceDir, pattern = fPattern, recursive =T, ignore.case=T)
  Files <- Files[!grepl("~",Files)]
  if(length(Files)>0){
    Files <- cbind(Files,file.info(paste0(sourceDir,"/",Files)),stringsAsFactors=F)
    latestFile <- head(Files[Files$mtime==max(Files$mtime),"Files"],1)
    return(latestFile)
  }
}

## Fantrax Functions ####
getFantraxDraftData <- function(userName,passWord,leagueId,fantraxDraftFile,download_location){
  #leagueId='nc30j3mnjsuzwnaa' #leagueId='1tpc1071kbu8rp1e'
  #loginUrl <- paste0("https://www.fantrax.com/login?showSignup=false&url=%2Fnewui%2Ffantasy%2FdraftResultsPopup.go%3FleagueId%3D",leagueId,"%26sxq_w%3D2")
  loginUrl <- paste0("https://www.fantrax.com/login")
  webUrl <- paste0("https://www.fantrax.com/newui/fantasy/draftResults.go?leagueId=",leagueId,"&csvDownload=true&csvDownload")
  
  download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  #Get available chrome drivers binman::list_versions("chromedriver")
  driver <- rsDriver(browser=c("chrome"), chromever="83.0.4103.39", port = 4444L)
  
  remote_driver <- driver[["client"]] #remote_driver$open()
  
  remote_driver$navigate(loginUrl)
  
  Sys.sleep(2)
  
  textfield_Username <- remote_driver$findElement(using = 'id', value = 'mat-input-0')
  textfield_PW <- remote_driver$findElement(using = 'id', value = 'mat-input-1')
  login_button <- remote_driver$findElement(using = 'class', value = 'mat-primary')
  
  textfield_Username$sendKeysToElement(list(userName))
  textfield_PW$sendKeysToElement(list(passWord, key = 'enter'))
  #login_button$clickElement()
  
  Sys.sleep(2)
  
  remote_driver$navigate(webUrl)
  
  Sys.sleep(2)
  
  # downloadCSV <- remote_driver$findElement(using = 'class', value = 'defaultLink')
  # downloadCSV$clickElement()
  
  latestFile <- gLatestFile(download_location,fantraxDraftFile)
  
  file.copy(file.path(download_location, latestFile), paste0("Data/",fantraxDraftFile,".csv"), overwrite = TRUE)
  
  remote_driver$close()
  driver$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  draftData <- read.csv(paste0("Data/",fantraxDraftFile,".csv"), stringsAsFactors = FALSE)
  return(draftData)
}
#getFantraxPlayerData(userName,passWord,leagueId,fantraxPlayerFile,download_location)
getFantraxPlayerData <- function(userName,passWord,leagueId,fantraxPlayerFile,download_location){
  #leagueId='nc30j3mnjsuzwnaa' #leagueId='1tpc1071kbu8rp1e'
  #loginUrl <- paste0("https://www.fantrax.com/login?showSignup=false&url=%2Fnewui%2Ffantasy%2FdraftResultsPopup.go%3FleagueId%3D",leagueId,"%26sxq_w%3D2")
  loginUrl <- paste0("https://www.fantrax.com/login")
  #https://www.fantrax.com/fxpa/downloadPlayerStats?leagueId=1tpc1071kbu8rp1e&pageNumber=1&view=STATS&positionOrGroup=ALL&seasonOrProjection=PROJECTION_0_135_SEASON&timeframeTypeCode=YEAR_TO_DATE&transactionPeriod=1&miscDisplayType=1&sortType=SCORE&maxResultsPerPage=20&statusOrTeamFilter=ALL&scoringCategoryType=5&timeStartType=PERIOD_ONLY&schedulePageAdj=0&searchName=&startDate=2020-07-23&endDate=2020-09-28&teamId=ei8euvnpkbu8rp1z&
  webUrl <- paste0("https://www.fantrax.com/fxpa/downloadPlayerStats?leagueId=",leagueId,"&view=STATS&positionOrGroup=ALL&seasonOrProjection=PROJECTION_0_135_SEASON&timeframeTypeCode=YEAR_TO_DATE&transactionPeriod=1&miscDisplayType=1&sortType=SCORE&statusOrTeamFilter=ALL&scoringCategoryType=5&timeStartType=PERIOD_ONLY")
  
  download_location <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  #Get available chrome drivers binman::list_versions("chromedriver")
  driver <- rsDriver(browser=c("chrome"), chromever="83.0.4103.39", port = 4444L)
  
  remote_driver <- driver[["client"]] #remote_driver$open()
  
  remote_driver$navigate(loginUrl)
  
  Sys.sleep(2)
  
  textfield_Username <- remote_driver$findElement(using = 'id', value = 'mat-input-0')
  textfield_PW <- remote_driver$findElement(using = 'id', value = 'mat-input-1')
  login_button <- remote_driver$findElement(using = 'class', value = 'mat-primary')
  
  textfield_Username$sendKeysToElement(list(userName))
  textfield_PW$sendKeysToElement(list(passWord, key = 'enter'))
  #login_button$clickElement()
  
  Sys.sleep(2)
  
  remote_driver$navigate(webUrl)
  
  Sys.sleep(2)
  
  # downloadCSV <- remote_driver$findElement(using = 'class', value = 'defaultLink')
  # downloadCSV$clickElement()
  
  latestFile <- gLatestFile(download_location,fantraxPlayerFile)
  
  file.copy(file.path(download_location, latestFile), paste0("Data/",fantraxPlayerFile,".csv"), overwrite = TRUE)
  
  remote_driver$close()
  driver$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  playerData <- read.csv(paste0("Data/",fantraxPlayerFile,".csv"), stringsAsFactors = FALSE)
  return(playerData)
}

getFangraphData <- function(download_location){
  hitterURL <- "https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=fangraphsdc&team=0&lg=all&players=0"
  pitcherURL <- "https://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=fangraphsdc&team=0&lg=all&players=0"
  
  hitterFile <- "Data/FanGraphs-Hitters.csv"
  pitcherFile <- "Data/FanGraphs-Pitchers.csv"
  fangraphFile <- "FanGraphs Leaderboard"
  
  hitterFile.ModifiedDate <- as.Date(file.info(hitterFile)$mtime)
  if(Sys.Date()-hitterFile.ModifiedDate > 0){
    #Get available chrome drivers binman::list_versions("chromedriver")
    
    getFangraphDataDownload(download_location,fangraphFile,hitterURL,hitterFile)
    getFangraphDataDownload(download_location,fangraphFile,pitcherURL,pitcherFile)
  }
  
}

getFangraphDataDownload <- function(download_location,fangraphFile,dataURL,dataFile){
  
  driver <- rsDriver(browser=c("chrome"), chromever="83.0.4103.39", port = 4444L)
  remote_driver <- driver[["client"]] #remote_driver$open()
  remote_driver$navigate(dataURL)
  
  Sys.sleep(2)
  exportData_Link <- remote_driver$findElement(using = 'id', value = 'ProjectionBoard1_cmdCSV')
  exportData_Link$clickElement()
  
  Sys.sleep(2)
  
  latestFile <- gLatestFile(download_location,fangraphFile)
  file.copy(file.path(download_location, latestFile), dataFile, overwrite = TRUE)
  
  Sys.sleep(1)
  
  remote_driver$close()
  driver$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
}

loadData <- function(filename){
  dData <- read.csv(paste0("Data/",filename,".csv"), stringsAsFactors = FALSE)
  return(dData)
}

updateDraftFromFantrax <- function(draftResults,username,password,leagueId,fantraxDraftFile,download_location){#leagueId='nc30j3mnjsuzwnaa' #leagueId='1tpc1071kbu8rp1e'
  fantraxDraftData <- getFantraxDraftData(username,password,leagueId,fantraxDraftFile,download_location)
  #fantraxDraftData <- read.csv(paste0("Data/",fantraxDraftFile,".csv"), stringsAsFactors = FALSE)
  fantraxDraftData <- fantraxDraftData[!is.na(fantraxDraftData$Player),]
  if(nrow(fantraxDraftData) == 0) return(fantraxDraftData)
  fantraxDraftData$pId <- sapply(1:nrow(fantraxDraftData),function(x){
    fD <- fantraxDraftData[x,]
    paste(fD[,c('Player','Team','Pos')],collapse = "|")
  })
  unpicked <- draftResults[draftResults$Pick=="","Overall"]
  slPicked <- fantraxDraftData$Ov.Pick
  picksToFill <- slPicked[slPicked %in% unpicked]
  pickstoUpdate <- fantraxDraftData[fantraxDraftData$Ov.Pick %in% picksToFill,c('Round','Fantasy.Team','pId')]
  return(pickstoUpdate)
}

getTeamsFantrax <- function(fantraxDraftFile){
  fD <- loadData(fantraxDraftFile)
  fTeams <-  unique(fD$Fantasy.Team)
  return(fTeams)
}

setConfigTxt <- function(configFile="Assets/config.txt"){
  key.val<-read.table(configFile, sep="=", col.names=c("key","value"), as.is=c(1,2))
  config <- key.val$value; names(config) <- key.val$key
  for(kV in 1:length(config)){
    assign(names(config)[kV],config[kV], envir = .GlobalEnv)
  }
  return(config)
}


## User Functions ###########################################

#dForcast <- forecastDraft(draftResults,ff); dRosters <- setRoster(draftedPlayers=dForcast)
forecastDraft <- function(draftResults,ff){
  draftResults$Selected <- ifelse(draftResults$Pick!="","selected","forecast")
  rForecast <- draftResults$Selected!="selected"
  dFF <- ff[!(ff$pId %in% draftResults[!rForecast,'Pick']),]
  if(any(rForecast)){
    for(rF in draftResults[rForecast,'Overall']){#rF=41
      dTeam <- draftResults[rF,'Team']
      dPlayers <- subset(draftResults,Pick!="" & Team == dTeam,Pick)
      dPicksLeft <- subset(draftResults,Pick=="" & Team == dTeam,Pick)
      if(nrow(dPlayers) > 0){
        dPos <- sapply(strsplit(dPlayers$Pick, split='|', fixed=TRUE), `[`, 3)
        draftResults <- forecastDraft_Restrict(draftResults,rF,dFF,dPlayers,dPos,dPicksLeft)
      }
      if(draftResults[rF,'Pick'] == "") draftResults[rF,'Pick'] <- head(dFF$pId,1)
      dFF <- dFF[dFF$pId != draftResults[rF,'Pick'], ]
    }
    #draftResults[rForecast,'Pick'] <- ff[1:nrow(draftResults[rForecast,]),'pId']
  }
  draftResults <- merge(draftResults,ff, by.x = "Pick", by.y = "pId", all.x=TRUE, sort=FALSE)
  draftResults <- draftResults[,c(2:4,1,5:ncol(draftResults))]
  draftResults <- draftResults[order(draftResults$Overall),]
  return(draftResults)
}

forecastDraft_Restrict <- function(draftResults,rF,dFF,dPlayers,dPos,dPicksLeft){
  if(nrow(dPlayers) > 0){
    #Enforce position caps
    dPickMin <- ifelse(nrow(dPicksLeft)>5,0,1)
    if(length(grep("OF",dPos)) >= dPickMin + 2){
      if(length(grep("P",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("P",dFF$pos),'pId'],1)}
      else if(length(grep("B",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("B",dFF$pos),'pId'],1)}
      else if(length(grep("C",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("C",dFF$pos),'pId'],1)}
      else if(length(grep("RP",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("RP",dFF$pos),'pId'],1)}
      else if(length(grep("SP",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("SP",dFF$pos),'pId'],1)}
      else if(length(grep("1B",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("1B",dFF$pos),'pId'],1)}
      else if(length(grep("2B",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("2B",dFF$pos),'pId'],1)}
      else if(length(grep("3B",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("3B",dFF$pos),'pId'],1)}
      else if(length(grep("SS",dPos)) == 0){draftResults[rF,'Pick'] <- head(dFF[grep("SS",dFF$pos),'pId'],1)}
    }
    if(draftResults[rF,'Pick'] == "") {
      dRestrict <- character()
      dRestrict <- c(dRestrict,"Util")
      if(length(grep("C",dPos)) >= dPickMin + 1) dRestrict <- c(dRestrict,"C")
      if(length(grep("1B",dPos)) >= dPickMin + 1) dRestrict <- c(dRestrict,"1B")
      if(length(grep("2B",dPos)) >= dPickMin + 1) dRestrict <- c(dRestrict,"2B")
      if(length(grep("3B",dPos)) >= dPickMin + 1) dRestrict <- c(dRestrict,"3B")
      if(length(grep("SS",dPos)) >= dPickMin + 1) dRestrict <- c(dRestrict,"SS")
      if(length(grep("SP",dPos)) >= dPickMin + 2) dRestrict <- c(dRestrict,"SP")
      if(length(grep("RP",dPos)) >= dPickMin + 2) dRestrict <- c(dRestrict,"RP")
      if(length(grep("OF",dPos)) >= dPickMin + 3) dRestrict <- c(dRestrict,"OF")
      
      if(length(dRestrict) > 0){
        if(any(!grepl(paste(dRestrict,collapse = "|"),dFF$pos))){
          draftResults[rF,'Pick'] <- head(dFF[!grepl(paste(dRestrict,collapse = "|"),dFF$pos),'pId'],1)
        }
      } 
    }
  }
  if(draftResults[rF,'Pick'] == "") draftResults[rF,'Pick'] <- head(dFF$pId,1)
  return(draftResults)
}
#forecastMockScore(draftResults,ff, hitters, pitchers, hitterStats, pitcherStats, fTeam=MyTeam)
forecastMockScore <- function(draftResults,ff, hitters, pitchers, hitterStats, pitcherStats,nPlayers=10,fTeam=NA){
  draftResults$Selected <- ifelse(draftResults$Pick!="","selected","forecast")
  rForecast <- draftResults$Selected!="selected"
  dFF_left <- ff[!(ff$pId %in% draftResults[!rForecast,'Pick']),]
  if(is.na(fTeam) || fTeam == "Next Pick"){
    fTeam <- head(draftResults[rForecast,'Team'],1) #Get next pick
  }
  myNextPick <- head(draftResults[rForecast & draftResults$Team == fTeam,'Overall'],1)
  nextPlayers <- head(dFF_left,nPlayers)
  nextDraftScore <- data.frame(Player=nextPlayers$pId,Score=rep(NA,nrow(nextPlayers)),
                               Rank=rep(NA,nrow(nextPlayers)), stringsAsFactors = F)
  allTeams <- unique(draftResults$Team)
  for(aT in 1:length(allTeams)){
    nextDraftScore[,allTeams[aT]] <- NA
  }
  for(nP in 1:nrow(nextPlayers)){#nP=1
    mockResults <- draftResults
    mockResults[myNextPick,'Pick'] <- nextPlayers[nP,'pId']
    mockForecast <- forecastDraft(mockResults,dFF_left)
    mockALL <- rotoTable_Total(mockForecast, hitters, pitchers, hitterStats, pitcherStats)
    if(nrow(mockALL)>0){
      mockRank <- rotoTable_Rank(pTotal = mockALL, pStats = c(hitterStats, pitcherStats))
      nextDraftScore[nP,'Score'] <- mockRank[mockRank$Team == fTeam, 'Total']
      nextDraftScore[nP,'Rank'] <- which(mockRank$Team == fTeam)
      for(aT in 1:length(allTeams)){
        nextDraftScore[nP,allTeams[aT]] <- mockRank[mockRank$Team == allTeams[aT], 'Total']
      }
    }
  }
  return(nextDraftScore)
}

forecastMockScoreKable <- function(draftResults,ff, hitters, pitchers, hitterStats, pitcherStats,nPlayers=10,fTeam=NA){
  nextDraftScore <- forecastMockScore(draftResults,ff, hitters, pitchers, hitterStats, pitcherStats,nPlayers,fTeam)
  nextDraftScore[4:ncol(nextDraftScore)] <- nextDraftScore[4:ncol(nextDraftScore)] %>% 
    mutate_if(is.numeric, function(x) {
      cell_spec(x, bold = T, 
                color = spec_color(x, begin = 0.1, end = 0.9),
                font_size = spec_font_size(x))
    }) 
  
  nextDraftScore <- nextDraftScore %>% mutate(Rank = cell_spec(
    Rank, color = "white", bold = T,
    background = spec_color(Rank, begin = 0.1, end = 0.9, option = "A")
  ))
  
  nextDraftScore <- nextDraftScore %>% kable(escape = F, align = "c") %>%
    kable_styling(c("striped", "condensed"), full_width = F)
  return(nextDraftScore)
}

draftChart <- function(dForcast){#dForcast=draftForecast
  draftPoints <- dForcast %>%
    group_by(Team) %>%
    summarise(actual = sum(points[Selected=="selected"]),
              forecast = sum(points))
  draftPoints$Team <- factor(draftPoints$Team)
  p <- ggplot(draftPoints, aes(x=actual, y=forecast, color=Team, fill=Team)) 
  p <- p + #geom_rect(aes(xmin=actualLo,xmax=actualHi, ymin=forecastLo,ymax=forecastHi), alpha = 0.5)+
    geom_point(size=5,color="white")
  p <- p + geom_label_repel(aes(label=paste0(Team,"\nActual:",round(actual,0),"\nForecast:",round(forecast,0))),
                            size=3,color="black",fontface="bold",show.legend = FALSE)
  p <- p + theme_minimal()+
    theme(legend.position = "none" #c(0.95, 0.1)
    ) + #ylab(yVal) + xlab(paste("Median",xVal,"Projection")) +
    labs(title = paste("Draft Projections after", nrow(dForcast[dForcast$Selected=="selected",]),"picks"))
  p
}
#rosters <- setRoster(draftedPlayers=draftForecast)
setRoster <- function(draftedPlayers,showForecast=TRUE,
                      rosterPositions = c('C-1','1B-1','2B-1','3B-1','SS-1','OF-1','OF-2','OF-3','SP-1','SP-2','RP-1','RP-2','P-1','BE-1','BE-2','BE-3')
){#draftedPlayers <- draftForecast
  #Set Roster Data
  teams <- unique(draftedPlayers$Team)
  rosters <- data.frame(matrix("",length(rosterPositions),length(teams)), stringsAsFactors = F)
  colnames(rosters) <- teams; rownames(rosters) <- rosterPositions; #rosters[,] <- ""
  benchSpots <- rosterPositions[grepl("BE-",rosterPositions)]
  
  if(!showForecast) draftedPlayers <- subset(draftedPlayers,Selected == "selected")
  for(t in 1:length(teams)){#t=1
    teamPlayers <- draftedPlayers[draftedPlayers$Team == teams[t],]
    teamPlayers <- teamPlayers[order(teamPlayers$points,decreasing = T),]
    if(nrow(teamPlayers) > 0){
      teamDrafted <- character()
      for(tP in 1:nrow(teamPlayers)){#tP=1
        tPos <- paste0(gsub("/","|",teamPlayers[tP,'pos']),"|BE")
        tPosP <- ifelse(grepl('P',tPos),paste0(tPos,"|^P"),tPos)
        newPlayer <- paste0(teamPlayers[tP,c('name','pos','team')], collapse = "|")
        if(teamPlayers[tP,"Selected"]=="forecast") newPlayer <- paste0("(",newPlayer,")")
        if(any(rosters[grepl(tPosP,rownames(rosters)),teams[t]]=="")){
          availSlots <- rownames(rosters)[grepl(tPosP,rownames(rosters)) & rosters[,teams[t]] == ""]
        }else{
          rosters[nrow(rosters)+1,] <- rep("",ncol(rosters))
          availSlots <- paste0("BE-",length(benchSpots)+1)
          benchSpots <- c(benchSpots,availSlots)
          rownames(rosters)[nrow(rosters)] <- availSlots
        }
        if(!(newPlayer %in% rosters[,teams[t]])){
          rosters[availSlots[1],teams[t]] <- newPlayer
          teamDrafted <- c(teamDrafted, newPlayer)
        }
      }
    }
  }
  return(rosters)
}
#dResult <- setDraftResult(dfDraft,dFCast=draftForecast)
setDraftResult <- function(dfDraft,dFCast){
  dTable <- matrix(data=character(),nrow=nrow(dfDraft),ncol=ncol(dfDraft))
  colnames(dTable) <- colnames(dfDraft)
  for(i in 1:nrow(dTable)){
    for(j in 1:ncol(dTable)){
      dPick <- dFCast[dFCast$Round==i & dFCast$Team==colnames(dTable)[j],"Pick"]
      if(dFCast[dFCast$Round==i & dFCast$Team==colnames(dTable)[j],"Selected"]=="forecast"){
        dPick <- paste0("((",dPick,"))")
      }
      dTable[i,j] <- dPick
    }
  }
  return(dTable)
}

draftPopulate <- function(picksToUpdate,dfDraft){
  if(nrow(picksToUpdate) > 0){
    for(dD in 1:nrow(picksToUpdate)){#dD=1
      dPick <- picksToUpdate[dD,]
      dfDraft[dPick$round,dPick$draft_slot] <- dPick$pId
    }
  }
  return(dfDraft)
}

rotoTable_Total <- function(draftForecast, hitters, pitchers, hitterStats, pitcherStats){
  hAvg <- c('AVG','OBP','SLG')
  pAvg <- c('ERA','WHIP','K.9')
  
  hPlayers <- merge(draftForecast,hitters,by=c("id","pos","WAR","ADP","points"))
  pPlayers <- merge(draftForecast,pitchers,by=c("id","pos","WAR","ADP","points"))
  
  hTotal <- hPlayers %>% group_by(Team.x) %>% summarize_at(funs(sum, weighted.mean(.,w=AB)), .vars=hitterStats)
  colnames(hTotal) <- gsub("weighted.mean","mean",colnames(hTotal))
  hTotal <- as.data.frame(hTotal[,c('Team.x',sapply(hitterStats,function(x) paste0(x,"_",ifelse(x %in% hAvg, "mean", "sum")) ))])
  colnames(hTotal) <- c('Team',hitterStats)
  
  pTotal <- pPlayers %>% group_by(Team.x) %>% summarize_at(funs(sum, weighted.mean(.,w=IP)), .vars=pitcherStats)
  colnames(pTotal) <- gsub("weighted.mean","mean",colnames(pTotal))
  pTotal <- as.data.frame(pTotal[,c('Team.x',sapply(pitcherStats,function(x) paste0(x,"_",ifelse(x %in% pAvg, "mean", "sum")) ))])
  colnames(pTotal) <- c('Team',pitcherStats)
  
  pTotal <- merge(hTotal,pTotal)
  
  for(p in c(hAvg,pAvg)){
    pTotal[,p] <- round(pTotal[,p],3)
  }
  
  return(pTotal)
}
#draftForecast <- forecastDraft(draftResults,ff)
#rotoAll <- rotoTable_Total(draftForecast, hitters, pitchers, hitterStats, pitcherStats)
#rotoTable_Rank(rotoAll, pStats = c(hitterStats,pitcherStats))
rotoTable_Rank <- function(pTotal, pStats, pAvg = c('AVG','OBP','SLG','ERA','WHIP','K.9'), pDesc = c('SO','ERA','WHIP')){
  pRank <- pTotal; pRank[,2:ncol(pRank)] <- 0.0;
  
  for(x in pStats){#x="W"
    pStat <- pTotal[,c('Team',x)]
    pStat <- pStat[order(pStat[,x], decreasing = (x %in% pDesc)),]
    for(y in (1:nrow(pRank))){#y=1
      nVals <- which(pStat[,x]==pStat[y,x])
      pRank[pRank[,1]==pStat[y,1],x] <- ifelse(length(nVals)==1, nVals[1], sum(nVals)/length(nVals))
    }
  }
  
  pRank$Total <- sapply(1:nrow(pRank),function(x) sum(pRank[x,2:ncol(pRank)]) )
  pRank <- pRank[order(pRank$Total, decreasing = TRUE),]
  pRank <- pRank[,c(1,ncol(pRank),2:(ncol(pRank)-1))]
  rownames(pRank) <- 1:nrow(pRank)
  
  return(pRank)
}
