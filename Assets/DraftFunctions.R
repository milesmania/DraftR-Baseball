
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
    for(rF in draftResults[rForecast,'Overall']){#rF=106
      dTeam <- draftResults[rF,'Team']
      dPlayers <- subset(draftResults,Pick!="" & Team == dTeam,Pick)
      dPos <- sapply(strsplit(dPlayers$Pick, split='|', fixed=TRUE), `[`, 3)
      if(nrow(dPlayers) > 0){
        #Enforce position caps
        if(length(grep("OF",dPos)) >= 2){
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
          if(length(grep("C",dPos)) >= 2) dRestrict <- c(dRestrict,"C")
          if(length(grep("1B",dPos)) >= 2) dRestrict <- c(dRestrict,"1B")
          if(length(grep("2B",dPos)) >= 2) dRestrict <- c(dRestrict,"2B")
          if(length(grep("3B",dPos)) >= 2) dRestrict <- c(dRestrict,"3B")
          if(length(grep("SS",dPos)) >= 2) dRestrict <- c(dRestrict,"SS")
          if(length(grep("SP",dPos)) >= 3) dRestrict <- c(dRestrict,"SP")
          if(length(grep("RP",dPos)) >= 3) dRestrict <- c(dRestrict,"RP")
          if(length(grep("OF",dPos)) >= 4) dRestrict <- c(dRestrict,"OF")
          
          if(length(dRestrict) > 0) draftResults[rF,'Pick'] <- head(dFF[!grepl(paste(dRestrict,collapse = "|"),dFF$pos),'pId'],1)
        }
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
        if(!(newPlayer %in% rosters[,teams[t]]) & any(rosters[grepl(tPosP,rownames(rosters)),teams[t]]=="") ){
          availSlots <- rownames(rosters)[grepl(tPosP,rownames(rosters)) & rosters[,teams[t]] == ""]
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
  
  hTotal <- hPlayers %>% group_by(Team.x) %>% summarize_at(funs(sum, mean), .vars=hitterStats)
  hTotal <- as.data.frame(hTotal[,c('Team.x',sapply(hitterStats,function(x) paste0(x,"_",ifelse(x %in% hAvg, "mean", "sum")) ))])
  colnames(hTotal) <- c('Team',hitterStats)
  
  pTotal <- pPlayers %>% group_by(Team.x) %>% summarize_at(funs(sum, mean), .vars=pitcherStats)
  pTotal <- as.data.frame(pTotal[,c('Team.x',sapply(pitcherStats,function(x) paste0(x,"_",ifelse(x %in% pAvg, "mean", "sum")) ))])
  colnames(pTotal) <- c('Team',pitcherStats)
  
  pTotal <- merge(hTotal,pTotal)
  
  for(p in c(hAvg,pAvg)){
    pTotal[,p] <- round(pTotal[,p],3)
  }
  
  return(pTotal)
}

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
