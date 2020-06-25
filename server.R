library(rhandsontable)
library(DT)
library(rjson)

source("C:/Users/rmiles/Documents/School/R/snlLoadPackages.R")
source("C:/Users/rmiles/Documents/School/R/snlFunctions.R")
source("C:/Users/rmiles/Documents/School/R/snlCharts.R")
source("C:/Users/rmiles/Documents/School/R/MiningModelClass.R")

shinyServer(function(input, output, session) {
  
  #fffile <- "C:/Users/rmiles/Downloads/ffa_customrankings2017-3.csv"
  #accessed from:  https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=fangraphsdc
  pitchers <- read.csv("C:/Users/rmiles/Downloads/FanGraphs-Pitchers.csv",stringsAsFactors = F); colnames(pitchers)[1] <- "Name"
  hitters <- read.csv("C:/Users/rmiles/Downloads/FanGraphs-Hitters.csv",stringsAsFactors = F); colnames(hitters)[1] <- "Name"
  #accessed from: http://crunchtimebaseball.com/baseball_map.html
  master <- read.csv("C:/Users/rmiles/Downloads/master.csv",stringsAsFactors = F)
  
  draftFile <- "C:/Users/rmiles/Documents/School/R/FantasyBaseball/FFDraftData.RData"
  #draftId <- 340916602432159744 #338482281133907968 #340196925133344768 #340208591887724544
  #teams <- c('Harvey','Leo','Eric','Ryan','Forrest','Sue','Emily','Julianne','Deb','Dom')
  #teams <- c('Ben','James','Nannah','Nick','Higgins','Cliff','Joel','Nate','Inman','Miles','Cusick','Aaron')
  teams <- c('Sweeney','Cusick','Joel','Inman','Nate','Jackson','Heun','Liechty','Miles')
  MyTeam <- 'Miles'
  rosterPositions <- c('C-1','1B-1','2B-1','3B-1','SS-1','OF-1','OF-2','OF-3','SP-1','SP-2','RP-1','RP-2','P-1','BE-1','BE-2','BE-3')
  
  qProbs <- c(0,.4,.6,.8,.9,.95,.98,1)
  
  hitterStats <- c('HR','RBI','SB','AVG','R','SO','OBP','SLG'); hDesc <- c('SO')
  n <- nrow(hitters)
  for(i in 1:length(hitterStats)){#i=7
    s <- hitterStats[i]
    
    qBreaks <- quantile(hitters[,s], probs=qProbs)
    qLabels <- c('0','1','3','5','7','9','10')
    
    if(length(qBreaks)>length(unique(qBreaks))){
      nQ <- c(1)
      for(q in 1:(length(qBreaks)-1)){
        if(qBreaks[q] != qBreaks[q+1]) nQ <- c(nQ,q+1)
      }
      qBreaks <- qBreaks[nQ]
      qLabels <- qLabels[nQ-1]
    }
    
    if(s %in% hDesc){
      #hitters[,paste0("R_",s)] <- (1-rank(hitters[,s])/n)*10
      qLabels <- qLabels[length(qLabels):1]
    }
    hitters[,paste0("R_",s)] <- cut(x=hitters[,s], breaks = qBreaks, labels=qLabels)
    hitters[,paste0("R_",s)] <- as.numeric(as.character(hitters[,paste0("R_",s)]))
  }
  hitters$points <- sapply(1:n,function(x) sum(hitters[x,grep("R_",colnames(hitters))],na.rm=T) )
  
  pitcherStats <- c('W','ERA','WHIP','SVHD','K.9'); pDesc <- c('ERA','WHIP')
  pitchers$SVHD <- pitchers$SV + pitchers$HLD
  
  n <- nrow(pitchers)
  for(i in 1:length(pitcherStats)){#i=1
    s <- pitcherStats[i]
    
    qBreaks <- quantile(pitchers[,s], probs=qProbs)
    qLabels <- c('0','1','3','5','7','9','10')
    
    if(length(qBreaks)>length(unique(qBreaks))){
      nQ <- c(1)
      for(q in 1:(length(qBreaks)-1)){
        if(qBreaks[q] != qBreaks[q+1]) nQ <- c(nQ,q+1)
      }
      qBreaks <- qBreaks[nQ]
      qLabels <- qLabels[nQ-1]
    }
    
    if(s %in% pDesc){
      qLabels <- qLabels[length(qLabels):1]
    }
    pitchers[,paste0("R_",s)] <- cut(x=pitchers[,s], breaks = qBreaks, labels=qLabels)
    pitchers[,paste0("R_",s)] <- as.numeric(as.character(pitchers[,paste0("R_",s)]))
  }
  pitchers$points <- sapply(1:n,function(x) sum(pitchers[x,grep("R_",colnames(pitchers))],na.rm=T) )
  
  nRounds <- length(rosterPositions)
  availChartY <- "rank"
  availChartX <- "points"
  
  master1 <- master[,grep('mlb|bats|throws|birth_year|pos|fg',colnames(master))]
  master1$age <- lubridate::year(Sys.Date()) - master1$birth_year
  players <- rbind(hitters[,c('Name','Team','WAR','ADP','points','playerid')],pitchers[,c('Name','Team','WAR','ADP','points','playerid')])
  players <- merge(players,master1,by.x='playerid',by.y='fg_id')
  players <- players[order(players$ADP),]
  
  ff <- players[,c('playerid','Name','mlb_team','ottoneu_pos','WAR','ADP','points','bats','throws','age')]
  
  colnames(ff)[1:4] <- c("id", "name","team","pos")
  
  ff$pId <- unlist(sapply(1:nrow(ff),function(x) paste(ff[x,'name'],ff[x,'team'],ff[x,'pos'],sep="|")))
  
  pitchers <- merge(ff[,c("id", "pId", "pos")],pitchers,by.x='id',by.y='playerid')
  hitters <- merge(ff[,c("id", "pId", "pos")],hitters,by.x='id',by.y='playerid')
  
  pitchers <- pitchers[order(pitchers$points),]
  hitters <- hitters[order(hitters$points),]
  
  #Set up ordered table
  fft<-ff[,c("pos","name","team","age","points","WAR","ADP")]
  
  #Set-Up rankings table 
  ffd<-ff[,c("id","pos","name","points","WAR","ADP")]
  
  ## Load File or Setup New ###########################################  
  if(file.exists(draftFile)){
    load(draftFile)
  }else{
    playersAvail <- unique(ff$pId)
    playerLevels <- c("",playersAvail)
    availPlayers = factor(rep("",nRounds),levels=playerLevels)
    #ff$pId <- factor(ff$pId,levels=playerLevels)
    playersTaken <- character()
    
    dfDraft = data.frame(matrix(availPlayers,length(availPlayers),length(teams)))
    colnames(dfDraft) <- teams
    
    rosters <- data.frame(matrix(availPlayers,length(availPlayers),length(teams)), stringsAsFactors = F)
    colnames(rosters) <- teams; rownames(rosters) <- rosterPositions; #rosters[,] <- ""
    
    rotoTotal <- data.frame()
    rotoRank <- data.frame()
    
    draftResults <- data.frame('Overall'=integer(),'Round'=integer(),'Team'=character(),'Pick'=character(), stringsAsFactors = F)
    n <- 0
    for(x in 1:nRounds){#x=1
      if(IsOdd(x)){
        roundOrder <- 1:length(teams)
      }else{
        roundOrder <- length(teams):1
      }
      for(t in roundOrder){#t=1
        n <- n + 1
        draftResults[n,'Overall'] <- n
        draftResults[n,'Round'] <- x
        draftResults[n,'Team'] <- teams[t]
      }
    }
    draftResults$Pick <- ""
    draftForecast <- forecastDraft(draftResults,ff)
    
    StartPickTime <- Sys.time()# + 4*60
    playersTakenCount <- 0
    
    save(dfDraft,teams,playersAvail,pitchers,hitters,rosters,availPlayers,playersTaken,draftResults,draftForecast,rotoTotal,rotoRank,playersTakenCount,StartPickTime, file = draftFile)
  }
  
  ## Reactives ###########################################
  
  values = reactiveValues(data = dfDraft, 
                          pAvail = playersAvail,
                          pTaken = playersTaken,
                          pTakenCount = playersTakenCount,
                          dataAvail = ff,
                          availHitters = hitters,
                          availPitchers = pitchers,
                          rosterData = rosters,
                          dResult = draftResults,
                          fResult = draftForecast,
                          rotoTotal = rotoTotal,
                          rotoRank = rotoRank,
                          availChartX = availChartX,
                          availChartY = availChartY,
                          StartPickTime = StartPickTime,
                          nextPick = head(draftResults[draftResults$Pick=="",],1),
                          picksAway = 0
                          )

  observe({
    #set up autocomplete on drafting table
    req(input$data)
    dfDraft <- hot_to_r(input$data)
    values$data <- dfDraft
    
    #Get playerTaken and players available
    playersTaken <- as.character(unlist(as.list(dfDraft)))
    playersTaken <- playersTaken[playersTaken != ""]
    if(length(playersTaken) != values$pTakenCount){
      values$StartPickTime <- Sys.time()# + 4*60
      playersTakenCount <- length(playersTaken)
      values$pTakenCount <- playersTakenCount
    }
    playersAvail <- as.character(unique(ff$pId))
    playersAvail <- playersAvail[!(playersAvail %in% playersTaken)]
    values$pAvail <- playersAvail
    values$pTaken <- playersTaken
    
    dfAvail <- ff
    values$dataAvail = dfAvail[!(dfAvail$pId %in% playersTaken),]
    values$availHitters = hitters[!(hitters$pId %in% playersTaken),]
    values$availPitchers = pitchers[!(pitchers$pId %in% playersTaken),]
    
    #update draft results
    for(x in 1:nRounds){#x=1
      for(t in 1:length(teams)){#t=1
        pick <- index(draftResults)[draftResults$Round == x & draftResults$Team == teams[t]]
        draftResults[pick, 'Pick'] <- as.character(dfDraft[x,t])
      }
    }
    values$dResult <- draftResults 
    values$dForecast <- forecastDraft(draftResults,ff)
    draftForecast <- values$dForecast
    
    rotoAll <- rotoTable_Total(values$dForecast, hitters, pitchers, hitterStats, pitcherStats)
    values$rotoRank <- rotoTable_Rank(rotoAll, pStats = c(hitterStats,pitcherStats))
    values$rotoTotal <- plyr::join(values$rotoRank[,c('Team','Total')],rotoAll)
      
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    values$rosterData <- rosters
    values$availChartY <- input$chartY
    values$availChartX <- input$chartX
    
    nextPick <- head(draftResults[draftResults$Pick=="",],1)
    values$nextPick <- nextPick
    values$picksAway <- head(subset(draftResults,Team==MyTeam & Pick=="","Overall"),1) - nextPick$Overall + 1
    
    save(dfDraft,teams,playersAvail,pitchers,hitters,rosters,availPlayers,playersTaken,draftResults,draftForecast,rotoTotal,rotoRank,playersTakenCount,StartPickTime, file = draftFile)
  })
  
  observeEvent(input$RefreshDraft,{
    picksToUpdate <- updateDraftFromSleeper(draftId,values$dResult)
    if(nrow(picksToUpdate) > 0){
      dfDraft <- values$data
      for(dD in 1:nrow(picksToUpdate)){
        dPick <- picksToUpdate[dD,]
        dfDraft[dPick$round,dPick$draft_slot] <- dPick$pId
      }
      values$data <- dfDraft
    }
  })
  
  ## Output updates ###########################################
  
  output$data = renderRHandsontable({
    rhandsontable(values$data, stretchH = "all") %>%
      hot_cols(colWidths = 100, type="autocomplete", source = values$pAvail) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  output$dataAvail = DT::renderDataTable({
    draftForecast <- values$dForecast
    dtF <- datatable(values$dataAvail[,c('name','pos','team','age','ADP','points')], 
              options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 50)) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("SP","RP","OF","C","1B","SS"),
                                               values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
    
    if("Team" %in% names(draftForecast)){
      myPlayers <- subset(draftForecast,Team==MyTeam & Selected=="forecast","name",drop=T)
      dtF <- dtF %>% 
        formatStyle(columns = "name", border = styleEqual(levels=myPlayers, values=rep('3px dashed red',length(myPlayers))))
    }
    dtF
    })
  
  output$dataAvailALL = DT::renderDataTable({
    datatable(values$dataAvail, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("SP","RP","OF","C","1B","SS"),
                                               values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
    #%>% formatRound(columns = colnames(values$dataAvail)[8:35])
  })
  
  output$dataAvailHitters = DT::renderDataTable({
    datatable(values$availHitters, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 10, order=list(which(colnames(hitters)=='ADP'),'asc'))) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("2B","3B","OF","C","1B","SS"),
                                               values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
    #%>% formatRound(columns = colnames(values$dataAvail)[8:35])
  })
  
  output$dataAvailPitchers = DT::renderDataTable({
    datatable(values$availPitchers, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 10, order=list(which(colnames(pitchers)=='ADP'),'asc'))) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("SP","RP","SP/RP"),
                                               values=c("pink","lightblue","lightgrey")))
    #%>% formatRound(columns = colnames(values$dataAvail)[8:35])
  })
  
  
  output$rosterData = renderTable({values$rosterData}, rownames = TRUE, striped = TRUE, width = "100%")
  
  output$rotoRank = DT::renderDataTable({values$rotoRank}, options = list(paging = FALSE, searching = FALSE, pageLength = 'All', order=list(2,'desc')))
  output$rotoTotal = DT::renderDataTable({values$rotoTotal}, options = list(paging = FALSE, searching = FALSE, pageLength = 'All', order=list(2,'desc')))
  
  # output$draftForecasted = renderRHandsontable({
  #   dTable <- values$data
  #   dFCast <- values$dForecast
  #   dComments <- matrix(data=NA,nrow=nrow(dTable),ncol=ncol(dTable))
  #   for(i in 1:nrow(dTable)){
  #     for(j in 1:ncol(dTable)){
  #       dTable[i,j] <- dFCast[dFCast$Round==i & dFCast$Team==colnames(dTable)[j],"Pick"]
  #       if(dFCast[dFCast$Round==i & dFCast$Team==colnames(dTable)[j],"Selected"]=="forecast"){
  #         dComments[i,j] = "Forecast"
  #       }
  #     }
  #   }
  #   rhandsontable(dTable, stretchH = "all", colWidths = 100, readOnly = TRUE, comments = dComments)
  # })
  
  output$draftForecasted = renderTable({setDraftResult(dfDraft, values$dForecast)}, rownames = TRUE, striped = TRUE, width = "100%")
  
  output$draftData = DT::renderDataTable({
    datatable(values$dForecast, options = list(lengthMenu = c(100, 50, 25, 10),
                                             columnDefs = list(list(visible = FALSE, targets = 5:ncol(values$dForecast))), 
                                             pageLength = 50)) %>%
      formatStyle(valueColumns="Selected",target = 'cell',columns = "Pick",color = styleEqual(levels="forecast",values="lightblue"))
  })
  
  output$draftTotalChart = renderPlot({draftChart(values$dForecast)},height = 500)
  
  
  output$nextPick <- renderUI({
    draftResults <- values$dResult
    nextPick <- values$nextPick
    dTxt <- paste("<h4>Round",nextPick$Round," Pick",rownames(nextPick),"On the Clock:",nextPick$Team,"</h4>")
    if(nextPick$Team!=MyTeam) dTxt <- paste0(dTxt," <i>Picks until ",MyTeam,": ",values$picksAway,"</i>")
    HTML(dTxt)
  })
  
  output$pickTimeElapsed <- renderText({
    invalidateLater(1000, session)
    paste("Time Since Last Pick:", 
          round(difftime(Sys.time(), values$StartPickTime, units='secs')), 'seconds')
  })
  
})

## User Functions ###########################################

#dForcast <- forecastDraft(draftResults,ff); dRosters <- setRoster(draftedPlayers=dForcast)
forecastDraft <- function(draftResults,ff){
  draftResults$Selected <- ifelse(draftResults$Pick!="","selected","forecast")
  rForecast <- draftResults$Selected!="selected"
  dFF <- ff[!(ff$pId %in% draftResults[!rForecast,'Pick']),]
  if(any(rForecast)){
    for(rF in draftResults[rForecast,'Overall']){#rF=101
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

draftChart <- function(dForcast){
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
#dResult <- setDraftResult(dfDraft,draftForecast)
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

updateDraftFromFantrax <- function(draftId,draftResults){#draftId='nc30j3mnjsuzwnaa'
  webUrl <- paste0("https://www.fantrax.com/newui/fantasy/draftResultsPopup.go?leagueId=",draftId)
  webpage <- read_html(webUrl)
  
}

