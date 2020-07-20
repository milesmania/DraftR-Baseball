shinyServer(function(input, output, session) {
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
  
  players <- players %>% left_join(fantraxPlayerData, by = c("mlb_name" = "Player", "mlb_team" = "Team"), suffix = c("",".Fantrax"))
  
  players$fantrax_pos <- sapply(1:nrow(players), function(x){
    if(!is.na(players[x,"Position"])){
      pos <- gsub(",","/",players[x,"Position"])
    }else{
      pos <- players[x,"ottoneu_pos"]
    }
    pos
  })
  
  ff <- players[,c('playerid','Name','mlb_team','fantrax_pos','WAR','ADP','points','bats','throws','age')]
  ff_anyDups <- which(duplicated(ff$playerid))
  if(length(ff_anyDups>0)){
    ff_checkDups <- ff[ff_anyDups,"playerid"]
    ff_keep <- c(ff_anyDups,which(!(ff$playerid %in% ff_checkDups)))
    ff_keep <- ff_keep[order(ff_keep)]
    ff <- ff[ff_keep,]
  }
  
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
        pick <- which(draftResults$Round == x & draftResults$Team == teams[t])
        draftResults[pick, 'Pick'] <- as.character(dfDraft[x,t])
      }
    }
    values$dResult <- draftResults 
    values$dForecast <- forecastDraft(draftResults,ff)
    draftForecast <- values$dForecast
    
    rotoAll <- rotoTable_Total(draftForecast, hitters, pitchers, hitterStats, pitcherStats)
    if(nrow(rotoAll)>0){
      rotoRank <- rotoTable_Rank(rotoAll, pStats = c(hitterStats,pitcherStats))
      values$rotoRank <- rotoRank
      rotoTotal <- plyr::join(values$rotoRank[,c('Team','Total')],rotoAll,by='Team')
      values$rotoTotal <- rotoTotal
    }
      
    rosters <- setRoster(draftForecast,showForecast=input$chartShowForecastedRoster)
    values$rosterData <- rosters
    values$availChartY <- input$chartY
    values$availChartX <- input$chartX
    
    nextPick <- head(draftResults[draftResults$Pick=="",],1)
    values$nextPick <- nextPick
    values$picksAway <- head(subset(draftResults,Team==MyTeam & Pick=="","Overall"),1) - nextPick$Overall + 1
    
    save(dfDraft,teams,playersAvail,pitchers,hitters,rosters,availPlayers,playersTaken,draftResults,draftForecast,rotoTotal,rotoRank,playersTakenCount,StartPickTime, file = draftFile)
  })
  
  ## Refresh Draft Button ####
  observeEvent(input$RefreshDraft,{
    withProgress(message = 'Updating Draft from Fantrax', value = 0, {
      incProgress(0.5,paste('Loading Fantrax Draft ...'))
      picksToUpdate <- updateDraftFromFantrax(values$dResult,userName,passWord,leagueId,fantraxDraftFile,download_location)
    })
    if(nrow(picksToUpdate) > 0){
      dfDraft <- values$data
      for(dD in 1:nrow(picksToUpdate)){
        dPick <- picksToUpdate[dD,]
        dfDraft[dPick$Round,dPick$Fantasy.Team] <- dPick$pId
      }
      values$data <- dfDraft
    }
  })
  
  ## Save Settings Button ####
  observeEvent(input$saveSettings,{
    withProgress(message = 'Saving Settings', value = 0, {
      incProgress(0.5,paste('Saving Config Settings ...'))
      userName <- input$userName
      passWord <- input$passWord
      leagueId <- input$leagueId
      MyTeam <- input$myTeam
      
      config <- c('userName'=userName,'passWord'=passWord,'leagueId'=leagueId,'MyTeam'=MyTeam)
      configTxt <- paste(sapply(1:length(config),function(x){paste(names(config[x]),config[x], sep = "=")}),
                         collapse = "\n")
      write(configTxt,file = "Assets/config.txt")
      Sys.sleep(1)
    })
  })
  
  ## Output updates ###########################################
  
  output$data = renderRHandsontable({
    rhandsontable(values$data, stretchH = "all") %>%
      hot_cols(colWidths = 100, type="autocomplete", source = values$pAvail) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  output$dataAvail = DT::renderDataTable({
    draftForecast <- values$dForecast
    dtF <- datatable(values$dataAvail[,c('name','pos','team','age','ADP','points')], rownames = FALSE,
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
    datatable(values$dataAvail, rownames = FALSE, options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 25)) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("SP","RP","OF","C","1B","SS"),
                                               values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
    #%>% formatRound(columns = colnames(values$dataAvail)[8:35])
  })
  
  output$dataAvailHitters = DT::renderDataTable({
    aHitters <- values$availHitters
    aHitters <- aHitters[order(aHitters$ADP),]
    rownames(aHitters) <- NULL
    datatable(aHitters, 
              options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 10, order=list(which(colnames(hitters)=='ADP'),'asc'))) %>%
      formatStyle("pos",target = 'row',
                  backgroundColor = styleEqual(levels=c("2B","3B","OF","C","1B","SS"),
                                               values=c("pink","lightgreen","lightblue","orange","violet","lightgrey")))
    #%>% formatRound(columns = colnames(values$dataAvail)[8:35])
  })
  
  output$dataAvailPitchers = DT::renderDataTable({
    datatable(values$availPitchers,  
              options = list(lengthMenu = c(100, 50, 25, 10), pageLength = 10, order=list(which(colnames(pitchers)=='ADP'),'asc'))) %>%
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

