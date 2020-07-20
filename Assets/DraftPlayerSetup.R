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