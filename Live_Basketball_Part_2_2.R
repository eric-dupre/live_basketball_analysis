############################################################################################################################
# Here we begin the process of scraping the box score and play by play from the site specified above.
#
# In this block, R will open an internet window with Firefox and navigate to your game site. If you get errors in this block, 
# increase the number in the Sys.sleep functions (specifying the number of seconds to pause before proceeding).
#############################################################################################################################
RSelenium::startServer()
remDr <- remoteDriver()
remDr$open()
remDr$navigate(site)
Sys.sleep(2)
webElems <- remDr$findElements(using = 'css selector', ".qtrh")
period <- unlist(lapply(webElems, function(x){x$getElementText()}))

if (user_control==FALSE) {
  if (period == "F" & exists("lineupboxscore")) {half <- 2
  } else if (period == "PRD 2") {half <- 2
  } else if (period == "PRD 1" | (period == "F" & exists("lineupboxscore")== FALSE)) {half <- 1
  } else half <- 3
}


############################################################################################################################
# In this segment, R will navigate to the home box score and click the full game tab. Then we save the box score into a 
# data frame named 'homeboxscore' to be used in later analyses. Note that each time the program is run, the home box score is  
# replaced with the most current version. So if you run the program with 10:00 in the first half and again at halftime, the 
# box score will reflect everything up to halftime, so you will be unable to determine what stats happened in the first 10 
# minutes compared to the second 10 minutes.
#############################################################################################################################
webElem <- remDr$findElement(using = "css selector", '#bb_b2')
remDr$mouseMoveToLocation(webElement = webElem)
remDr$click(1)
Sys.sleep(2)
doc <- remDr$getPageSource()[[1]]
raw_home_box <- readHTMLTable(doc)
homeboxscore <- as.data.frame(raw_home_box$`NULL`)
homeboxscore$PF <- as.numeric(str_extract(homeboxscore$PF, "[0-9]")) 
homeboxscore <- homeboxscore[c(1:nrow(homeboxscore)-1),]


hspecial_stats <- as.data.frame(raw_home_box$teamspecialty_table)
hspecial_stats <- transpose(hspecial_stats)
colnames(hspecial_stats) <- hspecial_stats[1, ]
hspecial_stats <- hspecial_stats[-1,]
hspecial_stats[,1:15] <- sapply(hspecial_stats[,1:15], as.numeric)
hspmin <- as.numeric(str_extract(str_extract(hspecial_stats[,16:18], "[0-9]*:"), "[0-9]*"))
hspsec <-as.numeric(str_extract(str_extract(hspecial_stats[,16:18], ":[0-9]*"), "[0-9]{2}"))
hsptime <- hspmin+(hspsec/60)
hspecial_stats[,tail(names(hspecial_stats),3)] <- hsptime

############################################################################################################################
# In this section, we do the same processing for the away box score. Again, R will navigate the browser to the away box score
# and select the full game tab. The away box score will be saved into a data frame named 'oppboxscore'.
#############################################################################################################################

webElem <- remDr$findElement(using = "css selector", '#bb_b3')
remDr$mouseMoveToLocation(webElement = webElem)
remDr$click(1)
Sys.sleep(2)
doc <- remDr$getPageSource()[[1]]
raw_away_box <- readHTMLTable(doc)
oppboxscore <- as.data.frame(raw_away_box$`NULL`)
oppboxscore$PF <- as.numeric(str_extract(oppboxscore$PF, "[0-9]")) 
oppboxscore <- oppboxscore[c(1:nrow(oppboxscore)-1),]


aspecial_stats <- as.data.frame(raw_away_box$teamspecialty_table)
aspecial_stats <- transpose(aspecial_stats)
colnames(aspecial_stats) <- aspecial_stats[1, ]
aspecial_stats <- aspecial_stats[-1,]
aspecial_stats[,1:15] <- sapply(aspecial_stats[,1:15], as.numeric)
aspmin <- as.numeric(str_extract(str_extract(aspecial_stats[,16:18], "[0-9]*:"), "[0-9]*"))
aspsec <-as.numeric(str_extract(str_extract(aspecial_stats[,16:18], ":[0-9]*"), "[0-9]{2}"))
asptime <- aspmin+(aspsec/60)
aspecial_stats[,tail(names(aspecial_stats),3)] <- asptime

############################################################################################################################
# In this block of code, we scrape and clean the play by play information so that we can perform analyses on each lineup
# that the home team plays. The first few lines naviagte the Firefox browser to the Play by Play tab of your game. Then, if
# you specify half 1 above, R will navigate to the first half play by play and scrape it. Similarly, if you are running for the 
# second half, the second half play by play will be scraped. Then, R will close the browser and begin processing the raw play by 
# play to remove special characters and clean some of the formatting. The final version will be saved to the character vector, 
# "playbyplay".
#############################################################################################################################

webElem <- remDr$findElement(using = "css selector", '#bb_b6')
remDr$mouseMoveToLocation(webElement = webElem)
remDr$click(1)
Sys.sleep(1)
if (half==1) {
  webElem <- remDr$findElement(using = "css selector", ".ui-state-highlight+ .ui-button-text-only .font4")
}
if (half==2) {
  webElem <- remDr$findElement(using = "css selector", ".ui-state-highlight+ .ui-button-text-only+ .ui-button-text-only .font4")
}
remDr$mouseMoveToLocation(webElement = webElem)
remDr$click(1)
Sys.sleep(2)
doc <- remDr$getPageSource()[[1]]
current_doc <- readHTMLTable(doc)
raw_playbyplay <- data.frame(current_doc$`NULL`, stringsAsFactors = FALSE)
raw_playbyplay <- raw_playbyplay[,c(1:3,5)]
remDr$closeWindow()
playbyplay <- as.vector(NA)
away_team <- colnames(raw_playbyplay[1])
home_team <- colnames(raw_playbyplay[4])

time <- 20

if (home==TRUE) {
  ourteam <- home_team
  against <- away_team
} else {
  ourteam <- away_team
  against <- home_team}

for (i in 1:nrow(raw_playbyplay)) {
  playbyplay[i] <- paste(raw_playbyplay[i,1], raw_playbyplay[i,2], raw_playbyplay[i,3], raw_playbyplay[i,4], sep = " ")
}

playbyplay <- gsub("Ã", " ", playbyplay)
playbyplay <- gsub("Â", " ", playbyplay)
playbyplay <- gsub(",", "", playbyplay)
playbyplay <- gsub("  ", " ", playbyplay)

############################################################################################################################
# At this point, all of the pre-processing is complete. Now we have three objects that will be manipulated and processed, 
# 'homeboxscore', 'oppboxscore', and 'playbyplay'. The two box scores will be used to create a game score for each player, on 
# both your team and the opposing team. For in-game purposes, we scale the game score calculation to indicate what a player's
# game score would be if they had the same level of productivity for the whole game. But actual game scores are retained for  
# post-game use. 
#############################################################################################################################

############################################################################################################################
# IN GAME PROCESSING
############################################################################################################################

############################################################################################################################
#
# Here we process the home team box score. We edit the FG, FT, and 3PT columns to remove the '-' from the 'MADE-ATTEMPTED'
# format and split into two columns. Then we calculate each player's game score and we add scaled game score. We scale the 
# game score such that the scaled game score represents the game score a player would have if they maintained their productivity
# level for 40 minutes. So, for a player that has played 10 minutes, their game score will be multiplied by 4 to get their scaled
# game score.
#
#############################################################################################################################
homeboxscore$FGMade <- as.numeric(str_extract(str_extract(homeboxscore$FG, "[0-9]*-"), "[0-9]*"))
homeboxscore$FGAtt<- abs(as.numeric(str_extract(homeboxscore$FG, "-[0-9]*")))
homeboxscore$ThreeFGMade <- as.numeric(str_extract(str_extract(homeboxscore$`3FG`, "[0-9]*-"), "[0-9]*"))
homeboxscore$ThreeFGAtt <- abs(as.numeric(str_extract(homeboxscore$`3FG`, "-[0-9]*")))
homeboxscore$FtMade<- as.numeric(str_extract(str_extract(homeboxscore$FT, "[0-9]*-"), "[0-9]*"))
homeboxscore$FtAtt <- abs(as.numeric(str_extract(homeboxscore$FT, "-[0-9]*")))

homeboxscore <- homeboxscore[,c(1:3,7:22)]
for (i in 3:ncol(homeboxscore)) {homeboxscore[,i] <- as.numeric(homeboxscore[,i])}
homeboxscore$GmScr <- (homeboxscore$PTS + .4 * homeboxscore$FGMade - .7 * homeboxscore$FGAtt - .4 * (homeboxscore$FtAtt - homeboxscore$FtMade) + .7 * homeboxscore$OR + .3 * homeboxscore$DR + homeboxscore$ST + .7 * homeboxscore$A + .7 * homeboxscore$BL - .4 * homeboxscore$PF - homeboxscore$TO)
homeboxscore$ScaleGmScr <- homeboxscore$GmScr *(40/homeboxscore$MIN)

############################################################################################################################
#
# Here we do the same processing for the opponent box score. Then we put an indicator variable to denote your players vs 
# the opponents so that you can analyze just your players or just opponents. 
#
#############################################################################################################################
oppboxscore$FGMade <- as.numeric(str_extract(str_extract(oppboxscore$FG, "[0-9]*-"), "[0-9]*"))
oppboxscore$FGAtt<- abs(as.numeric(str_extract(oppboxscore$FG, "-[0-9]*")))
oppboxscore$ThreeFGMade <- as.numeric(str_extract(str_extract(oppboxscore$`3FG`, "[0-9]*-"), "[0-9]*"))
oppboxscore$ThreeFGAtt <- abs(as.numeric(str_extract(oppboxscore$`3FG`, "-[0-9]*")))
oppboxscore$FtMade<- as.numeric(str_extract(str_extract(oppboxscore$FT, "[0-9]*-"), "[0-9]*"))
oppboxscore$FtAtt <- abs(as.numeric(str_extract(oppboxscore$FT, "-[0-9]*")))

oppboxscore <- oppboxscore[,c(1:3,7:22)]
for (i in 3:ncol(oppboxscore)) {oppboxscore[,i] <- as.numeric(oppboxscore[,i])}
oppboxscore$GmScr <- (oppboxscore$PTS + .4 * oppboxscore$FGMade - .7 * oppboxscore$FGAtt - .4 * (oppboxscore$FtAtt - oppboxscore$FtMade) + .7 * oppboxscore$OR + .3 * oppboxscore$DR + oppboxscore$ST + .7 * oppboxscore$A + .7 * oppboxscore$BL - .4 * oppboxscore$PF - oppboxscore$TO)
oppboxscore$ScaleGmScr <- oppboxscore$GmScr * (40/oppboxscore$MIN)

#create binary variable to indicate your players or opponent players against you. 
if (home==TRUE) {
  homeboxscore$ourplayers <- 1
  oppboxscore$ourplayers <- 0
} else {
  homeboxscore$ourplayers <- 0
  oppboxscore$ourplayers <- 1
}


############################################################################################################################
#
# Now we're moving to process the play by play information.
#
#############################################################################################################################



############################################################################################################################
# 
# First, we analyze the play by play to get a lineup box score of the home team. We get the starting lineup from the home box
# score (players starting are denoted with a star in their number column). We call the process_pbp function and subset out 
# the relevant information we want to print. After this block, we will have the lineup box score (home_lineupboxscore) and 
# the info to print (home_lineup_to_print).
#
#############################################################################################################################


lineup <- homeboxscore[grepl("\\Q*\\E",homeboxscore$`#`), 2] 
lineup <- as.character(lineup)
lineup <- gsub(" III", "",lineup)
lineup <- gsub(" II", "", lineup)
lineup <- lineup[order(lineup)]

process_pbp(home_team)
if (half==1) {
  home_lineupboxscore <- lineupboxscore 
  lineup_to_print <- home_lineupboxscore
  lineup_to_print <- lineup_to_print[!is.na(lineup_to_print$Game_Score),]
  lineup_to_print <- ddply(home_lineupboxscore, .(Name), summarize, Minutes=sum(Minutes), Plus_minus=sum(Plus_Minus), Game_Score=sum(Game_Score))
  lineup_to_print <- lineup_to_print[rev(order(lineup_to_print$Game_Score)),]
  home_lineup_to_print <- lineup_to_print
} else if (half==2) {
  home_lineupboxscore <- rbind(home_lineupboxscore[home_lineupboxscore$half==1,], lineupboxscore)
  lineup_to_print <- home_lineupboxscore
  lineup_to_print <- lineup_to_print[!is.na(lineup_to_print$Game_Score),]
  lineup_to_print <- ddply(home_lineupboxscore, .(Name), summarize, Minutes=sum(Minutes), Plus_minus=sum(Plus_Minus), Game_Score=sum(Game_Score))
  lineup_to_print <- lineup_to_print[rev(order(lineup_to_print$Game_Score)),]
  home_lineup_to_print <- lineup_to_print
  }

############################################################################################################################
# 
# Here we do the same processing for the away team. Then we aggregate the away and home box scores into the object 'lineupboxscore'
#
#############################################################################################################################

lineup <- oppboxscore[grepl("\\Q*\\E",oppboxscore$`#`), 2]
lineup <- as.character(lineup)
lineup <- gsub(" III", "", lineup)
lineup <- gsub(" II", "", lineup)
lineup <- lineup[order(lineup)]

process_pbp(away_team)
if (half==1){
  away_lineupboxscore <- lineupboxscore
  lineup_to_print <- away_lineupboxscore
  lineup_to_print <- lineup_to_print[!is.na(lineup_to_print$Game_Score),]
  lineup_to_print <- ddply(away_lineupboxscore, .(Name), summarize, Minutes=sum(Minutes), Plus_minus=sum(Plus_Minus), Game_Score=sum(Game_Score))
  lineup_to_print <- lineup_to_print[rev(order(lineup_to_print$Game_Score)),]
  away_lineup_to_print <- lineup_to_print
  
} else if (half ==2) {
  away_lineupboxscore <- rbind(away_lineupboxscore[away_lineupboxscore$half==1,], lineupboxscore)
  lineup_to_print <- away_lineupboxscore
  lineup_to_print <- lineup_to_print[!is.na(lineup_to_print$Game_Score),]  
  lineup_to_print <- ddply(away_lineupboxscore, .(Name), summarize, Minutes=sum(Minutes), Plus_minus=sum(Plus_Minus), Game_Score=sum(Game_Score))
  lineup_to_print <- lineup_to_print[rev(order(lineup_to_print$Game_Score)),]
  away_lineup_to_print <- lineup_to_print
  
  }


lineupboxscore <- rbind(away_lineupboxscore, home_lineupboxscore)  

############################################################################################################################
# 
# Here we do the processing for team stats for home and away teams. We create the columns of this data frame by aggregating 
# the corresponding column from the lineup box score table. 
#
#############################################################################################################################
home_teamstats <- data.frame(AST=sum(home_lineupboxscore$A),STL=sum(home_lineupboxscore$ST), TO=sum(home_lineupboxscore$TO), PF=sum(home_lineupboxscore$PF), BL=sum(home_lineupboxscore$BL), DRB=sum(home_lineupboxscore$DR), ORB=sum(home_lineupboxscore$OR),Threept=sum(home_lineupboxscore$Threept), FT=sum(home_lineupboxscore$FT),
                             FTA=sum(home_lineupboxscore$FTA), MISSFT=sum(home_lineupboxscore$MISSFT), FG=sum(home_lineupboxscore$FGM), FGA=sum(home_lineupboxscore$FGA), ourPoints=sum(home_lineupboxscore$PTS), OppPoints=sum(home_lineupboxscore$Oppspts),
                             Plus_Minus=sum(home_lineupboxscore$Plus_Minus), OppDR=sum(home_lineupboxscore$OppDR), OppFG=sum(home_lineupboxscore$OppFG), OppFGA=sum(home_lineupboxscore$OppFGA), OppORB=sum(home_lineupboxscore$OppORB),
                             OppTO=sum(home_lineupboxscore$OppTO), OppFT=sum(home_lineupboxscore$OppFT), OppFTA=sum(home_lineupboxscore$OppFTA), fthack=sum(home_lineupboxscore$fthack), TmDfRt=sum(home_lineupboxscore$TmDfRt), team=home_team, against=against)

away_teamstats <- data.frame(AST=sum(away_lineupboxscore$A),STL=sum(away_lineupboxscore$ST), TO=sum(away_lineupboxscore$TO), PF=sum(away_lineupboxscore$PF), BL=sum(away_lineupboxscore$BL), DRB=sum(away_lineupboxscore$DR), ORB=sum(away_lineupboxscore$OR),Threept=sum(away_lineupboxscore$Threept), FT=sum(away_lineupboxscore$FT),
                             FTA=sum(away_lineupboxscore$FTA), MISSFT=sum(away_lineupboxscore$MISSFT), FG=sum(away_lineupboxscore$FGM), FGA=sum(away_lineupboxscore$FGA), ourPoints=sum(away_lineupboxscore$PTS), OppPoints=sum(away_lineupboxscore$Oppspts),
                             Plus_Minus=sum(away_lineupboxscore$Plus_Minus), OppDR=sum(home_lineupboxscore$DR), OppFG=sum(home_lineupboxscore$FGM), OppFGA=sum(away_lineupboxscore$OppFGA), OppORB=sum(away_lineupboxscore$OppORB),
                             OppTO=sum(away_lineupboxscore$OppTO), OppFT=sum(away_lineupboxscore$OppFT), OppFTA=sum(away_lineupboxscore$OppFTA), fthack=sum(away_lineupboxscore$fthack), TmDfRt=sum(away_lineupboxscore$TmDfRt), team=away_team, against=against)

TmDfRt(home_teamstats)
home_teamstats <- cbind(teamstats, hspecial_stats)
TmDfRt(away_teamstats)
away_teamstats <- cbind(teamstats, aspecial_stats)
teamstats <- rbind(home_teamstats,away_teamstats)

############################################################################################################################
#
# Now that we have processed all of the data we need in our analyses, we are going to add columns to the individual box scores
# so that we can do season-long analysis. In particular, we add columns to specify the opponent and final score, from the 
# play by play data in this game. Then we sort and segment the box scores to create data frames with only the data we want to 
# print:Player number, player name, minutes played, game score, and plus minus. The basic stats are available to coaches in game
# so we did not find it useful to add that data in these printouts. 
#
#############################################################################################################################
oppboxscore$against <- against
oppboxscore$ourscore <- ourrun
oppboxscore$oppscore <- opprun
homeboxscore$against <- against
homeboxscore$ourscore <- ourrun
homeboxscore$oppscore <- opprun
homeboxscore <- homeboxscore[order(homeboxscore$ScaleGmScr, decreasing = T), ]
oppboxscore <- oppboxscore[order(oppboxscore$ScaleGmScr, decreasing = T), ]

homeprint <- homeboxscore[c(1,2,12,21,13)]

homenewnames <- c("Home Number", "Player", "Mins", "Game Score", "Plus Minus")
colnames(homeprint) <- homenewnames
homeprint$`Game Score` <- round(homeprint$`Game Score`, digits = 2)


Oppprint <- oppboxscore[c(1,2,12,21,13)]
Oppnewnames <- c("Away Number", "Player", "Mins", "Game Score", "Plus Minus")
colnames(Oppprint) <- Oppnewnames
Oppprint$`Game Score` <- round(Oppprint$`Game Score`, digits = 2)


############################################################################################################################
#
# Here we output our results to 4 files. The first is a text file with the summary information from both teams at the 
# individual and lineup levels. Then we export .csv files for individual, lineup, and team stats.
#
#############################################################################################################################
write.fwf(homeprint, file="print.txt", eol="\r\n",sep="          ")
write.fwf(Oppprint, file="print.txt", eol="\r\n",sep="          ", append = TRUE)
write.fwf(home_lineup_to_print, file="print.txt", eol="\r\n",sep="     ", append = TRUE)
write.fwf(away_lineup_to_print, file="print.txt", eol="\r\n",sep="     ", append = TRUE)

individualboxscore <- rbind(homeboxscore, oppboxscore)
write.csv(individualboxscore, "individualboxscore.csv")
write.csv(lineupboxscore, "lineupboxscore.csv")
write.csv(teamstats, "teamstats.csv")

