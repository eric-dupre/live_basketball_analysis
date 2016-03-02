############################################################################################################################
# Please read the readme file from github before running this script to understand the scope of this program and its limitations.
#
# Be sure to isntall the following packages if they are not already installed in your version of R: stringr, plyr, gdata, 
# data.table, RSelenium, and rvest. Additionally, uncomment the first line of code if this is your first time using RSelenium
# to download the binary needed to run the package. Finally, be sure to download the Mozilla Firefox web browser before runnning 
# this program.
#
#############################################################################################################################
#RSelenium::checkForServer()
library(stringr)
library(plyr)
library(gdata)
library(data.table)
require(RSelenium)
require(rvest)
options(stringsAsFactors = FALSE)
############################################################################################################################
#
# Below are all of the parameters that need to be specified in order for the program to run: working directory, StatBroadcast 
# url, half number (1 or 2), home (TRUE or FALSE), the opponent for the game you are studying, your team, and the last name 
# (without suffix)  of your best player or player of interest. 
#
#############################################################################################################################

setwd("C:/Users/eDupe_000/Desktop")
site <- "http://stats.statbroadcast.com/statmonitr/?id=108205"
home <- TRUE
# this user control flag allows you to manually control which half of a game to analyze. if set to TRUE, the programs will analyze the 
# half that you specify. Otherwise, the program will run for the current half (as specified by the statbroadcast site). This feature allows
# the user to analyze past games or games of opponents after the fact. To utilize this feature, set user control to TRUE, specify half 1, 
# then run both scripts. Before the second half, change the half indicator from 1 to 2 (should then say 'half <- 2') and ONLY RUN that line
# in this script. Finally, run the second script and you will have analysis of both halves ready. 
# NOTE THAT THE FIRST HALF MUST ALWAYS BE RUN BEFORE THE SECOND HALF
user_control <- FALSE
half <- 1
starplayer <- "SIMMONS"


############################################################################################################################
#
# Below is a function created to process play by play data for one of the two teams, create a box score for each 5 player lineup 
# that enters the game, calculate advanced statistics for each lineup, and output the lineup box score. Refer to the notes in the  
# funtion to get specific information on how the it runs. The function takes one argument, a character string corresponding to 
# a team's abbreviation. For example, Ohio State's abbreviation is "OSU" for the purposes of StatBroadcast.
#
#############################################################################################################################

##################
#START OF FUNCTION

process_pbp <- function(ourteam){

# in this first segment, the game segment and half segment objects are initialized. A game segment of 1 means that it is the 
# beginning of the game. When the "star player" subs in or subs out, game segment and half segment are increased by one. 
# Then, when the second half begins, half segment resets and game segment is increased by one. These segments can be used to 
# analyze a player's performance based off of different substitution patterns.
  
# Then, the function initiates the objects that track the score, opprun and ourrun for away and home score, respectively.

  if (half==1) {
    game_segment <- 1
    half_segment <- 1
    ourrun <- as.numeric(0)
    opprun <- as.numeric(0) 
  }
  
  
  if (half==2) {
    half_segment <- 1
    game_segment <- game_segment + 1
  }
  
# This section initiates the lineup boxscore, tracking the players in the game, the number of minutes they play together, 
# all of their box score stats for the time they're playing together, as well as indicators for opponent, half, and segments
  lineupboxscore <- data.frame(Name=NA, Minutes=NA, Plus_Minus=NA, ourscoreIn=ourrun, oppscoreIn=opprun,
                               ourscoreOut=NA, oppscoreOut=NA, TimeIn=20, TimeOut=NA, A=0,ST=0,TO=0, PF=0,
                               BL=0, DR=0, OR=0, MISSFT=0, FGA=0, FGM=0, PTS=0,
                               FT=0, Threept=0, Twopt=0, OppDR=0, OppFG=0, OppFGA=0, OppORB=0,
                               OppTO=0, OppFT=0, OppMissft=0, half=half, against=against, game_segment=game_segment, half_segment=half_segment)
# here the program will recognize the starting lineup from statbroadcast and list their names in the first line of the lineup box score
  lineupboxscore[1,1] <- paste(lineup, sep=", ", collapse=", ")
  lineupboxscore$against  <- as.character(lineupboxscore$against)  

# now that we have the data frame initiated, we are ready to process the play by play
# stats will be accumulated as the game goes on. For example, every time the starting lineup scores a basket, the points are counted in their line of 
# the lineup play by play. each time there is a substitution, a new line is created in the lineup box score
    
  for (i in 1:length(playbyplay)) {
# identifies the time on the clock (only the minutes component)
    if (grepl("[0-9]:", playbyplay[i])) {
      time <- as.numeric(str_extract(str_extract(playbyplay[i], "[:digit:]*:"), "[:digit:]*"))
    }
# in this block, we scrape the score from scoring plays and save them. the current team's score is saved in "ourrun" while their opponent's score is in "opprun"
    if (grepl("[0-9]-[0-9]", playbyplay[i])){
      if (home_team == ourteam) {
        opprun <- as.numeric(str_extract(str_extract(playbyplay[i], "[:digit:]{1,2}-"), "[:digit:]{1,2}"))
        ourrun <- abs(as.numeric(str_extract(playbyplay[i], "-[:digit:]{1,2}")))  
        opprun <<- as.numeric(str_extract(str_extract(playbyplay[i], "[:digit:]{1,2}-"), "[:digit:]{1,2}"))
        ourrun <<- abs(as.numeric(str_extract(playbyplay[i], "-[:digit:]{1,2}")))  
      } else {
        ourrun <- as.numeric(str_extract(str_extract(playbyplay[i], "[:digit:]{1,2}-"), "[:digit:]{1,2}"))
        opprun <- abs(as.numeric(str_extract(playbyplay[i], "-[:digit:]{1,2}")))  
        ourrun <<- as.numeric(str_extract(str_extract(playbyplay[i], "[:digit:]{1,2}-"), "[:digit:]{1,2}"))
        opprun <<- abs(as.numeric(str_extract(playbyplay[i], "-[:digit:]{1,2}")))  
      }
    }
# we remove suffixes from the play by play to more easily match player names
    if (grepl(" III", playbyplay[i])) {playbyplay[i] <- gsub(" III", "", playbyplay[i])}
    if (grepl(" II", playbyplay[i])) {playbyplay[i] <- gsub(" II", "", playbyplay[i])} 
    if (grepl(" IV$", playbyplay[i])) {playbyplay[i] <- gsub(" IV", "", playbyplay[i])} 

# this next large block counts all box score statistics and assigns them to the appropriate column. 
# in addition, we recognize substitutions and adjust the lineup accordingly    
    if (grepl(paste(ourteam), playbyplay[i])) {
      if (grepl("ASSIST", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 10] <- lineupboxscore[nrow(lineupboxscore), 10]+1}
      if (grepl("STEAL", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 11] <- lineupboxscore[nrow(lineupboxscore), 11]+1}
      if (grepl("TURNOVER", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 12] <- lineupboxscore[nrow(lineupboxscore), 12]+1}
      if (grepl("FOUL ON", playbyplay[i])) {lineupboxscore[nrow(lineupboxscore), 13] <- lineupboxscore[nrow(lineupboxscore), 13]+1}
      if (grepl("BLOCK", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 14] <- lineupboxscore[nrow(lineupboxscore), 14]+1}
      if (grepl("DEF[[:space:]]{1,2}REBOUND", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 15] <- lineupboxscore[nrow(lineupboxscore), 15]+1}
      if (grepl("OFF[[:space:]]{1,2}REBOUND", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 16] <- lineupboxscore[nrow(lineupboxscore), 16]+1}
      if (grepl("DEADB[[:space:]]{1,2}REBOUND", playbyplay[i])){
        if (grepl("MISSED J", playbyplay[i-1])| grepl("MISSED L", playbyplay[i-1]) | grepl("MISSED D", playbyplay[i-1]) | grepl("MISSED 3", playbyplay[i-1])){
            lineupboxscore[nrow(lineupboxscore), 16] <- lineupboxscore[nrow(lineupboxscore), 16]+1} 
        }  
      if (grepl("MISSED F", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 17] <- lineupboxscore[nrow(lineupboxscore), 17]+1}
      if(grepl("MISSED L", playbyplay[i]) | grepl("MISSED D", playbyplay[i]) | grepl("MISSED J", playbyplay[i]) | grepl("MISSED 3", playbyplay[i]) |
         grepl("LAYUP", playbyplay[i]) | grepl("DUNK", playbyplay[i]) | grepl("JUMPER", playbyplay[i]) | grepl("3PTR", playbyplay[i])) {
        lineupboxscore[nrow(lineupboxscore), 18] <- lineupboxscore[nrow(lineupboxscore), 18]+1
        }
      if (grepl("MISSED", playbyplay[i]) == FALSE) {
        if(grepl("LAYUP", playbyplay[i]) | grepl("DUNK", playbyplay[i]) | grepl("JUMPER", playbyplay[i]) | grepl("3PTR", playbyplay[i])) {lineupboxscore[nrow(lineupboxscore), 19] <- lineupboxscore[nrow(lineupboxscore), 19]+1}
        if(grepl("LAYUP", playbyplay[i]) | grepl("DUNK", playbyplay[i]) | grepl("JUMPER", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 20] <- lineupboxscore[nrow(lineupboxscore), 20]+2}
        if (grepl("FT B", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 20] <- lineupboxscore[nrow(lineupboxscore), 20]+1}
        if (grepl("3PTR", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 20] <- lineupboxscore[nrow(lineupboxscore), 20]+3}
        if (grepl("FT B", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 21] <- lineupboxscore[nrow(lineupboxscore), 21]+1}
        if (grepl("3PTR", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 22] <- lineupboxscore[nrow(lineupboxscore), 22]+1}
        if(grepl("LAYUP", playbyplay[i]) | grepl("DUNK", playbyplay[i]) | grepl("JUMPER", playbyplay[i])){lineupboxscore[nrow(lineupboxscore), 23] <- lineupboxscore[nrow(lineupboxscore), 23]+1}
      }
      
# note that in StatBroadcast, there is only one substitution per line and all of the subs in happen before subs out
      
# for each sub in, the incoming player is inserted into the lineup object but not into the lineup box score. 
# if the player coming in is the star player, game segment and half segment are incremented by one
# for players being subbed out, their names are removed from the lineup object
      if (grepl("SUB IN", playbyplay[i])) {
        if (grepl("^[A-Z]", playbyplay[i])){
          lineup[length(lineup)+1] <- str_extract(word(playbyplay[i], -4),"[A-Z]+-?[A-Z]+")
          if (str_extract(word(playbyplay[i], -4),"[A-Z]+-?[A-Z]+") == paste(starplayer)) {
            game_segment <- game_segment+1
            half_segment <- half_segment+1
          }
        } else {
          lineup[length(lineup)+1] <- str_extract(word(playbyplay[i], -1),"[A-Z]+-?[A-Z]+")
            if (str_extract(word(playbyplay[i], -1),"[A-Z]+-?[A-Z]+") == paste(starplayer)) {
              game_segment <- game_segment+1
              half_segment <- half_segment+1
            }
          }
        } else if (grepl("SUB OUT", playbyplay[i])) {
            if (grepl("^[A-Z]", playbyplay[i])){
              remove <- str_extract(word(playbyplay[i], -4),"[A-Z]+-?[A-Z]+")
              
              if (str_extract(word(playbyplay[i], -4),"[A-Z]+-?[A-Z]+")==paste(starplayer)) {
                game_segment <- game_segment+1
                half_segment <- half_segment+1
              }
            } else{
              remove <- str_extract(word(playbyplay[i], -1),"[A-Z]+-?[A-Z]+")
              if (str_extract(word(playbyplay[i], -1),"[A-Z]+-?[A-Z]+")==paste(starplayer)) {
                game_segment <- game_segment+1
                half_segment <- half_segment+1
              }
            }
            lineup <- lineup[lineup != remove]
# once all the substitutions for a dead ball have taken place, we add a new line in the lineup box score and initialize all box score stats to 0 so the process can begin again
            if (length(lineup) ==5) {
              lineup <- lineup[order(lineup)]
              newrow <- data.frame(Name=paste(lineup, sep=", ", collapse=", "), Minutes=NA, Plus_Minus=NA, ourscoreIn=ourrun, oppscoreIn=opprun, ourscoreOut=NA, oppscoreOut=NA, TimeIn=time, TimeOut=NA,
                                   A=0, ST=0, TO=0, PF=0, BL=0, DR=0, OR=0, MISSFT=0, FGA=0, FGM=0, PTS=0, FT=0, Threept=0, Twopt=0, OppDR=0, OppFG=0, OppFGA=0, OppORB=0, OppTO=0, OppFT=0, OppMissft=0, half=half, against=against,
                                   game_segment=game_segment, half_segment=half_segment) 
              lineupboxscore <- rbind(lineupboxscore, newrow)
# here we input the time when the sub was made and the score when the sub was made and calculate the minutes played and plus_minus score
              lineupboxscore[nrow(lineupboxscore)-1, 9] <- time
              lineupboxscore[nrow(lineupboxscore)-1, 6] <- ourrun
              lineupboxscore[nrow(lineupboxscore)-1, 7] <- opprun
              lineupboxscore[nrow(lineupboxscore)-1, 2] <- lineupboxscore[nrow(lineupboxscore)-1, 8] - lineupboxscore[nrow(lineupboxscore)-1, 9]
              lineupboxscore[nrow(lineupboxscore)-1, 3] <- (lineupboxscore[nrow(lineupboxscore)-1, 6]-lineupboxscore[nrow(lineupboxscore)-1, 4])-(lineupboxscore[nrow(lineupboxscore)-1, 7]-lineupboxscore[nrow(lineupboxscore)-1, 5])
            }
          }
    }
# now we leave the loop for the current team stats and track opponent stats so we can analyze their defensive performance
      if (grepl("DEF[[:space:]]{1,2}REBOUND", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])))   {lineupboxscore[nrow(lineupboxscore), 24] <- lineupboxscore[nrow(lineupboxscore), 24]+1}
      if((grepl("LAYUP", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])) & !(grepl("MISSED", playbyplay[i]))) | (grepl("DUNK", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])) & !(grepl("MISSED", playbyplay[i]))) |
         (grepl("JUMPER", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])) & !(grepl("MISSED", playbyplay[i]))) | (grepl("3PTR", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])) & !(grepl("MISSED", playbyplay[i])))) {lineupboxscore[nrow(lineupboxscore), 25] <- lineupboxscore[nrow(lineupboxscore), 25]+1}
      if((grepl("LAYUP", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) | (grepl("DUNK", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) | (grepl("JUMPER", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) |
         (grepl("3PTR", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])))) {lineupboxscore[nrow(lineupboxscore), 26] <- lineupboxscore[nrow(lineupboxscore), 26]+1}
      if (grepl("OFF[[:space:]]{1,2}REBOUND", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) {lineupboxscore[nrow(lineupboxscore), 27] <- lineupboxscore[nrow(lineupboxscore), 27]+1}
      if (grepl("DEADB[[:space:]]{1,2}REBOUND", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))){
        if (grepl("MISSED J", playbyplay[i-1])| grepl("MISSED L", playbyplay[i-1]) | grepl("MISSED D", playbyplay[i-1]) | grepl("MISSED 3", playbyplay[i-1]) & !(grepl(paste(ourteam), playbyplay[i]))){
          lineupboxscore[nrow(lineupboxscore), 27] <- lineupboxscore[nrow(lineupboxscore), 27]+1} }
      if (grepl("TURNOVER", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) {lineupboxscore[nrow(lineupboxscore), 28] <- lineupboxscore[nrow(lineupboxscore), 28]+1}
      if (grepl("FT B", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i])) & !(grepl("MISSED", playbyplay[i]))) {lineupboxscore[nrow(lineupboxscore), 29] <- lineupboxscore[nrow(lineupboxscore),29 ]+1}
      if (grepl("MISSED F", playbyplay[i]) & !(grepl(paste(ourteam), playbyplay[i]))) {lineupboxscore[nrow(lineupboxscore), 30] <- lineupboxscore[nrow(lineupboxscore), 30]+1}
      
  }
#now we exit the play by play loop and fill in the missing stats for the last lineup played (minutes played, score, and plus minus)
    lineupboxscore[nrow(lineupboxscore), 9] <- time
    lineupboxscore[nrow(lineupboxscore), 6] <- ourrun
    lineupboxscore[nrow(lineupboxscore), 7] <- opprun
    lineupboxscore[nrow(lineupboxscore), 2] <- lineupboxscore[nrow(lineupboxscore), 8] - lineupboxscore[nrow(lineupboxscore), 9]
    lineupboxscore[nrow(lineupboxscore), 3] <- (lineupboxscore[nrow(lineupboxscore), 6]-lineupboxscore[nrow(lineupboxscore), 4])-(lineupboxscore[nrow(lineupboxscore), 7]-lineupboxscore[nrow(lineupboxscore), 5])
  
# here we calculate advanced stats for each lineup, including game score, effective FG percentage, ft hack, and others
  lineupboxscore$Game_Score <- (lineupboxscore$PTS + .4 * lineupboxscore$FGM - .7 * lineupboxscore$FGA - .4 * lineupboxscore$MISSFT + .7 * lineupboxscore$OR + .3 * lineupboxscore$DR + lineupboxscore$ST + .7 * lineupboxscore$A + .7 * lineupboxscore$BL - .4 * lineupboxscore$PF - lineupboxscore$TO)*(40/lineupboxscore$Minutes)/5
  lineupboxscore$Oppspts <- lineupboxscore$oppscoreOut - lineupboxscore$oppscoreIn
  lineupboxscore$FTA <- lineupboxscore$MISSFT + lineupboxscore$FT
  lineupboxscore$Game_Score[lineupboxscore$Minutes == 0] <- NA
  lineupboxscore$OppFTA <- lineupboxscore$OppMissft + lineupboxscore$OppFT
  lineupboxscore$eFG <- ((lineupboxscore$FGM + 0.5 * lineupboxscore$Threept) / lineupboxscore$FGA)
  lineupboxscore$eFG <- ifelse(is.na(lineupboxscore$eFG), 0, 
                               ifelse((!is.na(lineupboxscore$eFG)), lineupboxscore$eFG, 1))
  lineupboxscore$TOp <- 100*(lineupboxscore$TO / (lineupboxscore$FGA + .44 * lineupboxscore$FTA + lineupboxscore$TO))
  lineupboxscore$TOp <- ifelse(is.na(lineupboxscore$TOp), 0, 
                               ifelse((!is.na(lineupboxscore$TOp)), lineupboxscore$TOp, 1))
  lineupboxscore$ORp <- (lineupboxscore$OR / (lineupboxscore$OR + lineupboxscore$OppDR))
  lineupboxscore$ORp <- ifelse(is.na(lineupboxscore$ORp), 0, 
                               ifelse((!is.na(lineupboxscore$ORp)), lineupboxscore$ORp, 1))
  lineupboxscore$FTp <- (lineupboxscore$FT / lineupboxscore$FGA)
  lineupboxscore$FTp <- ifelse(is.na(lineupboxscore$FTp), 0, 
                               ifelse((!is.na(lineupboxscore$FTp)), lineupboxscore$FTp, 1))
  lineupboxscore$fthack <- lineupboxscore$OppFT / lineupboxscore$OppFTA
  lineupboxscore$fthack <- ifelse(is.na(lineupboxscore$fthack), 0, 
                                  ifelse((!is.na(lineupboxscore$fthack)), lineupboxscore$fthack, 1))  
# here we estimate the number of possessions for the current team  
    DFGpl <- lineupboxscore$OppFG/lineupboxscore$OppFGA
    DORpl <- lineupboxscore$OppORB / (lineupboxscore$OppORB + lineupboxscore$DR)
    FMwtl <- (DFGpl *(1 - DORpl)) / (DFGpl * (1 - 1.07 * DORpl) + (1 - DFGpl) * DORpl)
    FMwtl <- ifelse(is.na(FMwtl), 0, 
                    ifelse((!is.na(FMwtl)), FMwtl, 1))
    stopsonel <- lineupboxscore$ST + lineupboxscore$BL * FMwtl * (1 - 1.07 * DORpl) + lineupboxscore$DR * (1 - FMwtl)
    stopstwol <- (((lineupboxscore$OppFGA - lineupboxscore$OppFG - lineupboxscore$BL) / lineupboxscore$Minutes) * FMwtl * (1 - 1.07 * DORpl) + ((lineupboxscore$OppTO - lineupboxscore$ST) / lineupboxscore$Minutes)) * lineupboxscore$Minutes +
      lineupboxscore$PF * .4 * lineupboxscore$OppFTA * (1 - (lineupboxscore$fthack))^2
    stopsl <- stopsonel + stopstwol
    teampossl <- .5 * ((lineupboxscore$FGA + .4 * lineupboxscore$FTA - 1.07 * (lineupboxscore$OR / (lineupboxscore$OR + lineupboxscore$OppDR)) * (lineupboxscore$FGA - lineupboxscore$FGM) + lineupboxscore$TO) + (lineupboxscore$OppFGA + .4 * lineupboxscore$OppFTA - 1.07 *(lineupboxscore$OppORB /(lineupboxscore$OppORB + lineupboxscore$DR)) * (lineupboxscore$OppFGA - lineupboxscore$OppFG) + lineupboxscore$OppTO))
    teampossl <- teampossl

#now we use posessions estimation and opponent points to calculate each lineup's defensive rating
  lineupboxscore$TmDfRt <- 100 * (lineupboxscore$Oppspts / teampossl)
  
  lineupboxscore <<- lineupboxscore
  game_segment <<- game_segment
}

################
#END OF LINEUP FUNCTION


############################################################################################################################
#
# Below we define a funtion that will be used with our overall team stats. This function will calculate game score, effective 
# FG%, and other stats for one team for the entire game. The function takes one argument, a data frame with team stats.
#
#############################################################################################################################

TmDfRt <- function(teamstats) {
  teamstats$Game_Score <- (teamstats$ourPoints + .4 * teamstats$FG - .7 * teamstats$FGA - .4 * teamstats$MISSFT + .7 * teamstats$ORB + .3 * teamstats$DRB + teamstats$STL + .7 * teamstats$AST + .7 * teamstats$BL - .4 * teamstats$PF - teamstats$TO)
  teamstats$eFG <- ((teamstats$FG + 0.5 * teamstats$Threept) / teamstats$FGA)
  teamstats$eFG <- ifelse(is.na(teamstats$eFG), 0, 
                          ifelse((!is.na(teamstats$eFG)), teamstats$eFG, 1))
  teamstats$TOp <- 100*(teamstats$TO / (teamstats$FGA + .44 * teamstats$FTA + teamstats$TO))
  teamstats$TOp <- ifelse(is.na(teamstats$TOp), 0, 
                          ifelse((!is.na(teamstats$TOp)), teamstats$TOp, 1))
  teamstats$ORp <- (teamstats$ORB / (teamstats$ORB + teamstats$OppDR))
  teamstats$ORp <- ifelse(is.na(teamstats$ORp), 0, 
                          ifelse((!is.na(teamstats$ORp)), teamstats$ORp, 1))
  teamstats$FTp <- (teamstats$FT / teamstats$FGA)
  teamstats$FTp <- ifelse(is.na(teamstats$FTp), 0, 
                          ifelse((!is.na(teamstats$FTp)), teamstats$FTp, 1))
  DFGpt <- teamstats$OppFG/teamstats$OppFGA
  DORpt <- teamstats$OppORB / (teamstats$OppORB + teamstats$DRB)
  FMwtt <- (DFGpt *(1 - DORpt)) / (DFGpt * (1 - 1.07 * DORpt) + (1 - DFGpt) * DORpt)
  stopsonet <- teamstats$STL + teamstats$BL * FMwtt * (1 - 1.07 * DORpt) + teamstats$DRB * (1 - FMwtt)
  stopstwot <- (((teamstats$OppFGA - teamstats$OppFG - teamstats$BL) / teamstats$Minutes) * FMwtt * (1 - 1.07 * DORpt) + ((teamstats$OppTO - teamstats$STL) / teamstats$Minutes)) * teamstats$Minutes +
    teamstats$PF * .4 * teamstats$OppFTA * (1 - (teamstats$fthack))^2
  stopst <- stopsonet + stopstwot
  teamposst <- .5 * ((teamstats$FGA + .4 * teamstats$FTA - 1.07 * (teamstats$ORB / (teamstats$ORB + teamstats$OppDR)) * (teamstats$FGA - teamstats$FG) + teamstats$TO) + (teamstats$OppFGA + .4 * teamstats$OppFTA - 1.07 *
                                                                                                                                                                            (teamstats$OppORB /(teamstats$OppORB + teamstats$DRB)) * (teamstats$OppFGA - teamstats$OppFG) + teamstats$OppTO))
  teamstats$TmDfRt <- 100 * (teamstats$OppPoints / teamposst)
  
  row.names(teamstats) <- "teamstats"
  
  teamstats <<- teamstats
}

############################################################################################################################
#
# Now, all functions and initial variables have been set and we are ready to import data and run our analysis   
#
#############################################################################################################################

