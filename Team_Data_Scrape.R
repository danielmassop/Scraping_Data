#install.packages("XML")
library(XML)
#install.packages("RCurl")
library(RCurl)

teams = c('ATL','BRK','BOS','CHO','CHI',
          'CLE','DAL','DEN','DET','GSW',
          'HOU','IND','LAC','LAL','MEM',
          'MIA','MIL','MIN','NOP','NYK',
          'OKC','ORL','PHI','PHO','POR',
          'SAC','SAS','TOR','UTA','WAS')

months = c(10,11,12)

season = c()

for (i in teams){
  for (k in months){
    for (j in c(1:31)){
      if (j < 10) {
        j = paste('0',j,sep = "")
      }
      url = paste("http://www.basketball-reference.com/boxscores/2016",k,j,"0",i,".html",sep = "")
      if(url.exists(url) == TRUE){
        data <- readHTMLTable(url, stringsAsFactors = FALSE)
        away_data = as.data.frame(data[1])
        away_data = away_data[,c(1:20)]
        
        colnames(away_data) = c("Players","MP","FG","FGA","FG%","THR","THRA",
                                "3P%","FT","FTA","FT%","ORB","DRB","TRB","AST",
                                "STL","BLK","TO","PF","PTS")
        
        away_data = away_data[away_data$Players != "Reserves",]
        away_data = away_data[away_data$MP != "Did Not Play",]
        away_data = away_data[away_data$MP != "Player Suspended",]
        away_data = away_data[away_data$MP != "Did Not Dress",]
        away_data = away_data[away_data$MP != "Not With Team",]
        
        away_data$FG = as.numeric(as.character(away_data$FG))
        away_data$FGA = as.numeric(as.character(away_data$FGA))
        away_data$THR = as.numeric(as.character(away_data$THR))
        away_data$THRA = as.numeric(as.character(away_data$THRA))
        away_data$FT = as.numeric(as.character(away_data$FT))
        away_data$FTA = as.numeric(as.character(away_data$FTA))
        away_data$ORB = as.numeric(as.character(away_data$ORB))
        away_data$DRB = as.numeric(as.character(away_data$DRB))
        away_data$AST = as.numeric(as.character(away_data$AST))
        away_data$STL = as.numeric(as.character(away_data$STL))
        away_data$BLK = as.numeric(as.character(away_data$BLK))
        away_data$TO = as.numeric(as.character(away_data$TO))
        away_data$PF = as.numeric(as.character(away_data$FGA))
        away_data$PTS = as.numeric(as.character(away_data$PTS))
        
        away = NULL
        away$team = toupper(substr(names(as.data.frame(data[1]))[1], 5, 7))
        away = as.data.frame(away)
        
        away$FG = sum(away_data$FG)
        away$FGA = sum(away_data$FGA)
        away$THR = sum(away_data$THR)
        away$THRA = sum(away_data$THRA)
        away$FT = sum(away_data$FT)
        away$FTA = sum(away_data$FTA)
        away$ORB = sum(away_data$ORB)
        away$DRB = sum(away_data$DRB)
        away$TRB = away$ORB + away$DRB
        away$AST = sum(away_data$AST)
        away$STL = sum(away_data$STL)
        away$BLK = sum(away_data$BLK)
        away$TO = sum(away_data$TO)
        away$PF = sum(away_data$PF)
        away$PTS = sum(away_data$PTS)
        
        home_data = as.data.frame(data[3])
        away_data = away_data[,c(1:20)]
        
        colnames(home_data) = c("Players","MP","FG","FGA","FG%","THR","THRA",
                                "3P%","FT","FTA","FT%","ORB","DRB","TRB","AST",
                                "STL","BLK","TO","PF","PTS")
        
        home_data = home_data[home_data$Players != "Reserves",]
        home_data = home_data[home_data$MP != "Did Not Play",]
        home_data = home_data[home_data$MP != "Player Suspended",]
        home_data = home_data[home_data$MP != "Did Not Dress",]
        home_data = home_data[home_data$MP != "Not With Team",]
        
        home_data$FG = as.numeric(as.character(home_data$FG))
        home_data$FGA = as.numeric(as.character(home_data$FGA))
        home_data$THR = as.numeric(as.character(home_data$THR))
        home_data$THRA = as.numeric(as.character(home_data$THRA))
        home_data$FT = as.numeric(as.character(home_data$FT))
        home_data$FTA = as.numeric(as.character(home_data$FTA))
        home_data$ORB = as.numeric(as.character(home_data$ORB))
        home_data$DRB = as.numeric(as.character(home_data$DRB))
        home_data$AST = as.numeric(as.character(home_data$AST))
        home_data$STL = as.numeric(as.character(home_data$STL))
        home_data$BLK = as.numeric(as.character(home_data$BLK))
        home_data$TO = as.numeric(as.character(home_data$TO))
        home_data$PF = as.numeric(as.character(home_data$FGA))
        home_data$PTS = as.numeric(as.character(home_data$PTS))
        
        home = NULL
        home$team = toupper(substr(names(as.data.frame(data[3]))[1], 5, 7))
        home = as.data.frame(home)
        
        home$FG = sum(home_data$FG)
        home$FGA = sum(home_data$FGA)
        home$THR = sum(home_data$THR)
        home$THRA = sum(home_data$THRA)
        home$FT = sum(home_data$FT)
        home$FTA = sum(home_data$FTA)
        home$ORB = sum(home_data$ORB)
        home$DRB = sum(home_data$DRB)
        home$TRB = home$ORB + home$DRB
        home$AST = sum(home_data$AST)
        home$STL = sum(home_data$STL)
        home$BLK = sum(home_data$BLK)
        home$TO = sum(home_data$TO)
        home$PF = sum(home_data$PF)
        home$PTS = sum(home_data$PTS)
        
        home_final = cbind(home, away)
        
        away_final = cbind(away, home)
        
        colnames(home_final) = c("Team","FG","FGA","THR","THRA","FT","FTA","ORB","DRB","TRB",
                                 "AST","STL","BLK","TO","PF","PTS","Opp","oFG","oFGA","oTHR",
                                 "oTHRA","oFT","oFTA","oORB","oDRB","oTRB","oAST","oSTL",
                                 "oBLK","oTO","oPF","oPTS")
        colnames(away_final) = c("Team","FG","FGA","THR","THRA","FT","FTA","ORB","DRB","TRB",
                                 "AST","STL","BLK","TO","PF","PTS","Opp","oFG","oFGA","oTHR",
                                 "oTHRA","oFT","oFTA","oORB","oDRB","oTRB","oAST","oSTL",
                                 "oBLK","oTO","oPF","oPTS")
        
        home_final$WIN = if(home_final$PTS > home_final$oPTS){TRUE} else {FALSE}
        home_final$date = paste(k,j,"2016",sep = "/")
        home_final$HOME = TRUE
        
        away_final$WIN = if(away_final$PTS > away_final$oPTS){TRUE} else {FALSE}
        away_final$date = paste(k,j,"2016",sep = "/")
        away_final$HOME = FALSE
        
        final = rbind(home_final,away_final)
        
        season = rbind(season, final)
      }
    }
  }
  print(i)
}

months = c('01','02','03')

for (i in teams){
  for (k in months){
    for (j in c(1:31)){
      if (j < 10) {
        j = paste('0',j,sep = "")
      }
      
      url = paste("http://www.basketball-reference.com/boxscores/2017",k,j,"0",i,".html",sep = "")
      if(url.exists(url) == TRUE){
        data <- readHTMLTable(url, stringsAsFactors = FALSE)
        away_data = as.data.frame(data[1])
        away_data = away_data[,c(1:20)]
        
        colnames(away_data) = c("Players","MP","FG","FGA","FG%","THR","THRA",
                                "3P%","FT","FTA","FT%","ORB","DRB","TRB","AST",
                                "STL","BLK","TO","PF","PTS")
        
        away_data = away_data[away_data$Players != "Reserves",]
        away_data = away_data[away_data$MP != "Did Not Play",]
        away_data = away_data[away_data$MP != "Player Suspended",]
        away_data = away_data[away_data$MP != "Did Not Dress",]
        away_data = away_data[away_data$MP != "Not With Team",]
        
        away_data$FG = as.numeric(as.character(away_data$FG))
        away_data$FGA = as.numeric(as.character(away_data$FGA))
        away_data$THR = as.numeric(as.character(away_data$THR))
        away_data$THRA = as.numeric(as.character(away_data$THRA))
        away_data$FT = as.numeric(as.character(away_data$FT))
        away_data$FTA = as.numeric(as.character(away_data$FTA))
        away_data$ORB = as.numeric(as.character(away_data$ORB))
        away_data$DRB = as.numeric(as.character(away_data$DRB))
        away_data$AST = as.numeric(as.character(away_data$AST))
        away_data$STL = as.numeric(as.character(away_data$STL))
        away_data$BLK = as.numeric(as.character(away_data$BLK))
        away_data$TO = as.numeric(as.character(away_data$TO))
        away_data$PF = as.numeric(as.character(away_data$FGA))
        away_data$PTS = as.numeric(as.character(away_data$PTS))
        
        away = NULL
        away$team = toupper(substr(names(as.data.frame(data[1]))[1], 5, 7))
        away = as.data.frame(away)
        
        away$FG = sum(away_data$FG)
        away$FGA = sum(away_data$FGA)
        away$THR = sum(away_data$THR)
        away$THRA = sum(away_data$THRA)
        away$FT = sum(away_data$FT)
        away$FTA = sum(away_data$FTA)
        away$ORB = sum(away_data$ORB)
        away$DRB = sum(away_data$DRB)
        away$TRB = away$ORB + away$DRB
        away$AST = sum(away_data$AST)
        away$STL = sum(away_data$STL)
        away$BLK = sum(away_data$BLK)
        away$TO = sum(away_data$TO)
        away$PF = sum(away_data$PF)
        away$PTS = sum(away_data$PTS)
        
        home_data = as.data.frame(data[3])
        away_data = away_data[,c(1:20)]
        
        colnames(home_data) = c("Players","MP","FG","FGA","FG%","THR","THRA",
                                "3P%","FT","FTA","FT%","ORB","DRB","TRB","AST",
                                "STL","BLK","TO","PF","PTS")
        
        home_data = home_data[home_data$Players != "Reserves",]
        home_data = home_data[home_data$MP != "Did Not Play",]
        home_data = home_data[home_data$MP != "Player Suspended",]
        home_data = home_data[home_data$MP != "Did Not Dress",]
        home_data = home_data[home_data$MP != "Not With Team",]
        
        home_data$FG = as.numeric(as.character(home_data$FG))
        home_data$FGA = as.numeric(as.character(home_data$FGA))
        home_data$THR = as.numeric(as.character(home_data$THR))
        home_data$THRA = as.numeric(as.character(home_data$THRA))
        home_data$FT = as.numeric(as.character(home_data$FT))
        home_data$FTA = as.numeric(as.character(home_data$FTA))
        home_data$ORB = as.numeric(as.character(home_data$ORB))
        home_data$DRB = as.numeric(as.character(home_data$DRB))
        home_data$AST = as.numeric(as.character(home_data$AST))
        home_data$STL = as.numeric(as.character(home_data$STL))
        home_data$BLK = as.numeric(as.character(home_data$BLK))
        home_data$TO = as.numeric(as.character(home_data$TO))
        home_data$PF = as.numeric(as.character(home_data$FGA))
        home_data$PTS = as.numeric(as.character(home_data$PTS))
        
        home = NULL
        home$team = toupper(substr(names(as.data.frame(data[3]))[1], 5, 7))
        home = as.data.frame(home)
        
        home$FG = sum(home_data$FG)
        home$FGA = sum(home_data$FGA)
        home$THR = sum(home_data$THR)
        home$THRA = sum(home_data$THRA)
        home$FT = sum(home_data$FT)
        home$FTA = sum(home_data$FTA)
        home$ORB = sum(home_data$ORB)
        home$DRB = sum(home_data$DRB)
        home$TRB = home$ORB + home$DRB
        home$AST = sum(home_data$AST)
        home$STL = sum(home_data$STL)
        home$BLK = sum(home_data$BLK)
        home$TO = sum(home_data$TO)
        home$PF = sum(home_data$PF)
        home$PTS = sum(home_data$PTS)
        
        home_final = cbind(home, away)
        
        away_final = cbind(away, home)
        
        colnames(home_final) = c("Team","FG","FGA","THR","THRA","FT","FTA","ORB","DRB","TRB",
                                 "AST","STL","BLK","TO","PF","PTS","Opp","oFG","oFGA","oTHR",
                                 "oTHRA","oFT","oFTA","oORB","oDRB","oTRB","oAST","oSTL",
                                 "oBLK","oTO","oPF","oPTS")
        colnames(away_final) = c("Team","FG","FGA","THR","THRA","FT","FTA","ORB","DRB","TRB",
                                 "AST","STL","BLK","TO","PF","PTS","Opp","oFG","oFGA","oTHR",
                                 "oTHRA","oFT","oFTA","oORB","oDRB","oTRB","oAST","oSTL",
                                 "oBLK","oTO","oPF","oPTS")
        
        home_final$WIN = if(home_final$PTS > home_final$oPTS){TRUE} else {FALSE}
        home_final$date = paste(k,j,"2017",sep = "/")
        home_final$HOME = TRUE
        
        away_final$WIN = if(away_final$PTS > away_final$oPTS){TRUE} else {FALSE}
        away_final$date = paste(k,j,"2017",sep = "/")
        away_final$HOME = FALSE
        
        final = rbind(home_final,away_final)
        
        season = rbind(season, final)
      }
    }
  }
  print(i)
}

season$date = as.Date(season$date, "%m/%d/%Y")

write.csv(season, "C://Users//Daniel.Massop//Desktop//R_Codes//2016_Season.csv", row.names = FALSE)
