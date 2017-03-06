season = read.csv("C://Users//Daniel.Massop//Desktop//R_Codes//2016_Season.csv")

avg_score = mean(season$PTS)

teams_agg = aggregate(season[,c('PTS','oPTS')], by = list(season$Team), mean)

season = season[,c('Team','Opp','PTS','oPTS')]

season_join = merge(season, teams_agg, by.x = 'Team', by.y = 'Group.1', all.x = TRUE)
season_join = merge(season_join, teams_agg, by.x = 'Opp', by.y = 'Group.1', all.x = TRUE)

colnames(season_join) = c('Opp','Team','PTS','oPTS','Team_Avg_Pts','Team_Avg_Pts_Allowed','Opp_Avg_Pts','Opp_Avg_Pts_Allowed')

season_join = season_join[,c(2,1,3:8)]

season_join$Offense_Index = season_join$PTS / season_join$Opp_Avg_Pts_Allowed
season_join$Defense_Index = season_join$oPTS / season_join$Opp_Avg_Pts

season_total = aggregate(season_join[,c('Offense_Index','Defense_Index')], by = list(season_join$Team), FUN = mean)
season_deviation = aggregate(season_join[,c('Offense_Index','Defense_Index')], by = list(season_join$Team), FUN = sd)

season_final = cbind(season_total, season_deviation[,2:3])

colnames(season_final) = c('Team','Offense_Index_Mean','Defense_Index_Mean','Offense_Index_Deviation','Defense_Index_Deviation')

team1 = 'DET'
team2 = 'GSW'

team1_data = season_final[season_final$Team == team1,]
team2_data = season_final[season_final$Team == team2,]

team1_index <- rnorm(1000000,mean = team1_data$Offense_Index_Mean * team2_data$Defense_Index_Mean,
                     sd = sqrt(team1_data$Offense_Index_Deviation * team1_data$Offense_Index_Deviation +
                                 team2_data$Defense_Index_Deviation * team2_data$Defense_Index_Deviation))

team2_index <- rnorm(1000000,mean = team2_data$Offense_Index_Mean * team1_data$Defense_Index_Mean,
                     sd = sqrt(team2_data$Offense_Index_Deviation * team2_data$Offense_Index_Deviation +
                                 team1_data$Defense_Index_Deviation * team1_data$Defense_Index_Deviation))

winner = ifelse(team1_index >= team2_index, team1, team2)

team1_score = team1_index*avg_score
team2_score = team2_index*avg_score

simulation = c()
simulation$winner = winner
simulation$team1_score = team1_score
simulation$team2_score = team2_score

simulation = as.data.frame(simulation)

#The first number is team 1 victories, the second number is team 2 victories
library(plyr)

a = count(simulation, "winner")
a$per = a$freq / sum(a$freq)

a$PX4 = (factorial(7)/(factorial(4)*factorial(3))) * (a$per^4) * ((1-a$per)^3)
