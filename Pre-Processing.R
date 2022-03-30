coach <- read.csv("data/coach.csv")
cpi <- read.csv("data/cpi.csv")
division <- read.csv("data/division.csv")
playerscv <- read.csv("data/players cv.csv")
playerssalary <- read.csv("data/players salary.csv")
playersstat <- read.csv("data/players stat.csv")
team <- read.csv("data/team.csv")
advanced_stats <- read.csv("data/NBA_1950to2020_allstats_dataset.csv")


# Gather data only from the 2014-15 season
player_stats <- playersstat[playersstat$Season=="2015-16",]
player_salaries <- playerssalary[playerssalary$SEASON=="2015-2016",]
player_salaries$NAME <- as.character(player_salaries$NAME)
for(i in 1:nrow(player_salaries)){
  player_salaries$NAME[i] <- unlist(strsplit(player_salaries$NAME[i],","))[1]
}
colnames(player_salaries)[3]<-"Player"
players_cv <- playerscv[playerscv$From<= 2015 & playerscv$To>=2016,]


# Merge data from individual players
data <- merge(player_stats, players_cv, by = "Player")
data <- merge(data, player_salaries, by = "Player")
data <- data[,c(1,3,4,6:30,34:38,42)]

# Make categories based on birthplace: USA/International
usa <- read.delim("data/usa.txt")[,1]
data$International = ifelse(data$Place_of_Birth %in% usa, "No", "Yes")


write.csv(data,"data/PlayerData.csv", row.names = TRUE)

attach(data)
summary(lm(BPM ~ OBPM + DBPM))  # R^2 > .999
summary(lm(WS ~ OWS + DWS))  # R^2 > .999
summary(lm(TRB. ~ ORB. + DRB.))  # R^2 > .998