library(stringr)
library(stringi)



coach <- read.csv("data/coach.csv")
cpi <- read.csv("data/cpi.csv")
division <- read.csv("data/division.csv")
playerscv <- read.csv("data/players cv.csv")
playerssalary <- read.csv("data/players salary.csv")
playersstat <- read.csv("data/players stat.csv")
team <- read.csv("data/team.csv")
advanced_stats <- read.csv("data/NBA_1950to2020_allstats_dataset.csv")
advanced_stats <- advanced_stats[advanced_stats$Season == 2016,]
advanced_stats$Player <- gsub(" Jr.", "", advanced_stats$Player)
advanced_stats$Player <- gsub(" III", "", advanced_stats$Player)
advanced_stats$Player <- stri_trans_general(advanced_stats$Player, "Latin-ASCII")
advanced_stats$Player <- str_replace_all(advanced_stats$Player, '[^[:alnum:][:space:]]', '')
euro_names <- c("Alexis AjinAa", "Anderson VarejAo", "Boban MarjanoviA", "Bojan BogdanoviA", "Damjan RudeA 34", "Dennis SchrAder", 
                "Donatas MotiejAnas", "Ersan Alyasova", "Goran DragiA", "Greivis VAsquez", "Jonas ValanAiAnas", "Jorge GutiACrrez",
                "JosAC CalderAn", "Jusuf NurkiA", "Kristaps PorziAAis", "Manu GinAbili", "Mirza TeletoviA", "NenAª",
                "Nikola JokiA", "Nikola MirotiA", "Nikola PekoviA", "Nikola VuAeviA", "Omer Asik", "Patty Mills", 
                "Tibor PleiAY", "Walter Tavares")
english_names <- c("Alexis Ajinca", "Anderson Varejao", "Boban Marjanovic","Bojan Bogdanovic", "Damjan Rudez", "Dennis Schroder", 
                    "Donatas Motiejunas", "Ersan Ilyasova", "Goran Dragic", "Greivis Vasquez", "Jonas Valanciunas", "Jorge Gutierrez",
                    "Jose Calderon", "Jusuf Nurkic", "Kristaps Porzingis", "Manu Ginobili", "Mirza Teletovic", "Nene Hilario",
                    "Nikola Jokic", "Nikola Mirotic", "Nikola Pekovic", "Nikola Vucevic", "Omer Asik", "Patrick Mills", 
                    "Tibor Pleiss", "Walter Tavares")
for (i in 1:length(euro_names)){
  advanced_stats$Player <- gsub(euro_names[i], english_names[i], advanced_stats$Player)
}

# Gather data only from the 2014-15 season
playersstat[!playersstat$Player == "Sam Dekker",]
player_stats <- playersstat[playersstat$Season=="2015-16",]
player_stats <- player_stats[!player_stats$Player == "Sam Dekker",]

player_salaries <- playerssalary[playerssalary$SEASON=="2015-2016",]
player_salaries$NAME <- as.character(player_salaries$NAME)
for(i in 1:nrow(player_salaries)){
  player_salaries$NAME[i] <- unlist(strsplit(player_salaries$NAME[i],","))[1]
}
colnames(player_salaries)[3]<-"Player"
player_salaries$Player <- gsub(" Jr.", "", player_salaries$Player)
player_salaries$Player <- gsub(" III", "", player_salaries$Player)
player_salaries$Player <- str_replace_all(player_salaries$Player, '[^[:alnum:][:space:]]', '')
player_salaries$Player <- str_replace_all(player_salaries$Player, c("Patty Mills", "Louis Williams"), c("Patrick Mills", "Lou Williams"))


#players_cv <- playerscv[playerscv$From<= 2015 & playerscv$To>=2016,]


# Merge data from individual players
data <- merge(player_stats, playerscv, by = "Player")
data$Player <- gsub(" Jr.", "", data$Player)
data$Player <- gsub(" III", "", data$Player)
data$Player <- str_replace_all(data$Player, '[^[:alnum:][:space:]]', '')

data <- merge(data, player_salaries, by = "Player")
data <- data[,c(1,3,4,6:30,34:38,42)]

for(i in 1:nrow(data)){
  data$Height[i] <- as.numeric(unlist(strsplit(as.character(data$Ht[i]),"-"))[1])*12 + as.numeric(unlist(strsplit(as.character(data$Ht[i]),"-"))[2])
}
data <- merge(data,advanced_stats[,c("Player", "PTS", "TS.")], by="Player")


# Make categories based on birthplace: USA/International
usa <- read.delim("data/usa.txt")[,1]
data$International = ifelse(data$Place_of_Birth %in% usa, "No", "Yes")


write.csv(data,"data/PlayerData.csv", row.names = TRUE)


attach(data)
summary(lm(BPM ~ OBPM + DBPM))  # R^2 > .999
summary(lm(WS ~ OWS + DWS))  # R^2 > .999
summary(lm(TRB. ~ ORB. + DRB.))  # R^2 > .998

data <- data[,-c(12,22,26)]
