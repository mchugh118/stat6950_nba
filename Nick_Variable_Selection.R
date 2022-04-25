library(car)
library(dplyr)
data <- read_csv("data/PlayerData.csv")

data_regressors <- data %>% mutate(logsal = log(SALARY)) %>% 
  dplyr::select(logsal, Age, G, GS, MP, PER, X3PAr, FTr, ORB., DRB., AST., 
                STL., BLK., TOV., USG., ORtg, DRtg, OWS, DWS, WS.48, OBPM, DBPM, 
                VORP, Pos, Height, PTS, TS., International, Conference)
attach(data_regressors)

Pos <- as.factor(Pos)
International <- as.factor(International)
Conference <- as.factor(Conference)

full = lm(logsal ~ ., data = data_regressors)

avPlots(full, terms=~., intercept = FALSE)

vif(full)[,3]>5
