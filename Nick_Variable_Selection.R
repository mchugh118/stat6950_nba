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
summary(full)

avPlots(full, terms=~., intercept = FALSE)

vif(full)[,3]

full2 = lm(logsal ~ . - WS.48 - PER - ORtg - PTS, data = data_regressors) # Sequentially removed highest VIF until next iteration lowered both Adj R2 and Mult R2
summary(full2)
avPlots(full2, terms=~., intercept = FALSE, ellipse = TRUE)
vif(full2)[,3]
