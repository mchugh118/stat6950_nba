regfit_full <- regsubsets(logsal ~ .^2, data = data, nvmax = 50, method = "backward")
reg_summary <- summary(regfit_full)
names(reg_summary)
reg_summary$adjr2



# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(reg_summary$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
abline(v = 30)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
abline(v = 30)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
abline(v = 30)

plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "bic")
plot(regfit_full, scale = "Cp")
coef(regfit_full, 30)

mod15 <- lm(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
            DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
            stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
            ows_adj:OBPM + Multiteam:dws_adj,
            # weights = mod15wt,
            data = data)

mod20 <- lm(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
              DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
              stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
              ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
              OBPM:Multiteam,
            # weights = mod20wt,
            data = data)

mod25 <- lm(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
              DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
              stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
              ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
              OBPM:Multiteam + gs_adj + gs_adj:orb_adj + PER:dws_adj + PER:Pos_cat,
            # weights = mod25wt,
            data = data)

mod30 <- lm(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
              DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
              stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
              ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
              OBPM:Multiteam + gs_adj + gs_adj:orb_adj + PER:dws_adj + PER:Pos_cat + AST. + MP:Multiteam +
              ftr_adj:ORtg + AST.:Multiteam,
            weights = mod30wt,
            data = data)

anova(mod15, mod20, test = "F")
anova(mod20, mod25, test = "F")
anova(mod25, mod30, test = "F")


mod <- mod20


# 6-fold cross-validation
set.seed(1396)
ctrl <- trainControl(method = "cv", number = 6)

model15 <- train(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
                   DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
                   stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
                   ows_adj:OBPM + Multiteam:dws_adj,
                 data = data,
                 method = "lm", 
                 # weights = mod15wt,
                 trControl = ctrl)

model20 <- train(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
                   DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
                   stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
                   ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
                   OBPM:Multiteam,
                 data = data,
                 method = "lm", 
                 # weights = mod20wt,
                 trControl = ctrl)

model25 <- train(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
                   DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
                   stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
                   ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
                   OBPM:Multiteam + gs_adj + gs_adj:orb_adj + PER:dws_adj + PER:Pos_cat,
                 data = data,
                 method = "lm", 
                 # weights = mod25wt,
                 trControl = ctrl)

model30 <- train(logsal ~ vorp_adj + Multiteam + Age + ORtg + Age:ORtg + G + PER + G:PER + MP + MP:PER + 
                   DBPM + PER:DBPM + ftr_adj + USG. + Height + ftr_adj:USG. + ftr_adj:Height + DRB. + DRB.:Multiteam +
                   stl_adj + stl_adj:DBPM + ORtg:dws_adj + dws_adj + ORtg:Pos_cat + Pos_cat + ows_adj + OBPM +
                   ows_adj:OBPM + Multiteam:dws_adj + orb_adj + blk_adj + PER:Multiteam + X3PAr + X3PAr:Multiteam +
                   OBPM:Multiteam + gs_adj + gs_adj:orb_adj + PER:dws_adj + PER:Pos_cat + AST. + MP:Multiteam +
                   ftr_adj:ORtg + AST.:Multiteam,
                 data = data,
                 method = "lm", 
                 # weights = mod30wt,
                 trControl = ctrl)

print(model15)
print(model20)
print(model25)
print(model30)
