################## NHL WIN PREDICTION MODEL ################## 
################## MARIE ERWOOD ################## 
################## MSC DATA SCIENCE PROJECT ################## 

############## LIBRARIES AND SEED ############## 

library(tidyverse)
library(stringr)
library(caret)
library(magrittr)
library(lubridate)
library(corrplot)
library(tree)
library(randomForest)
library(boot)
library(scales)
library(ggridges)


set.seed(2134)

############## NST DATA CLEANING ##################

nst2014 <- read.csv("Natural Stat Trick_2014_15.csv", 
                stringsAsFactors = F)

nst2015 <- read.csv("Natural Stat Trick_2015_16.csv", 
                    stringsAsFactors = F)

nst2016 <- read.csv("Natural Stat Trick_2016_17.csv", 
                    stringsAsFactors = F)

nst2017 <- read.csv("Natural Stat Trick_2017_18.csv", 
                    stringsAsFactors = F)

nst2018 <- read.csv("Natural Stat Trick_2018_19.csv", 
                    stringsAsFactors = F)

impute_data <- function(df){
  
  df <- df%>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
              HDGF. = as.double(ifelse(HDGF. == "-", 0, HDGF.)),
              MDGF. = as.double(ifelse(MDGF. == "-", 0, MDGF.)),
              MDSH. = as.double(ifelse(MDSH. == "-", 0, MDSH.)),
              MDSV. = as.double(ifelse(MDSV. == "-", 0, MDSV.)),
              LDGF. = as.double(ifelse(LDGF. == "-", 0, LDGF.)),
              HDSH. = as.double(ifelse(HDSH. == "-", 0, HDSH.)),
              HDSV. = as.double(ifelse(HDSV. == "-", 0, HDSV.)),
              LDSH. = as.double(ifelse(LDSH. == "-", 0, LDSH.)),
              LDSV. = as.double(ifelse(LDSV. == "-", 0, LDSV.)),
              SCF. = as.double(ifelse(SCF. == "-", 0, SCF.)),
              HDCF. = as.double(ifelse(HDCF. == "-", 0, HDCF.)),
              HDSF. = as.double(ifelse(HDSF. == "-", 0, HDSF.)),
              MDCF. = as.double(ifelse(MDCF. == "-", 0, MDCF.)),
              MDSF. = as.double(ifelse(MDSF. == "-", 0, MDSF.)),
              LDCF. = as.double(ifelse(LDCF. == "-", 0, LDCF.)),
              LDSF. = as.double(ifelse(LDSF. == "-", 0, LDSF.)),
              xGF. = as.double(ifelse(xGF. == "-", 0, xGF.))
  )%>%
    select(-X)
  
}

nst2014 <- impute_data(nst2014)
nst2015 <- impute_data(nst2015)
nst2016 <- impute_data(nst2016)
nst2017 <- impute_data(nst2017)
nst2018 <- impute_data(nst2018)



nst <- nst2014 %>%
  bind_rows(nst2015)%>%
  bind_rows(nst2016)%>%
  bind_rows(nst2017)%>%
  bind_rows(nst2018)

colnames(nst) <- str_replace(names(nst), "\\.", "perc")

nst %<>%
  mutate(Team = case_when(Team == "Anaheim Ducks" ~ "ANA",
                          Team == "Arizona Coyotes" ~ "ARI",
                          Team == "Boston Bruins" ~ "BOS",
                          Team == "Buffalo Sabres" ~ "BUF",
                          Team == "Carolina Hurricanes" ~ "CAR",
                          Team == "Columbus Blue Jackets" ~ "CBJ",
                          Team == "Calgary Flames" ~ "CGY",
                          Team == "Chicago Blackhawks" ~ "CHI",
                          Team == "Colorado Avalanche" ~ "COL",
                          Team == "Dallas Stars" ~ "DAL",
                          Team == "Detroit Red Wings" ~ "DET",
                          Team == "Edmonton Oilers" ~ "EDM",
                          Team == "Florida Panthers" ~ "FLA",
                          Team == "Los Angeles Kings" ~ "L.A",
                          Team == "Minnesota Wild" ~ "MIN",
                          Team == "Montreal Canadiens" ~ "MTL",
                          Team == "New Jersey Devils" ~ "N.J",
                          Team == "Nashville Predators" ~ "NSH",
                          Team == "New York Islanders" ~ "NYI",
                          Team == "New York Rangers" ~ "NYR",
                          Team == "Ottawa Senators" ~ "OTT",
                          Team == "Philadelphia Flyers" ~ "PHI",
                          Team == "Pittsburgh Penguins" ~ "PIT",
                          Team == "San Jose Sharks" ~ "S.J",
                          Team == "St Louis Blues" ~ "STL",
                          Team == "Tampa Bay Lightning" ~ "T.B",
                          Team == "Toronto Maple Leafs" ~ "TOR",
                          Team == "Vancouver Canucks" ~ "VAN",
                          Team == "Vegas Golden Knights" ~ "VGK",
                          Team == "Washington Capitals" ~ "WSH",
                          Team == "Winnipeg Jets" ~ "WPG"))
  

nst_avg <- nst %>%
  split(.$Team)%>%
  map(~ mutate_if(., is.numeric, cummean))%>%
  bind_rows()

nst %<>%
  separate(Game, c("gameDate", "Game"), sep = "\\s\\-\\s")%>%
    mutate(Game = str_replace(Game, "[a-z]\\s[A-Z]", ""))%>%
    mutate(Game = str_replace(Game, "[a-z]\\s[A-Z]", ""))%>%
    separate(Game, c("awayTeam", "awayGoals", "homeTeam", "homeGoals"), 
             sep = "\\s")%>%
    mutate(awayGoals = str_remove(awayGoals, "\\,"))%>%
    mutate(awayTeam = case_when(awayTeam == "Ducks" ~ "ANA",
                                awayTeam == "Coyotes" ~ "ARI",
                                awayTeam == "Bruins" ~ "BOS",
                                awayTeam == "Sabres" ~ "BUF",
                                awayTeam == "Hurricanes" ~ "CAR",
                                awayTeam == "Bluackets" ~ "CBJ",
                                awayTeam == "Flames" ~ "CGY",
                                awayTeam == "Blackhawks" ~ "CHI",
                                awayTeam == "Avalanche" ~ "COL",
                                awayTeam == "Stars" ~ "DAL",
                                awayTeam == "Reings" ~ "DET",
                                awayTeam == "Oilers" ~ "EDM",
                                awayTeam == "Panthers" ~ "FLA",
                                awayTeam == "Kings" ~ "L.A",
                                awayTeam == "Wild" ~ "MIN",
                                awayTeam == "Canadiens" ~ "MTL",
                                awayTeam == "Devils" ~ "N.J",
                                awayTeam == "Predators" ~ "NSH",
                                awayTeam == "Islanders" ~ "NYI",
                                awayTeam == "Rangers" ~ "NYR",
                                awayTeam == "Senators" ~ "OTT",
                                awayTeam == "Flyers" ~ "PHI",
                                awayTeam == "Penguins" ~ "PIT",
                                awayTeam == "Sharks" ~ "S.J",
                                awayTeam == "Blues" ~ "STL",
                                awayTeam == "Lightning" ~ "T.B",
                                awayTeam == "Mapleafs" ~ "TOR",
                                awayTeam == "Canucks" ~ "VAN",
                                awayTeam == "Goldenights" ~ "VGK",
                                awayTeam == "Capitals" ~ "WSH",
                                awayTeam == "Jets" ~ "WPG"))%>%
    mutate(homeTeam = case_when(homeTeam == "Ducks" ~ "ANA",
                                homeTeam == "Coyotes" ~ "ARI",
                                homeTeam == "Bruins" ~ "BOS",
                                homeTeam == "Sabres" ~ "BUF",
                                homeTeam == "Hurricanes" ~ "CAR",
                                homeTeam == "Bluackets" ~ "CBJ",
                                homeTeam == "Flames" ~ "CGY",
                                homeTeam == "Blackhawks" ~ "CHI",
                                homeTeam == "Avalanche" ~ "COL",
                                homeTeam == "Stars" ~ "DAL",
                                homeTeam == "Reings" ~ "DET",
                                homeTeam == "Oilers" ~ "EDM",
                                homeTeam == "Panthers" ~ "FLA",
                                homeTeam == "Kings" ~ "L.A",
                                homeTeam == "Wild" ~ "MIN",
                                homeTeam == "Canadiens" ~ "MTL",
                                homeTeam == "Devils" ~ "N.J",
                                homeTeam == "Predators" ~ "NSH",
                                homeTeam == "Islanders" ~ "NYI",
                                homeTeam == "Rangers" ~ "NYR",
                                homeTeam == "Senators" ~ "OTT",
                                homeTeam == "Flyers" ~ "PHI",
                                homeTeam == "Penguins" ~ "PIT",
                                homeTeam == "Sharks" ~ "S.J",
                                homeTeam == "Blues" ~ "STL",
                                homeTeam == "Lightning" ~ "T.B",
                                homeTeam == "Mapleafs" ~ "TOR",
                                homeTeam == "Canucks" ~ "VAN",
                                homeTeam == "Goldenights" ~ "VGK",
                                homeTeam == "Capitals" ~ "WSH",
                                homeTeam == "Jets" ~ "WPG"))%>%
    mutate(gameDate = ymd(gameDate))%>%
    mutate(season = case_when(gameDate < "2015-07-01" ~ 2014,
                              gameDate < "2016-07-01" & gameDate > "2015-09-01" ~ 2015,
                              gameDate < "2017-07-01" & gameDate > "2016-09-01"~ 2016,
                              gameDate < "2018-07-01" & gameDate > "2017-09-01"~ 2017,
                              gameDate < "2019-07-01" & gameDate > "2018-09-01" ~ 2018))%>%
    select(season, everything())%>%
    mutate(home_win = ifelse(homeGoals > awayGoals, 1, 0))%>%
    select(-Attendance)%>%
    mutate(homeGoals = as.numeric(homeGoals), awayGoals = as.numeric(awayGoals))



nst_avg %<>% 
  separate(Game, c("gameDate", "Game"), sep = "\\s\\-\\s")%>%
  mutate(Game = str_replace(Game, "[a-z]\\s[A-Z]", ""))%>%
  mutate(Game = str_replace(Game, "[a-z]\\s[A-Z]", ""))%>%
  separate(Game, c("awayTeam", "awayGoals", "homeTeam", "homeGoals"), 
           sep = "\\s")%>%
  mutate(awayGoals = str_remove(awayGoals, "\\,"))%>%
  mutate(awayTeam = case_when(awayTeam == "Ducks" ~ "ANA",
                              awayTeam == "Coyotes" ~ "ARI",
                              awayTeam == "Bruins" ~ "BOS",
                              awayTeam == "Sabres" ~ "BUF",
                              awayTeam == "Hurricanes" ~ "CAR",
                              awayTeam == "Bluackets" ~ "CBJ",
                              awayTeam == "Flames" ~ "CGY",
                              awayTeam == "Blackhawks" ~ "CHI",
                              awayTeam == "Avalanche" ~ "COL",
                              awayTeam == "Stars" ~ "DAL",
                              awayTeam == "Reings" ~ "DET",
                              awayTeam == "Oilers" ~ "EDM",
                              awayTeam == "Panthers" ~ "FLA",
                              awayTeam == "Kings" ~ "L.A",
                              awayTeam == "Wild" ~ "MIN",
                              awayTeam == "Canadiens" ~ "MTL",
                              awayTeam == "Devils" ~ "N.J",
                              awayTeam == "Predators" ~ "NSH",
                              awayTeam == "Islanders" ~ "NYI",
                              awayTeam == "Rangers" ~ "NYR",
                              awayTeam == "Senators" ~ "OTT",
                              awayTeam == "Flyers" ~ "PHI",
                              awayTeam == "Penguins" ~ "PIT",
                              awayTeam == "Sharks" ~ "S.J",
                              awayTeam == "Blues" ~ "STL",
                              awayTeam == "Lightning" ~ "T.B",
                              awayTeam == "Mapleafs" ~ "TOR",
                              awayTeam == "Canucks" ~ "VAN",
                              awayTeam == "Goldenights" ~ "VGK",
                              awayTeam == "Capitals" ~ "WSH",
                              awayTeam == "Jets" ~ "WPG"))%>%
  mutate(homeTeam = case_when(homeTeam == "Ducks" ~ "ANA",
                              homeTeam == "Coyotes" ~ "ARI",
                              homeTeam == "Bruins" ~ "BOS",
                              homeTeam == "Sabres" ~ "BUF",
                              homeTeam == "Hurricanes" ~ "CAR",
                              homeTeam == "Bluackets" ~ "CBJ",
                              homeTeam == "Flames" ~ "CGY",
                              homeTeam == "Blackhawks" ~ "CHI",
                              homeTeam == "Avalanche" ~ "COL",
                              homeTeam == "Stars" ~ "DAL",
                              homeTeam == "Reings" ~ "DET",
                              homeTeam == "Oilers" ~ "EDM",
                              homeTeam == "Panthers" ~ "FLA",
                              homeTeam == "Kings" ~ "L.A",
                              homeTeam == "Wild" ~ "MIN",
                              homeTeam == "Canadiens" ~ "MTL",
                              homeTeam == "Devils" ~ "N.J",
                              homeTeam == "Predators" ~ "NSH",
                              homeTeam == "Islanders" ~ "NYI",
                              homeTeam == "Rangers" ~ "NYR",
                              homeTeam == "Senators" ~ "OTT",
                              homeTeam == "Flyers" ~ "PHI",
                              homeTeam == "Penguins" ~ "PIT",
                              homeTeam == "Sharks" ~ "S.J",
                              homeTeam == "Blues" ~ "STL",
                              homeTeam == "Lightning" ~ "T.B",
                              homeTeam == "Mapleafs" ~ "TOR",
                              homeTeam == "Canucks" ~ "VAN",
                              homeTeam == "Goldenights" ~ "VGK",
                              homeTeam == "Capitals" ~ "WSH",
                              homeTeam == "Jets" ~ "WPG"))%>%
  mutate(gameDate = ymd(gameDate))%>%
  mutate(season = case_when(gameDate < "2015-07-01" ~ 2014,
                            gameDate < "2016-07-01" & gameDate > "2015-09-01" ~ 2015,
                            gameDate < "2017-07-01" & gameDate > "2016-09-01"~ 2016,
                            gameDate < "2018-07-01" & gameDate > "2017-09-01"~ 2017,
                            gameDate < "2019-07-01" & gameDate > "2018-09-01" ~ 2018))%>%
  select(season, everything())%>%
  mutate(home_win = ifelse(homeGoals > awayGoals, 1, 0))%>%
  select(-Attendance)%>%
  mutate(homeGoals = as.numeric(homeGoals), awayGoals = as.numeric(awayGoals))

nst_avg%>%
  summarise(home_wins = mean(home_win==1))

nst_avg_home <- nst_avg %>%
  filter(homeTeam==Team)
  
nst_avg_away <- nst_avg %>%
  filter(awayTeam==Team)

colnames(nst_avg_home)<- paste0("h_", names(nst_avg_home))

colnames(nst_avg_away)<- paste0("a_", names(nst_avg_away))

#### checking correlations:

nst_h_cor <- nst_avg_home%>%
  select(-c(h_season:h_Team))

nst_h_cor <- cor(nst_h_cor)

corrplot(nst_h_cor, tl.cex = 0.5)

nst_a_cor <- nst_avg_away%>%
  select(-c(a_season:a_Team))

nst_a_cor <- cor(nst_a_cor)

corrplot(nst_a_cor, tl.cex = 0.5)



nst_all <- nst_avg_home %>%
  left_join(nst_avg_away, by = c("h_gameDate" = "a_gameDate", 
                                 "h_homeTeam" = "a_homeTeam",
                                 "h_awayTeam" = "a_awayTeam"))%>%
  select(-c(a_season:a_homeGoals, a_home_win, h_Team, a_Team))

############## DATA FOR APP SELECTIONS ##################

season <- nst_avg%>%
  select(season)%>%
  unique()

team_names <- nst_avg%>%
  select(Team)%>%
  unique()%>%
  arrange(Team)%>%
  filter(Team != "ATL")

############## REMOVE UNNEEDED DATAFRAMES ##################

rm(nst_avg_away, nst_avg_home, nst2014, nst2015, nst2016, nst2017, nst2018)

############## NST MODEL CREATION - LOGISTIC ##################

#### data processing: ----
nst_process <- nst_all %>%
  mutate_at(scale, .vars = vars(-c(h_season:h_homeGoals, h_home_win)))%>%
  mutate_if(is.character, as.factor)

model_train <- nst_process %>%
  filter(h_season != 2018)

model_test <- nst_process %>%
  filter(h_season == 2018)

#### functions and parameters: ----

# logloss function via https://www.r-bloggers.com/making-sense-of-logarithmic-loss/
# last accessed 2019-09-15

log_loss <- function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
              - (sum(actual * log(predicted) 
              + (1 - actual) * log(1 - predicted))) / length(actual)
}

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)


#### Model A: very simple model: ----

mod_a_train <- train(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+
                        h_GF+a_GF, 
                        method = "glm",data = model_train, family = "binomial",
                        trControl = ctrl)

pred = predict(mod_a_train, newdata=model_test)

(a_res <- confusionMatrix(data=pred, model_test$h_home_win, positive = "1"))

mod_a_pred <- predict(mod_a_train, newdata = model_test, type = "prob")

mod_a_res <- model_test %>% 
  mutate(prob = mod_a_pred[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = mod_a_res$h_home_win, pred = mod_a_res$pred_win)

# Log loss:

(al <- log_loss(mod_a_res$h_home_win, mod_a_res$prob))


##### Model B: selected variables: -----

mod_b_train <- train(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_CF+h_CA
                      +h_FF+h_FA+h_SF+h_SA+h_GF+h_GA+h_xGF+h_xGA+h_SVperc+h_SHperc
                      +h_PDO+a_CF+a_CA+a_FF+a_FA+a_SF+a_SA+a_GF+a_GA+a_xGF+a_xGA+a_SVperc
                      +a_SHperc+a_PDO, method = "glm",
                      data = model_train, family = "binomial", trControl = ctrl)

pred = predict(mod_b_train, newdata=model_test)

(b_res <- confusionMatrix(data=pred, model_test$h_home_win, positive = "1"))

mod_b_pred <- predict(mod_b_train, newdata = model_test, type = "prob")

mod_b_res <- model_test %>% 
  mutate(prob = mod_b_pred[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = mod_b_res$h_home_win, pred = mod_b_res$pred_win)

# Log loss:

(bl <- log_loss(mod_b_res$h_home_win, mod_b_res$prob))


##### Model C: percentage metrics only --------

mod_c_train <- train(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_CFperc+h_FFperc+h_SFperc+h_GFperc+
            h_xGFperc+h_SCFperc+h_HDCFperc+h_HDSFperc+h_HDGFperc+h_HDSHperc+
            h_HDSVperc+h_MDCFperc+h_MDSFperc+h_MDGFperc+h_MDSHperc+h_MDSVperc+
            h_LDCFperc+h_LDSFperc+h_LDGFperc+h_LDSHperc+h_LDSVperc+h_PDO+
            a_CFperc+a_FFperc+a_SFperc+a_GFperc+a_xGFperc+a_SCFperc+a_HDCFperc+
            a_HDSFperc+a_HDGFperc+a_HDSHperc+a_HDSVperc+a_MDCFperc+a_MDSFperc+
            a_MDGFperc+a_MDSHperc+a_MDSVperc+a_LDCFperc+a_LDSFperc+a_LDGFperc+
            a_LDSHperc+a_LDSVperc+a_PDO, method = "glm",data = model_train, family = "binomial",
            trControl = ctrl)


pred = predict(mod_c_train, newdata=model_test)

(c_res <- confusionMatrix(data=pred, model_test$h_home_win, positive = "1"))

mod_c_pred <- predict(mod_c_train, newdata = model_test, type = "prob")

mod_c_res <- model_test %>% 
  mutate(prob = mod_c_pred[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = mod_c_res$h_home_win, pred = mod_c_res$pred_win)

# Log loss:

(cl <- log_loss(mod_c_res$h_home_win, mod_c_res$prob))


#### Model D: significant variables from Model B: ----

varImp(mod_b_train)

mod_d_train <- train(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_GF+a_GF+
                        a_xGA+h_SHperc+h_PDO+h_SVperc+a_PDO+a_SVperc+a_SHperc+h_FA,
                     method = "glm",data = model_train, family = "binomial",
                     trControl = ctrl)

pred = predict(mod_d_train, newdata=model_test)

(d_res <- confusionMatrix(data=pred, model_test$h_home_win, positive = "1"))

mod_d_pred <- predict(mod_d_train, newdata = model_test, type = "prob")

mod_d_res <- model_test %>% 
  mutate(prob = mod_d_pred[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = mod_d_res$h_home_win, pred = mod_d_res$pred_win)

# Log loss:

(dl <- log_loss(mod_d_res$h_home_win, mod_d_res$prob))


#### Model E: significant variables from Model C: ----

varImp(mod_c_train)

mod_e_train <- train(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+
                       a_GFperc+h_HDGFperc+h_LDCFperc+h_PDO+
                       h_FFperc+h_GFperc+h_CFperc+a_SCFperc+h_xGFperc, 
                     method = "glm",data = model_train, family = "binomial",
                     trControl = ctrl)


pred = predict(mod_e_train, newdata=model_test)

(e_res <- confusionMatrix(data=pred, model_test$h_home_win, positive = "1"))

mod_e_pred <- predict(mod_e_train, newdata = model_test, type = "prob")

mod_e_res <- model_test %>% 
  mutate(prob = mod_e_pred[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = mod_e_res$h_home_win, pred = mod_e_res$pred_win)

# Log loss:

(el <- log_loss(mod_e_res$h_home_win, mod_e_res$prob))


#### results table: ----

results <- data.frame(model_name = c("A_log", "B_log", "C_log", "D_log", "E_log"),
                      accuracy = c(a_res$overall[1],
                                   b_res$overall[1],
                                   c_res$overall[1],
                                   d_res$overall[1],
                                   e_res$overall[1]),
                      logloss = c(al,bl,cl,dl,el))

(results)


############## NST MODEL CREATION - TREES ##################

#### data processing (as above in logistic model): ----
nst_process <- nst_all %>%
  mutate_at(scale, .vars = vars(-c(h_season:h_homeGoals, h_home_win)))%>%
  mutate_if(is.character, as.factor)

model_train <- nst_process %>%
  filter(h_season != 2018)

model_test <- nst_process %>%
  filter(h_season == 2018)


#### single tree: ----

tree1 <- tree(as.factor(h_home_win) ~ .-h_awayGoals -h_homeGoals -h_TOI -a_TOI 
              -h_gameDate -h_season, data = model_train)

tree1_pred <- predict(tree1, newdata = model_test, type = "class")

tree1_preds <- model_test %>% 
  mutate(pred_win = tree1_pred)%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tree1_preds$h_home_win, pred = tree1_preds$pred_win)

summary(tree1)

#### Model A: simple randomForest: ----

treefita <- randomForest(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+
                           h_GF+a_GF, data = model_train, ntree = 500,
                        importance = TRUE)
treefita
summary(treefita)
varImpPlot(treefita)

tfa <- predict(treefita, newdata = model_test, type = "prob")


tf_a <- model_test %>% 
  mutate(prob = tfa[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf_a$h_home_win, pred = tf_a$pred_win)

(at_a <- (514+192)/(514+192+397+168))


# Log loss:

(atl <- log_loss(tf_a$h_home_win, tf_a$prob))


#### Model B: selected variables: ----

treefitb <- randomForest(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_CF+h_CA
                         +h_FF+h_FA+h_SF+h_SA+h_GF+h_GA+h_xGF+h_xGA+h_SVperc+h_SHperc
                         +h_PDO+a_CF+a_CA+a_FF+a_FA+a_SF+a_SA+a_GF+a_GA+a_xGF+a_xGA+a_SVperc
                         +a_SHperc+a_PDO, data = model_train, ntree = 500,
                         importance = TRUE)
treefitb
summary(treefitb)
varImpPlot(treefitb)

tfb <- predict(treefitb, newdata = model_test, type = "prob")


tf_b <- model_test %>% 
  mutate(prob = tfb[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf_b$h_home_win, pred = tf_b$pred_win)

(bt_a <- (515+190)/(515+190+399+167))


# Log loss:

(btl <- log_loss(tf_b$h_home_win, tf_b$prob))


##### Model C: percentage metrics only --------

treefitc <- randomForest(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_CFperc+h_FFperc+h_SFperc+h_GFperc+
                       h_xGFperc+h_SCFperc+h_HDCFperc+h_HDSFperc+h_HDGFperc+h_HDSHperc+
                       h_HDSVperc+h_MDCFperc+h_MDSFperc+h_MDGFperc+h_MDSHperc+h_MDSVperc+
                       h_LDCFperc+h_LDSFperc+h_LDGFperc+h_LDSHperc+h_LDSVperc+h_PDO+
                       a_CFperc+a_FFperc+a_SFperc+a_GFperc+a_xGFperc+a_SCFperc+a_HDCFperc+
                       a_HDSFperc+a_HDGFperc+a_HDSHperc+a_HDSVperc+a_MDCFperc+a_MDSFperc+
                       a_MDGFperc+a_MDSHperc+a_MDSVperc+a_LDCFperc+a_LDSFperc+a_LDGFperc+
                       a_LDSHperc+a_LDSVperc+a_PDO, data = model_train, ntree = 500,
                       importance = TRUE)


treefitc
summary(treefitc)
varImpPlot(treefitc)

tfc <- predict(treefitc, newdata = model_test, type = "prob")


tf_c <- model_test %>% 
  mutate(prob = tfc[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf_c$h_home_win, pred = tf_c$pred_win)

(ct_a <- (506+203)/(506+203+386+176))


# Log loss:

(ctl <- log_loss(tf_c$h_home_win, tf_c$prob))

#### Model D: most important variables from Model B: ----

varImpPlot(treefitb)

treefitd <- randomForest(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_PDO+
                            h_GF+a_GA+h_SHperc+h_xGA+a_PDO+h_SVperc+h_GA+a_GF,
                            data = model_train, ntree = 500,
                            importance = TRUE)
treefitd
summary(treefitd)
varImpPlot(treefitd)

tfd <- predict(treefitd, newdata = model_test, type = "prob")


tf_d <- model_test %>% 
  mutate(prob = tfd[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf_d$h_home_win, pred = tf_d$pred_win)

(dt_a <- (527+188)/(527+188+401+155))

# Log loss:

(dtl <- log_loss(tf_d$h_home_win, tf_d$prob))


#### Model E: most important variables from Model C: ----

varImpPlot(treefitc)

treefite <- randomForest(as.factor(h_home_win) ~ h_homeTeam+h_awayTeam+h_GFperc+a_GFperc+
                            h_MDSFperc+h_PDO+h_MDCFperc+a_PDO+a_HDSFperc+h_SCFperc+
                            h_SFperc, data = model_train, ntree = 500,
                            importance = TRUE)


treefite
summary(treefite)
varImpPlot(treefite)

tfe <- predict(treefite, newdata = model_test, type = "prob")


tf_e <- model_test %>% 
  mutate(prob = tfe[,2])%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf_e$h_home_win, pred = tf_e$pred_win)

(et_a <- (512+195)/(512+195+394+170))

# Log loss:

(etl <- log_loss(tf_e$h_home_win, tf_e$prob))

#### results table: ----

results_tree <- data.frame(model_name = c("A_tree", "B_tree", "C_tree", "D_tree", "E_tree"),
                           accuracy = c(at_a,
                                   bt_a,
                                   ct_a,
                                   dt_a,
                                   et_a),
                      logloss = c(atl,btl,ctl,dtl,etl))

(results_tree)

############## NST MODEL SELECTION ##################

overall_results <- results%>%
  bind_rows(results_tree)

(overall_results)


############## DATA PREP FOR APP ##################

#### score totals: ----

team_totals <- read.csv("Team Season Totals - Natural Stat Trick.csv",
                        stringsAsFactors = F)

team_totals %<>%
  mutate(Team_Code = case_when(Team == "Anaheim Ducks" ~ "ANA",
                          Team == "Arizona Coyotes" ~ "ARI",
                          Team == "Boston Bruins" ~ "BOS",
                          Team == "Buffalo Sabres" ~ "BUF",
                          Team == "Carolina Hurricanes" ~ "CAR",
                          Team == "Columbus Blue Jackets" ~ "CBJ",
                          Team == "Calgary Flames" ~ "CGY",
                          Team == "Chicago Blackhawks" ~ "CHI",
                          Team == "Colorado Avalanche" ~ "COL",
                          Team == "Dallas Stars" ~ "DAL",
                          Team == "Detroit Red Wings" ~ "DET",
                          Team == "Edmonton Oilers" ~ "EDM",
                          Team == "Florida Panthers" ~ "FLA",
                          Team == "Los Angeles Kings" ~ "L.A",
                          Team == "Minnesota Wild" ~ "MIN",
                          Team == "Montreal Canadiens" ~ "MTL",
                          Team == "New Jersey Devils" ~ "N.J",
                          Team == "Nashville Predators" ~ "NSH",
                          Team == "New York Islanders" ~ "NYI",
                          Team == "New York Rangers" ~ "NYR",
                          Team == "Ottawa Senators" ~ "OTT",
                          Team == "Philadelphia Flyers" ~ "PHI",
                          Team == "Pittsburgh Penguins" ~ "PIT",
                          Team == "San Jose Sharks" ~ "S.J",
                          Team == "St Louis Blues" ~ "STL",
                          Team == "Tampa Bay Lightning" ~ "T.B",
                          Team == "Toronto Maple Leafs" ~ "TOR",
                          Team == "Vancouver Canucks" ~ "VAN",
                          Team == "Vegas Golden Knights" ~ "VGK",
                          Team == "Washington Capitals" ~ "WSH",
                          Team == "Winnipeg Jets" ~ "WPG"))%>%
  select(Team, Team_Code, everything())

colnames(team_totals) <- str_replace(names(team_totals), "\\.", "perc")


#### estimating total points: -----

points_pred <- mod_e_res %>%
  select(h_season:h_home_win, prob, pred_win)%>%
  mutate(h_pred_points = ifelse(pred_win == 1, 2, 0),
         a_pred_points = ifelse(pred_win == 1, 0, 2))

h_points_pred <- points_pred %>%
  select(h_season, h_gameDate, Team = h_homeTeam, 
         pred_points = h_pred_points)


a_points_pred <- points_pred %>%
  select(h_season, h_gameDate, Team = h_awayTeam, 
         pred_points = a_pred_points)

points_pred <- h_points_pred %>%
  bind_rows(a_points_pred)%>%
  mutate(h_season = as.factor(h_season))

points_preds <- points_pred%>%
  group_by(h_season, Team)%>%
  summarise(total_pred_points = sum(pred_points))

### bootstrapped points: ----

# boot_points function amended from https://stats.idre.ucla.edu/r/library/r-library-introduction-to-bootstrapping/
# last accessed 2019-09-15

boot_points <- function(df, sample_size) {
  resamples <- lapply(1:sample_size, function(i) sample(df, 82, replace=TRUE))
  points_sum <- sapply(resamples, sum)
  list(sums=points_sum)   
}

points_boot <- points_pred %>%
  select(-c(h_season, h_gameDate))%>%
  split(.$Team)%>%
  map(~ boot_points(.[,-1], 50000))%>%
  map_df(., ~as.data.frame(.), .id="id")

### scale to reasonable range: ----

points_boot %<>%
  mutate(sums_scaled = rescale(sums, to = c(45, 130)))

points_boot_summary <- points_boot%>%
  group_by(id)%>%
  summarise(point_prediction = mean(sums_scaled))%>%
  select(Team = id, point_prediction)%>%
  left_join(team_totals, by = c("Team" = "Team_Code"))%>%
  select(Team, Team_Name = Team.y, GP:Points, point_prediction, CF:PDO)

#### prediction summary: ----

preds_final <- mod_e_res %>%
  select(h_season:h_home_win, pred_win, prob)

names(preds_final) <- str_replace(names(preds_final), "h_", "")




