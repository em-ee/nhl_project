library(tidyverse)
library(stringr)
library(caret)
library(magrittr)
library(lubridate)
library(randomForest)
library(boot)


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

nst2014 %<>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
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

nst2015 %<>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
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

nst2016 %<>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
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

nst2017 %<>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
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

nst2018 %<>% mutate(GF. = as.double(ifelse(GF. == "-", 0, GF.)),
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

nst <- nst2014 %>%
  bind_rows(nst2015)%>%
  bind_rows(nst2016)%>%
  bind_rows(nst2017)%>%
  bind_rows(nst2018)

colnames(nst) <- str_replace(names(nst), "\\.", "perc")

nst_avg <- nst %>%
  split(.$Team)%>%
  map(~ mutate_if(., is.numeric, cummean))%>%
  bind_rows()%>%
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

season <- nst_avg%>%
  select(season)%>%
  unique()

nst_avg%>%
  summarise(home_wins = mean(home_win==1))

team_names <- nst_avg%>%
  select(Team)%>%
  unique()%>%
  arrange(Team)%>%
  filter(Team != "ATL")

nst_avg_home <- nst_avg %>%
  filter(homeTeam==Team)
  
nst_avg_away <- nst_avg %>%
  filter(awayTeam==Team)

colnames(nst_avg_home)<- paste0("h_", names(nst_avg_home))

colnames(nst_avg_away)<- paste0("a_", names(nst_avg_away))

nst_all <- nst_avg_home %>%
  left_join(nst_avg_away, by = c("h_gameDate" = "a_gameDate", 
                                 "h_homeTeam" = "a_homeTeam",
                                 "h_awayTeam" = "a_awayTeam"))%>%
  select(-c(a_season:a_homeGoals, a_home_win, h_Team, a_Team))

############## NST MODEL CREATION - LOGISTIC ##################

model_train <- nst_all %>%
  mutate_at(scale, .vars = vars(-c(h_season:h_homeGoals, h_home_win)))%>%
  mutate_if(is.character, as.factor)%>%
  filter(h_season != 2018)

model_test <- nst_all %>%
  mutate_at(scale, .vars = vars(-c(h_season:h_homeGoals, h_home_win)))%>%
  mutate_if(is.character, as.factor)%>%
  filter(h_season == 2018)

LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}


##### all variables: ----

mod1 <- glm(h_home_win ~ .-h_awayGoals -h_homeGoals -h_TOI -a_TOI 
            -h_gameDate -h_season, data = model_train, family = "binomial")


mod2 <- predict(mod1, newdata = model_test, type = "response")

# warning - multicollinearity (because % and raw scores for same metric)

logregm1 <- model_test %>% 
  mutate(prob = mod2)%>%
  mutate(pred_win = ifelse(prob > 0.5457317, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win, prob, pred_win, everything())

table(actual = logregm1$h_home_win, pred = logregm1$pred_win)

# (541+157)/(541+157+432+141)
# [1] 0.5586153

LogLossBinary(logregm1$h_home_win, logregm1$prob)


##### selected variables: -----

mod3 <- glm(h_home_win~h_awayTeam+h_CF+h_CA+h_FF+h_FA+h_SF+h_SA+h_GF+h_GA+h_xGF+h_xGA+h_SVperc+h_SHperc
            +h_PDO+a_CF+a_CA+a_FF+a_FA+a_SF+a_SA+a_GF+a_GA+a_xGF+a_xGA+a_SVperc
            +a_SHperc+a_PDO,data = model_train, family = "binomial")


mod4 <- predict(mod3, newdata = model_test, type = "response")


logregm3 <- model_test %>% 
  mutate(prob = mod4)%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = logregm3$h_home_win, pred = logregm3$pred_win)

# (480+536)/(480+536+146+109)
# [1] 0.559402

LogLossBinary(logregm3$h_home_win, logregm3$prob)

##### percentage metrics only --------


mod5 <- glm(h_home_win ~ h_awayTeam+h_CFperc+h_FFperc+h_SFperc+h_GFperc+
            h_xGFperc+h_SCFperc+h_HDCFperc+h_HDSFperc+h_HDGFperc+h_HDSHperc+
            h_HDSVperc+h_MDCFperc+h_MDSFperc+h_MDGFperc+h_MDSHperc+h_MDSVperc+
            h_LDCFperc+h_LDSFperc+h_LDGFperc+h_LDSHperc+h_LDSVperc+h_PDO+
            a_CFperc+a_FFperc+a_SFperc+a_GFperc+a_xGFperc+a_SCFperc+a_HDCFperc+
            a_HDSFperc+a_HDGFperc+a_HDSHperc+a_HDSVperc+a_MDCFperc+a_MDSFperc+
            a_MDGFperc+a_MDSHperc+a_MDSVperc+a_LDCFperc+a_LDSFperc+a_LDGFperc+
            a_LDSHperc+a_LDSVperc+a_PDO, data = df_2, family = "binomial")

mod6 <- predict(mod5, newdata = df_2_test, type = "response")


logregm5 <- model_test %>% 
  mutate(prob = mod6)%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = logregm5$h_home_win, pred = logregm5$pred_win)

# (221+486)/(221+486+196+368)
# [1] 0.5562549

LogLossBinary(logregm5$h_home_win, logregm5$prob)

##### taking most significant variables from mod5: --------

mod7 <- glm(h_home_win ~ h_homeTeam+h_awayTeam+h_CFperc+h_FFperc+h_GFperc+
              h_HDGFperc+h_LDCFperc+h_PDO+a_GFperc, 
            data = model_train, family = "binomial")

mod8 <- predict(mod7, newdata = model_test, type = "response")


logregm7 <- model_test %>% 
  mutate(prob = mod8)%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = logregm7$h_home_win, pred = logregm7$pred_win)

# (216+498)/(216+498+184+373)
# [1] 0.5617624

LogLossBinary(logregm7$h_home_win, logregm7$prob)

##### taking most significant variables from mod3: --------

mod9 <- glm(h_home_win ~ h_homeTeam+h_awayTeam+h_FA+h_GF+h_SVperc+h_SHperc+h_PDO+
              a_SF+a_GF+a_xGA+a_SVperc+a_SHperc+a_PDO,
            data = model_train, family = "binomial")

mod10 <- predict(mod9, newdata = model_test, type = "response")


logregm9 <- model_test %>% 
  mutate(prob = mod10)%>%
  mutate(pred_win = ifelse(prob > 0.5, 1, 0))%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = logregm9$h_home_win, pred = logregm9$pred_win)

# (234+474)/(234+474+355+208)
# [1] 0.5570417

LogLossBinary(logregm9$h_home_win, logregm9$prob)

points_pred <- logregm7 %>%
  select(h_season:h_home_win, prob, pred_win)%>%
  mutate(h_actual_points = ifelse(h_home_win == 1, 2, 0),
         a_actual_points = ifelse(h_actual_points == 2, 0, 2),
         h_pred_points = ifelse(pred_win == 1, 2, 0), 
         a_pred_points = ifelse(h_pred_points == 2, 0, 2))

h_points_pred <- points_pred %>%
  select(h_season, h_gameDate, Team = h_homeTeam, 
         actual_points = h_actual_points, pred_points = h_pred_points)


a_points_pred <- points_pred %>%
  select(h_season, h_gameDate, Team = h_awayTeam, 
         actual_points = a_actual_points, pred_points = a_pred_points)

points_pred <- h_points_pred %>%
  bind_rows(a_points_pred)%>%
  mutate(h_season = as.factor(h_season))

points_preds <- points_pred%>%
  group_by(h_season, Team)%>%
  summarise(total_pred_points = sum(pred_points), 
            total_actual = sum(actual_points))

### bootstrapped points:

boot_points <- function(df, sample_size) {
  resamples <- lapply(1:sample_size, function(i) sample(df, 82, replace=TRUE))
  points_sum <- sapply(resamples, sum)
  list(sums=points_sum)   
}

points_boot <- points_pred %>%
  select(-c(h_season, h_gameDate, actual_points))%>%
  split(.$Team)%>%
  map(~ boot_points(.[,-1], 50000))%>%
  map_df(., ~as.data.frame(.), .id="id")



############## NST MODEL CREATION - TREE ##################


treefit1 <- randomForest(as.factor(h_home_win) ~ .-h_awayGoals -h_homeGoals -h_TOI -a_TOI 
                        -h_gameDate -h_season, data = model_train, ntree = 100,
                        importance = TRUE)

summary(treefit1)
varImpPlot(treefit1)

tf1 <- predict(treefit1, newdata = model_test, type = "response")


tf2 <- model_test %>% 
  mutate(pred_win = tf1)%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf2$h_home_win, pred = tf2$pred_win)

##### just using most important variables from treefit1: --------



treefit2 <- randomForest(as.factor(h_home_win) ~ h_awayTeam+h_homeTeam+h_GFperc+
                           a_GFperc+h_PDO+a_PDO+h_MDSFperc, 
                         data = model_train, ntree = 500,
                         importance = TRUE)

summary(treefit2)
varImpPlot(treefit2)

tf3 <- predict(treefit2, newdata = model_test, type = "response")


tf4 <- model_test %>% 
  mutate(pred_win = tf3)%>%
  select(h_season:h_homeGoals, h_home_win:pred_win, everything())

table(actual = tf4$h_home_win, pred = tf4$pred_win)





