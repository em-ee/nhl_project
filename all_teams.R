library(tidyverse)
library(caret)
library(magrittr)
library(lubridate)
library(randomForest)

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
  select(-Attendance)

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


############## NST MODEL CREATION - LOGISTIC ##################
  
model_data <- nst_avg %>%
  select(-c(awayGoals, homeGoals, TOI, gameDate, Team, season))



mod1 <- glm(home_win ~ ., data = model_data, family = "binomial")


mod2 <- predict(mod1, newdata = model_data, type = "response")

logregm1 <- nst_avg %>% 
  mutate(prob = mod2)%>%
  mutate(pred_win = ifelse(prob > 0.5365854, 1, 0))%>%
  select(season:Team, home_win:pred_win, everything())

table(actual = logregm1$home_win, pred = logregm1$pred_win)



mod3 <- glm(home_win ~ homeTeam+awayTeam+CF+CA+FF+FA+SF+SA+GF+GA+xGF+xGA+SVperc+SHperc+PDO, data = model_data, family = "binomial")


mod4 <- predict(mod3, newdata = model_data, type = "response")

# backwards <- step(mod1) # to do feature selection

logregm3 <- nst_avg %>% 
  mutate(prob = mod4)%>%
  mutate(pred_win = ifelse(prob > 0.5365854, 1, 0))%>%
  select(season:Team, home_win:pred_win, everything())

table(actual = logregm3$home_win, pred = logregm3$pred_win)

############## NST MODEL CREATION - TREE ##################

model_data_tree <- nst_avg %>%
  select(-c(awayGoals, homeGoals, TOI, gameDate, Team, season))%>%
  mutate(awayTeam = as.factor(awayTeam), 
         homeTeam = as.factor(homeTeam), 
         home_win = as.factor(home_win))

  
treefit <- randomForest(home_win ~ ., data = model_data_tree, ntree = 500,
                        importance = TRUE)


