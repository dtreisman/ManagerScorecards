library(baseballr)
library(tidyverse)
library(lubridate)
library(slider)
library(randomForest)


df <- purrr::map(.x = c(2019, 2020, 2021), .f = pullPitcherPBP)
df <- select(game_date, game_year, player_name, pitcher, batter, events, description, 
         stand, p_throws, home_team, away_team, home_score, away_score, 
         on_1b, on_2b, on_3b, outs_when_up, inning, inning_topbot, woba_value,
         bat_score, fld_score, post_bat_score, at_bat_number) %>%
  filter(!is.na(events), events != "") 



retro_game20 <- retrosheet::get_retrosheet("game", c(2020))
retro_game21 <- retrosheet::get_retrosheet("game", c(2021))


# get winner of game
df_winner <- retro_game20 %>%
  bind_rows(retro_game21) %>%
  group_by(Date, HmTm, VisTm) %>%
  mutate(n = n()) %>%
  filter(n != 2) %>%
  ungroup() %>%
  mutate(Winner = ifelse(HmRuns > VisRuns, HmTm, VisTm)) %>%
  select(Date, HmTm, VisTm, Winner)


df_winner[which(df_winner$HmTm == "ANA"), "HmTm"] <- "LAA"
df_winner[which(df_winner$VisTm == "ANA"), "VisTm"] <- "LAA"
df_winner[which(df_winner$Winner == "ANA"), "Winner"] <- "LAA"

df_winner[which(df_winner$HmTm == "CHA"), "HmTm"] <- "CWS"
df_winner[which(df_winner$VisTm == "CHA"), "VisTm"] <- "CWS"
df_winner[which(df_winner$Winner == "CHA"), "Winner"] <- "CWS"

df_winner[which(df_winner$HmTm == "CHN"), "HmTm"] <- "CHC"
df_winner[which(df_winner$VisTm == "CHN"), "VisTm"] <- "CHC"
df_winner[which(df_winner$Winner == "CHN"), "Winner"] <- "CHC"

df_winner[which(df_winner$HmTm == "KCA"), "HmTm"] <- "KC"
df_winner[which(df_winner$VisTm == "KCA"), "VisTm"] <- "KC"
df_winner[which(df_winner$Winner == "KCA"), "Winner"] <- "KC"

df_winner[which(df_winner$HmTm == "LAN"), "HmTm"] <- "LAD"
df_winner[which(df_winner$VisTm == "LAN"), "VisTm"] <- "LAD"
df_winner[which(df_winner$Winner == "LAN"), "Winner"] <- "LAD"

df_winner[which(df_winner$HmTm == "NYA"), "HmTm"] <- "NYY"
df_winner[which(df_winner$VisTm == "NYA"), "VisTm"] <- "NYY"
df_winner[which(df_winner$Winner == "NYA"), "Winner"] <- "NYY"

df_winner[which(df_winner$HmTm == "NYN"), "HmTm"] <- "NYM"
df_winner[which(df_winner$VisTm == "NYN"), "VisTm"] <- "NYM"
df_winner[which(df_winner$Winner == "NYN"), "Winner"] <- "NYM"

df_winner[which(df_winner$HmTm == "SDN"), "HmTm"] <- "SD"
df_winner[which(df_winner$VisTm == "SDN"), "VisTm"] <- "SD"
df_winner[which(df_winner$Winner == "SDN"), "Winner"] <- "SD"

df_winner[which(df_winner$HmTm == "SFN"), "HmTm"] <- "SF"
df_winner[which(df_winner$VisTm == "SFN"), "VisTm"] <- "SF"
df_winner[which(df_winner$Winner == "SFN"), "Winner"] <- "SF"

df_winner[which(df_winner$HmTm == "SLN"), "HmTm"] <- "STL"
df_winner[which(df_winner$VisTm == "SLN"), "VisTm"] <- "STL"
df_winner[which(df_winner$Winner == "SLN"), "Winner"] <- "STL"

df_winner[which(df_winner$HmTm == "TBA"), "HmTm"] <- "TB"
df_winner[which(df_winner$VisTm == "TBA"), "VisTm"] <- "TB"
df_winner[which(df_winner$Winner == "TBA"), "Winner"] <- "TB"

df_winner[which(df_winner$HmTm == "WAS"), "HmTm"] <- "WSH"
df_winner[which(df_winner$VisTm == "WAS"), "VisTm"] <- "WSH"
df_winner[which(df_winner$Winner == "WAS"), "Winner"] <- "WSH"

df_mod <- df %>%
  inner_join(df_winner, by = c("game_date" = "Date",
                               "home_team" = "HmTm",
                               "away_team" = "VisTm"))

prepareTrainingData <- function(df) {
  df <- df %>%
    arrange(game_date, home_team, away_team, at_bat_number) %>%
    mutate(game_date_string = str_remove_all(game_date, "-"),
           game_id = paste0(game_date_string, "_", home_team, "_", away_team),
           month = month(game_date),
           pitching_team = ifelse(inning_topbot == "Top", home_team, away_team),
           win = ifelse(Winner == pitching_team, 1, 0),
           on_1b = ifelse(is.na(on_1b), 0, 1),
           on_2b = ifelse(is.na(on_2b), 0, 1),
           on_3b = ifelse(is.na(on_3b), 0, 1),
           state = paste0(outs_when_up, on_1b, on_2b, on_3b),
           score_diff = fld_score - bat_score,
           platoon = ifelse(stand == p_throws, 1, 0),
           runs_scored = post_bat_score - bat_score) %>%
    group_by(pitcher) %>%
    mutate(pitcher_woba = slider::slide_mean(woba_value,
                                             before = 100, 
                                             complete = F, 
                                             na_rm = T)) %>%
    group_by(batter) %>%
    mutate(batter_woba = slider::slide_mean(woba_value,
                                            before = 100, 
                                            complete = F, 
                                            na_rm = T)) %>%
    group_by(pitcher, game_id) %>%
    #arrange(game_date, inning, desc(inning_topbot)) %>%
    padr::fill_by_value(woba_value, 0) %>%
    mutate(one = 1,
           n_batters = cumsum(one),
           pitcher_game_woba = cummean(woba_value)*n_batters) %>%
    ungroup()
  
  return(df)
}
prepareNewData <- function(df) {
  df <- df %>%
    select(game_date, game_year, player_name, pitcher, batter, events, description, 
           stand, p_throws, home_team, away_team, home_score, away_score, 
           on_1b, on_2b, on_3b, outs_when_up, inning, inning_topbot, woba_value,
           bat_score, fld_score, post_bat_score, at_bat_number) %>%
    arrange(game_date, home_team, away_team, at_bat_number) %>%
    mutate(game_date_string = str_remove_all(game_date, "-"),
           game_id = paste0(game_date_string, "_", home_team, "_", away_team),
           month = month(game_date),
           pitching_team = ifelse(inning_topbot == "Top", home_team, away_team),
           on_1b = ifelse(is.na(on_1b), 0, 1),
           on_2b = ifelse(is.na(on_2b), 0, 1),
           on_3b = ifelse(is.na(on_3b), 0, 1),
           state = paste0(outs_when_up, on_1b, on_2b, on_3b),
           score_diff = fld_score - bat_score,
           platoon = ifelse(stand == p_throws, 1, 0),
           runs_scored = post_bat_score - bat_score) %>%
    group_by(pitcher) %>%
    mutate(pitcher_woba = slider::slide_mean(woba_value,
                                             before = 100, 
                                             complete = F, 
                                             na_rm = T)) %>%
    group_by(batter) %>%
    mutate(batter_woba = slider::slide_mean(woba_value,
                                            before = 100, 
                                            complete = F, 
                                            na_rm = T)) %>%
    group_by(pitcher, game_id) %>%
    #arrange(game_date, inning, desc(inning_topbot)) %>%
    padr::fill_by_value(woba_value, 0) %>%
    mutate(one = 1,
           n_batters = cumsum(one),
           pitcher_game_woba = cummean(woba_value)*n_batters) %>%
    ungroup()
  
  return(df)
}


df_mod <- prepareTrainingData(df_mod) %>%
  filter(game_year != 2019) %>%
  select(-c(events, description, home_team, away_team, home_score, away_score,
            on_1b, on_2b, on_3b, outs_when_up, woba_value)) %>%
  mutate(inning = ifelse(inning > 9, 10, inning),
         inning = as.factor(inning),
         woba_diff = pitcher_woba - batter_woba)

df_days_rest <- df_mod %>%
  group_by(pitcher, player_name, game_date) %>%
  summarise(n_batters_tot = sum(one)) %>%
  #group_by(person_id, name, game_date) %>%
  mutate(days_rest = as.numeric(game_date - lag(game_date)),
         days_rest = ifelse(days_rest > 5, 5, days_rest),
         one = 1,
         n_games_last_5 = unlist(slide_index(.x = one, .i = game_date, .f = sum, .before = 5))-1) %>%
  ungroup() %>%
  padr::fill_by_value(days_rest, value = 5) %>%
  select(-n_batters_tot)


df_mod <- df_mod %>%
  left_join(df_days_rest %>%
              select(-c(player_name, one)), by = c("pitcher", "game_date"))


df_mod$inning <- factor(df_mod$inning, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


train <- df_mod %>% filter(game_year == 2020) %>%
  na.omit()
test <- df_mod %>% filter(game_year == 2021) %>%
  na.omit()

fit_wp <- glm(win ~ state + inning + inning_topbot + score_diff + 
                platoon + stand + p_throws + pitcher_woba  + 
                batter_woba + pitcher_game_woba + batter_woba*pitcher_woba +
                days_rest + n_games_last_5 + n_batters, data = train, family = "binomial")
summary(fit_wp)

pred_wp <- predict(fit_wp, test, type = "response")
pred_wp_votes <- ifelse(pred_wp >= 0.5, 1, 0)
MLmetrics::LogLoss(y_pred = rep(.5, nrow(test)), y_true = test$win)
MLmetrics::LogLoss(y_pred = pred_wp, y_true = test$win)
MLmetrics::Accuracy(y_pred = pred_wp_votes, y_true = test$win)
table(pred_wp_votes, test$win)

fit_runs <- lm(runs_scored ~ state + inning + inning_topbot + score_diff + 
                 platoon + stand + p_throws + pitcher_woba  + 
                 batter_woba + pitcher_game_woba + batter_woba*pitcher_woba +
                 days_rest + n_games_last_5 + n_batters, data = train)
summary(fit_runs)

pred_runs <- predict(fit_runs, test)

MLmetrics::RMSE(y_pred = rep(mean(train$runs_scored), nrow(test)), y_true = test$runs_scored)
MLmetrics::LogLoss(y_pred = pred_runs, y_true = test$runs_scored)

MLmetrics::R2_Score(y_pred = pred_runs, y_true = test$runs_scored)


fit_wp <- glm(win ~ state + inning + inning_topbot + score_diff + 
                platoon + stand + p_throws + pitcher_woba  + 
                batter_woba + pitcher_game_woba + batter_woba*pitcher_woba +
                days_rest + n_games_last_5 + n_batters, data = df_mod, family = "binomial")

# fit_runs <- lm(runs_scored ~ state + inning + inning_topbot + score_diff + 
#                  platoon + stand + p_throws + pitcher_woba  + 
#                  batter_woba + pitcher_game_woba + batter_woba*pitcher_woba +
#                  days_rest + n_games_last_5 + n_batters, data = df_mod)
fit_runs <- randomForest(x = df_mod[, c(6, 7, 8, 9, 20, 21, 22, 24, 25, 27, 28, 29, 30, 31)],
                       y = df_mod$runs_scored, ntree = 50, do.trace = T, mtry = 10)

saveRDS(object = fit_wp, file = "models/win_probability_model.Rds")
saveRDS(object = fit_runs, file = "models/expected_runs_model.Rds")

saveRDS(object = df_mod, file = "data/training_data.Rds")
