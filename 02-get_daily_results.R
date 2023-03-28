library(gt)
library(baseballr)
library(tidyverse)
library(lubridate)
library(slider)
library(rtweet)
library(randomForest)
library(piggyback)
# library(googledrive)
try(source("0x-helper_functions.R"), silent = F)

api_key <- Sys.getenv("TWITTERAPIKEY")
api_secret <- Sys.getenv("TWITTERAPISECRET")
access_token <- Sys.getenv("TWITTERACCESSTOKEN")
access_secret <- Sys.getenv("TWITTERACCESSTOKENSECRET")


auth <- rtweet::rtweet_bot(api_key = api_key,
                           api_secret = api_secret, 
                           access_token = access_token, 
                           access_secret = access_secret)

rtweet::auth_as(auth)

new_games <- scrape_statcast_savant_pitcher_all(start_date = Sys.Date()-1, end_date = Sys.Date()-1)

repo <- "dtreisman/ManagerScorecards"
data_tag <- "Data"
models_tag <- "Models"

# pb_release_create(repo = repo, tag = models_tag)
# pb_release_create(repo = repo, tag = data_tag)

if (nrow(new_games > 0)) {
  
  # Get pre-scraped active rosters with positions
  print("Getting ESPN Rosters...")
  rosters <- read_csv(url("https://github.com/dtreisman/DailyMLBRosters/raw/main/data/DailyMLBRosters.csv"))
  pitchers <- rosters %>%
    filter(pos %in% c("SP", "RP")) %>%
    mutate(team_abbr = str_to_upper(team_abbr)) %>%
    select(name, pos, jersey, bats, throws, team_abbr, date, player_id) %>%
    mutate(date = lubridate::as_date(date),
           jersey = as.numeric(jersey))
  print(max(pitchers$date))
  
  # get MLB.com active rosters
  al_teams <- tibble(baseballr::mlb_teams(season = 2023, league_ids = c(103)))
  nl_teams <- tibble(baseballr::mlb_teams(season = 2023, league_ids = c(104)))
  al_teams[which(al_teams$team_abbreviation == "CWS"), "team_abbreviation"] <- "CHW"
  
  print("Getting MLB rosters...")
  mlb_rosters <- tibble()
  for (day in seq.Date(Sys.Date()-1, Sys.Date()-1, by = "1 day")) { # seq.Date(Sys.Date()-9, Sys.Date()-9, by = "1 day")) {
    print(as_date(day))
    for (i in 1:length(c(al_teams$team_abbreviation, nl_teams$team_abbreviation))) {
      abbrs <- c(al_teams$team_abbreviation, nl_teams$team_abbreviation)
      ids <- c(al_teams$team_id, nl_teams$team_id)
      #print(abbrs[i])
      temp <- mlb_rosters(team_id = c(ids[i]), season = 2023, roster_type = 'active', date = as_date(day))
      
      temp <- temp %>%
        select(person_id, person_full_name, jersey_number) %>%
        mutate(jersey_number = as.numeric(jersey_number))
      temp$date <- as_date(day)
      temp$team <- abbrs[i]
      mlb_rosters <- bind_rows(mlb_rosters, temp)
    }
    
  }
  
  print(mlb_rosters %>% head(1))
  print(nrow(mlb_rosters))
  print("Rosters complete.")
  
  pb_download(file = "MLBRosters.Rds", repo = repo, overwrite = T, tag = data_tag)
  all_mlb_rosters <- readRDS("MLBRosters.Rds")
  all_mlb_rosters <- bind_rows(all_mlb_rosters, mlb_rosters)
  saveRDS(object = all_mlb_rosters, file = "MLBRosters.Rds")

  
  
  
  print("Getting old raw games...")
  pb_download(file = "df_current.Rds", repo = repo, overwrite = T, tag = data_tag)
  df_current <- readRDS(file = "df_current.Rds")
  
  print("Games until this yesterday:")
  print(nrow(df_current))
  
  df_current <- bind_rows(df_current, new_games)
  saveRDS(object = df_current, file = "df_current.Rds")
  

   
  
  new_games <- new_games %>%   #scrape_statcast_savant_pitcher_all(start_date = Sys.Date()-9, end_date = Sys.Date()-9) %>%
    filter(events != "", !is.na(events)) %>%
    mutate(inning = ifelse(inning > 9, 10, inning),
           inning = as.factor(inning),
           game_date_string = str_remove_all(game_date, "-"),
           game_id = paste0(game_date_string, "_", home_team, "_", away_team))
  
  print("New Games:")
  print(nrow(new_games))
  print(unique(new_games$game_id))
  
  # new_games <- pullPitcherPBP(2023)
  # new_games <- new_games %>%
  #   filter(events != "", !is.na(events)) %>%
  #   mutate(inning = ifelse(inning > 9, 10, inning),
  #          inning = as.factor(inning),
  #          game_date_string = str_remove_all(game_date, "-"),
  #          game_id = paste0(game_date_string, "_", home_team, "_", away_team))
  
  
  
  
  
  # old_games <- df_mod %>%
  #   filter(game_year == year(Sys.Date()) - 2) %>%
  #   bind_rows(statcast_22 %>%
  #               mutate(inning = ifelse(inning > 9, 10, inning),
  #                      inning = as.factor(inning))) %>%
  #   prepareNewData()
  
  
  
  #
  # df_new <- df %>%
  #   bind_rows(df_current) %>%
  #   bind_rows(new_games)%>%
  #   prepareNewData()%>%
  #   mutate(inning = ifelse(inning > 9, 10, inning),
  #          inning = as.factor(inning))
  
  print("Begin data prep.")
  
  pb_download(repo = repo, tag = data_tag, file = "prev_2_years.Rds")
  df <- readRDS("prev_2_years.Rds")
  
  df_new <- df %>%
    bind_rows(df_current) %>%
    prepareNewData() %>%
    mutate(inning = ifelse(inning > 9, 10, inning),
           inning = as.factor(inning))
  
  print("Full Prepared Dataset Rows:")
  print(nrow(df_new))
  print(max(df_new$game_date))
  
  
  print("Augment new data")
  df_new <- augmentNewData(df_new) %>%
    padr::fill_by_value(pitcher_woba, mean(old_games$pitcher_woba)) %>%
    ungroup() %>%
    mutate(woba_diff = pitcher_woba - batter_woba,
           inning = as.factor(inning),
           state = factor(state))
  
  print("Full Augmented Dataset Rows:")
  print(nrow(df_new))
  print(max(df_new$game_date))
  
  df_pred <- df_new %>%
    filter(game_date == new_games$game_date[1]) %>%
    mutate(player_id = as.character(player_id)) %>%
    unique()
  
    
  
  pb_download(file = "old_games.Rds", repo = repo, overwrite = T, tag = data_tag)
  old_games <- readRDS(file = "old_games.Rds")
  old_games <- bind_rows(old_games, df_pred)
  saveRDS(object = old_games, file = "old_games.Rds")
  
  
  pb_download(file = "win_probability_model.Rds", repo = repo, 
              overwrite = T, tag = models_tag)
  fit_wp <- readRDS("win_probability_model.Rds")
  # pb_upload(file = "win_probability_model.Rds", repo = repo, tag = models_tag, overwrite = T)
  
  pb_download(file = "expected_runs_model.Rds", repo = repo,
              overwrite = T, tag = models_tag)
  fit_runs <- readRDS("expected_runs_model.Rds")
  # pb_upload(file = "expected_runs_model.Rds", repo = repo, tag = models_tag, overwrite = T)
  
  pb_download(file = "expected_pitching_change_model.Rds", repo = repo,
              overwrite = T, tag = models_tag)
  fit_new_pitcher <- readRDS("expected_pitching_change_model.Rds")
  # pb_upload(file = "expected_pitching_change_model.Rds", repo = repo,  tag = models_tag, overwrite = T)
  
  print('Prediction Dataset:')
  print(colSums(is.na(df_pred)))
  print(str(df_pred))
  print(nrow(df_pred))
        
  pred_wp <- predict(fit_wp, df_pred, type = "response")
  pred_runs <- predict(fit_runs, df_pred)
  pred_new_pitcher <- predict(fit_new_pitcher, df_pred, type = "response")
  
  
  # pred_wp_hist <- predict(fit_wp, df_new, type = "response")
  # pred_runs_hist  <- predict(fit_runs, df_new)
  # pred_np_hist  <- predict(fit_new_pitcher, df_new, type = "response")
  
  
  # df_output_history <- df_new %>%
  # bind_cols(pred_wp_hist) %>%
  # bind_cols(pred_runs_hist) %>%
  # bind_cols(pred_np_hist) %>%
  # rename(pred_wp = ...46,
  #        pred_runs = ...47,
  #        pred_new_pitcher = ...48) %>%
  # # select(game_id, game_date, name, jersey, pitching_team, inning, inning_topbot,
  # #        pred_wp, pred_runs, is_pitching, state, fld_score, bat_score, n_batters) %>%
  # mutate(situation_id = paste0(game_id, "_", inning_topbot, "_", inning, "_",
  #                              state, "_", fld_score, "_", bat_score)) %>%
  # group_by(situation_id) %>%
  # mutate(pred_wp = ifelse(pred_wp < 0, 0, pred_wp),
  #        pred_runs = ifelse(pred_runs < 0, 0, pred_runs),
  #        max_wp = max(pred_wp),
  #        current_wp = ifelse(is_pitching == 1, pred_wp, NA),
  #        min_runs = min(pred_runs),
  #        current_runs = ifelse(is_pitching == 1, pred_runs, NA)) %>%
  # fill(current_wp, .direction = "downup") %>%
  # fill(current_runs, .direction = "downup") %>%
  # mutate(wp_diff = current_wp - max_wp,
  #        runs_diff = min_runs - current_runs,
  #        runs_diff_adj =  ifelse(new_pitcher == 1,
  #                                runs_diff * (1 - pred_new_pitcher),
  #                                runs_diff * -pred_new_pitcher)) %>%
  # filter(is_pitching == 1, n_batters > 3)
  
  df_output <- df_pred %>%
    bind_cols(pred_wp) %>%
    bind_cols(pred_runs) %>%
    bind_cols(pred_new_pitcher) %>%
    rename(pred_wp = ...46,
           pred_runs = ...47,
           pred_new_pitcher = ...48) %>%
    # select(game_id, game_date, name, jersey, pitching_team, inning, inning_topbot,
    #        pred_wp, pred_runs, is_pitching, state, fld_score, bat_score, n_batters) %>%
    mutate(situation_id = paste0(game_id, "_", inning_topbot, "_", inning, "_",
                                 state, "_", fld_score, "_", bat_score)) %>%
    group_by(situation_id) %>%
    mutate(pred_wp = ifelse(pred_wp < 0, 0, pred_wp),
           pred_runs = ifelse(pred_runs < 0, 0, pred_runs),
           max_wp = max(pred_wp),
           current_wp = ifelse(is_pitching == 1, pred_wp, NA),
           min_runs = min(pred_runs),
           current_runs = ifelse(is_pitching == 1, pred_runs, NA)) %>%
    fill(current_wp, .direction = "downup") %>%
    fill(current_runs, .direction = "downup") %>%
    mutate(wp_diff = current_wp - max_wp,
           runs_diff = min_runs - current_runs,
           runs_diff_adj =  ifelse(new_pitcher == 1,
                                   runs_diff * (1 - pred_new_pitcher),
                                   runs_diff * -pred_new_pitcher)) %>%
    filter(is_pitching == 1)
  
  
  pb_download(file = "output_history.Rds", repo = repo, overwrite = T, tag = data_tag)
  df_output_history <- readRDS("output_history.Rds")
  df_output_history <- bind_rows(df_output_history, df_output)
  saveRDS(object = df_output_history, file = "output_history.Rds")
 
  
  
  
  # by_team <- df_output %>%
  #   # filter(df_output$game_id %in% )
  #   filter(is_pitching == 1, n_batters > 3) %>%
  #   mutate(wp_diff = round(wp_diff, 3),
  #          runs_diff_adj = round(runs_diff_adj, 3)) %>%
  #   group_by(pitching_team) %>%
  #   summarise(wp_diff = sum(wp_diff, na.rm = T),
  #             runs_diff_adj = sum(runs_diff_adj, na.rm = T))
  
  
  dist_history <- df_output_history %>%
    group_by(game_id, pitching_team) %>%
    summarise(runs_diff_adj = sum(runs_diff_adj))
  
  
  yesterday_games <- mlb_schedule(2023) %>%
    filter(date == Sys.Date()-1) %>%
    select(game_pk) %>%
    deframe()
  
  
  # mlb_pbp(662955) %>%
  #   select(contains("OnFirst")) %>%
  #   names()
  
  for (id in yesterday_games) {
    
    print(id)
    tweet <- write_tweet(id, df_output, dist_history)
    print(tweet)
    print(length(tweet))
    rtweet::post_tweet(tweet)

    Sys.sleep(31)
  }
  
  pb_upload(file = "MLBRosters.Rds", repo = repo, tag = data_tag, overwrite = T)
  pb_upload(file = "df_current.Rds", repo = repo, tag = data_tag, overwrite = T)
  pb_upload(file = "old_games.Rds", repo = repo, tag = data_tag, overwrite = T)
  pb_upload(file = "output_history.Rds", repo = repo, tag = data_tag, overwrite = T)
} else {
  rtweet::post_tweet(status = glue::glue("No games yesterday {Sys.Date()-1}."))
  print('no games')
}





