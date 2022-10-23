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
           runs_scored = post_bat_score - bat_score,
           one = 1) %>%
    group_by(pitcher) %>%
    mutate(pitcher_woba = slider::slide_mean(woba_value,
                                             before = 100, 
                                             complete = F, 
                                             na_rm = T),
           pitcher_n_app = unlist(slider::slide_index(.x = one,
                                               .i = game_date,
                                               .f = ~sum(., na.rm = T),
                                             .before = 100, 
                                             .complete = F))) %>%
    group_by(batter) %>%
    mutate(batter_woba = slider::slide_mean(woba_value,
                                            before = 100, 
                                            complete = F, 
                                            na_rm = T),
           batter_n_app = unlist(slider::slide_index(.x = one, 
                                            .i = game_date,
                                            .f = ~sum(., na.rm = T), 
                                            .before = 100, 
                                            .complete = F))) %>%
    group_by(pitcher, game_pk) %>%
    #arrange(game_date, inning, desc(inning_topbot)) %>%
    padr::fill_by_value(woba_value, 0) %>%
    mutate(one = 1,
           n_batters = cumsum(one),
           pitcher_game_woba = cummean(woba_value)*n_batters) %>%
    ungroup() %>%
    mutate(avg_woba = slider::slide_mean(x = woba_value, 
                                         before = 5000, 
                                         complete = F,
                                         na_rm = T),
           pitcher_woba = ifelse(pitcher_n_app <=15, avg_woba, pitcher_woba),
           batter_woba = ifelse(batter_n_app <=15, avg_woba, batter_woba)) %>%
    group_by(game_id, pitching_team) %>%
    mutate(new_pitcher = ifelse(pitcher != lag(pitcher), 1, 0))%>%
    ungroup() %>%
    padr::fill_by_value(new_pitcher, value = 0)
  
  return(df)
}

prepareNewData <- function(df) {
  df <- df %>%
    select(game_date, game_year, game_pk, player_name, pitcher, batter, events, description, 
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
           runs_scored = post_bat_score - bat_score,
           one = 1) %>%
    group_by(pitcher) %>%
    mutate(pitcher_woba = slider::slide_mean(woba_value,
                                             before = 100, 
                                             complete = F, 
                                             na_rm = T),
           pitcher_n_app = unlist(slider::slide_index(.x = one,
                                                      .i = game_date,
                                                      .f = ~sum(., na.rm = T),
                                                      .before = 100, 
                                                      .complete = F))) %>%
    group_by(batter) %>%
    mutate(batter_woba = slider::slide_mean(woba_value,
                                            before = 100, 
                                            complete = F, 
                                            na_rm = T),
           batter_n_app = unlist(slider::slide_index(.x = one, 
                                                     .i = game_date,
                                                     .f = ~sum(., na.rm = T), 
                                                     .before = 100, 
                                                     .complete = F))) %>%
    group_by(pitcher, game_pk) %>%
    #arrange(game_date, inning, desc(inning_topbot)) %>%
    padr::fill_by_value(woba_value, 0) %>%
    mutate(one = 1,
           n_batters = cumsum(one),
           pitcher_game_woba = cummean(woba_value)*n_batters) %>%
    ungroup() %>%
    mutate(avg_woba = slider::slide_mean(x = woba_value, 
                                         before = 5000, 
                                         complete = F,
                                         na_rm = T))%>%
    group_by(game_id, pitching_team) %>%
    mutate(new_pitcher = ifelse(pitcher != lag(pitcher), 1, 0)) %>%
    ungroup() %>%
    padr::fill_by_value(new_pitcher, value = 0) %>%
    padr::fill_by_value(batter_n_app, value = 0) 
  
  return(df)
}


augmentNewData <- function(df_new){
  df_new <- df_new %>%
    select(-c(events, description, home_team, away_team, home_score, away_score,
              on_1b, on_2b, on_3b, outs_when_up, one)) %>%
    left_join(pitchers, 
              by = c("pitching_team" = "team_abbr", "game_date" = "date")) %>%
    left_join(all_mlb_rosters, 
              by = c("pitching_team" = "team", "jersey" = "jersey_number", "game_date" = "date")) %>%
    mutate(is_pitching = ifelse(person_id == pitcher, 1, 0),
           pitcher_woba = ifelse(is_pitching == 0, NA, pitcher_woba)) %>%
    group_by(person_id) %>%
    #mutate(pitcher_woba = ifelse(is.na(pitcher_woba), lag(pitcher_woba), pitcher_woba)) %>%
    fill(pitcher_woba, .direction = "downup") %>%
    fill(pitcher_n_app, .direction = "down") %>%
    group_by(game_pk, person_id) %>%
    mutate(n_batters_tot = cumsum(is_pitching)) %>%
    group_by(game_id, person_id, is_pitching) %>%
    mutate(pitcher_game_woba = cummean(woba_value*n_batters_tot),
           platoon = ifelse(stand == throws, 1, 0)) %>% 
    group_by(pitcher, game_id) %>%
    #arrange(game_date, inning, desc(inning_topbot)) %>%
    ungroup() %>%
    mutate(pitcher_woba = ifelse(pitcher_n_app <= 15, avg_woba, pitcher_woba),
           batter_woba = ifelse(batter_n_app <= 15, avg_woba, batter_woba)) %>%
    filter(game_year == 2022, !is.na(is_pitching)) %>%
    mutate(available = ifelse((pos == "RP" & n_batters_tot == 0) | (is_pitching == 1), 1, 0)) %>%
    # mutate(available = ifelse((pos == "RP" & n_batters_tot == 0) | (pos == "SP" & n_batters_tot > 0), 1, 0)) %>%
    filter(available == 1)
  
  df_days_rest_new <- df_new %>%
    filter(is_pitching == 1) %>%
    select(game_pk, game_date, person_id, name, is_pitching) %>%
    group_by(person_id, name, game_date) %>%
    summarise(n_batters_tot = sum(is_pitching)) %>%
    #group_by(person_id, name, game_date) %>%
    mutate(days_rest = as.numeric(game_date - lag(game_date)),
           days_rest = ifelse(days_rest > 5, 5, days_rest),
           one = 1,
           n_games_last_5 = unlist(slide_index(.x = one, .i = game_date, .f = sum, .before = 5))-1) %>%
    ungroup() %>%
    padr::fill_by_value(days_rest, value = 5) %>%
    select(-n_batters_tot)
  
  
  df_new <- df_new %>%
    left_join(df_days_rest_new %>% 
                select(-c(name, one)), by = c("person_id", "game_date")) %>%
    fill(days_rest, .direction = "down") %>%
    fill(n_games_last_5, .direction = "down") %>%
    padr::fill_by_value(days_rest, value = 5) %>%
    padr::fill_by_value(n_games_last_5, value = 0) %>%
    padr::fill_by_value(pitcher_woba, mean(old_games$pitcher_woba)) %>%
    ungroup() %>%
    mutate(woba_diff = pitcher_woba - batter_woba)
  
  return(df_new)
}


write_tweet <- function(id, df_output, dist_history){
  
  team_info <- mlb_teams(season = 2022, sport_ids = 1) %>%
    select(team_full_name, team_abbreviation)
  
  yesterday_scores <- mlb_schedule(2022) %>% 
    filter(date == Sys.Date() - 1)  %>% 
    select(game_pk, 
           date, 
           teams_away_team_name, teams_away_score, 
           teams_home_team_name, teams_home_score) %>%
    left_join(team_info %>% rename(away_abbr = team_abbreviation), 
              by = c("teams_away_team_name" = "team_full_name"))  %>% 
    left_join(team_info %>% rename(home_abbr = team_abbreviation), 
              by = c("teams_home_team_name" = "team_full_name")) %>%
    mutate(game_date_string = str_remove_all(date, "-"),
           game_id = paste0(game_date_string, "_", home_abbr, "_", away_abbr)) %>%
    select(game_date_string, game_pk, home_abbr, teams_home_score, away_abbr, teams_away_score) 
  
  output <- df_output %>%
    separate(game_id, into = c("game_date_string", "home", "away")) %>%
    left_join(yesterday_scores %>% select(-game_pk),
              by = c("game_date_string", "home" = "home_abbr", "away" = "away_abbr")) %>%
    # filter(df_output$game_id %in% )
    filter(is_pitching == 1, n_batters > 3) %>%
    mutate(runs_diff_adj = round(runs_diff_adj, 2)) %>%
    mutate(game_id = paste0(game_date_string, "_", home, "_", away)) %>%
    # View()
    group_by(game_id, game_pk, pitching_team) %>%
    summarise(runs_diff_adj = sum(runs_diff_adj, na.rm = T)) %>%
    separate(game_id, into = c("game_date_string", "home", "away")) %>%
    #select(-wp_diff) %>%
    filter(game_pk == id) %>%
    pivot_wider(id_cols = game_date_string:game_pk, names_from = pitching_team, values_from = runs_diff_adj)%>%
    left_join(yesterday_scores %>%
                select(-game_date_string), by = c("game_pk", "home" = "home_abbr", "away" = "away_abbr")) 
  
  game_date <-  paste0(month(as_date(output$game_date_string), label = T), 
                       " ", 
                       day(as_date(output$game_date_string)),
                       ", ",
                       year(as_date(output$game_date_string)))
  
  home_team <- output$home
  away_team <- output$away
  home_score <- output$teams_home_score
  away_score <- output$teams_away_score
  
  
  if (length(names(output)) != 8 | nrow(output) == 0) {
    
    output <- yesterday_scores %>%
      filter(game_pk == id)
    
    home_team <- output$home_abbr
    away_team <- output$away_abbr
    home_score- output$teams_home_score
    away_score <- output$teams_away_score
    
    game_date <-  paste0(month(as_date(output$game_date_string), label = T), 
                         " ", 
                         day(as_date(output$game_date_string)),
                         ", ",
                         year(as_date(output$game_date_string)))
    
    
    tweet = glue::glue(
      
      "{game_date}
    {away_team} @ {home_team}
    
    Final Score: {away_score} - {home_score}
  
    Error in data source. Scorecard not available.
    
    "
    ) %>% substr(1, 278)
  } else {
    
    
    
    home_run_diff <- deframe(output[,5])
    away_run_diff <- deframe(output[,6])
    
    home_score <- output$teams_home_score
    away_score <- output$teams_away_score
    
    h_percentile <- round(mean(deframe(output[,5]) < dist_history$runs_diff_adj) * 100)
    a_percentile <- round(mean(deframe(output[,6]) < dist_history$runs_diff_adj) * 100)
    
    tweet <- glue::glue(
      
      "{game_date}
    {away_team} @ {home_team}
    
    Final Score: {away_score} - {home_score}
    
    
    {away_team} xRuns Lost: {away_run_diff} (Percentile: {a_percentile}%)
    {home_team} xRuns Lost: {home_run_diff} (Percentile: {h_percentile}%)
    
    "
    ) %>% substr(1, 278)
  }
  
  return(tweet)
}
