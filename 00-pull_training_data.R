library(baseballr)
library(tidyverse)
 

  
pullPitcherPBP <- function(year) {
  statcast_all <- data.frame()
  dates <- seq.Date(as.Date(glue::glue("{year}-03-20")), as.Date(glue::glue("{year}-11-03")), by = "1 day")
  for (i in 1:length(dates)) {
    print(dates[i])
    temp <- scrape_statcast_savant_pitcher_all(start_date = dates[i], end_date = dates[i])
    if (nrow(temp) > 0) {
      statcast_all <- rbind(statcast_all, temp)  
    }
    
  }
  
  
  statcast_all <- unique(statcast_all)
  write.csv(statcast_all, glue::glue("statcast_{year}_pitchers.csv"))
  saveRDS(statcast_all, glue::glue("statcast_{year}_pitchers.rds"))
  return(statcast_all)
}  


df_current <- data.frame()
dates <- seq.Date(as.Date(glue::glue("2022-03-20")), as.Date(glue::glue("2022-11-03")), by = "1 day")
for (i in 1:length(dates)) {
  print(dates[i])
  temp <- scrape_statcast_savant_pitcher_all(start_date = dates[i], end_date = dates[i])
  if (nrow(temp) > 0) {
    df_current <- rbind(df_current, temp)  
  }
  
}


df_current <- pullPitcherPBP(2022)
df_current <- readRDS("statcast_2022_pitchers.rds")



