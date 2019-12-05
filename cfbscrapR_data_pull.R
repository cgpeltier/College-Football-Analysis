library(tidyverse)
library(cfbscrapR)


week1 <- cfb_pbp_data(year = 2019, week=1, epa_wpa = TRUE)
week2 <- cfb_pbp_data(year = 2019, week=2, epa_wpa = TRUE)
week3 <- cfb_pbp_data(year = 2019, week=3, epa_wpa = TRUE)
week4 <- cfb_pbp_data(year = 2019, week=4, epa_wpa = TRUE)
week5 <- cfb_pbp_data(year = 2019, week=5, epa_wpa = TRUE)
week6 <- cfb_pbp_data(year = 2019, week=6, epa_wpa = TRUE)
week7 <- cfb_pbp_data(year = 2019, week=7, epa_wpa = TRUE)
week8 <- cfb_pbp_data(year = 2019, week=8, epa_wpa = TRUE)
week9 <- cfb_pbp_data(year = 2019, week=9, epa_wpa = TRUE)
week10 <- cfb_pbp_data(year = 2019, week=10, epa_wpa = TRUE)
week11 <- cfb_pbp_data(year = 2019, week=11, epa_wpa = TRUE)
week12 <- cfb_pbp_data(year = 2019, week=12, epa_wpa = TRUE)
week13 <- cfb_pbp_data(year = 2019, week=13, epa_wpa = TRUE)
week14  <- cfb_pbp_data(year = 2019, week=14, epa_wpa = TRUE)
cfb_regular_play_2019 <- rbind(week1, week2, week3, week4, week5, week6, week7, week8,week9, week10,week11,week12, week13, week14)

cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    rename(adjusted_yardline = adj_yd_line,
           offense = offense_play,
           defense = defense_play)



## automate the above
season_df <- data.frame()
for(i in 1:14){
  model <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE)
  df <- data.frame(model)
  season_df <- bind_rows(season_df, df)
}
