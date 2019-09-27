library(tidyverse)
library(ggplot2)

work_dir <- "C:/Users/eqa47693/Desktop/CFB/cfb_2018_epa"
setwd(work_dir)

cfb_regular_play_2017 <- read_csv("cfb_regular_play_17.csv")


## new play type and successful play variables
cfb_regular_play_2017 <- cfb_regular_play_2017 %>%
  mutate(adj_yd_line = ifelse(offense == home_team & period == (1 | 2), yard_line, 
                              ifelse(offense == away & period == (1 | 2), (100-yard_line),
                                     ifelse(offense == home_team & period == (3 | 4), (100-yard_line),
                                            ifelse(offense == away & period ==(3 | 4), yard_line, 0)))),
         rz_play = ifelse((adj_yd_line <= 20), 1, 0), 
         so_play = ifelse((adj_yd_line <= 40 | play_type == "(Passing Touchdown) | (Rushing Touchdown"), 1, 0),
         pass = if_else(play_type == "Pass Reception" | play_type == "Passing Touchdown" |
                          play_type == "Sack" | play_type == "Pass Interception Return" |
                          play_type == "Pass Incompletion" | play_type == "Sack Touchdown" |
                          (play_type == "Safety" & str_detect(play_text, "sacked")) |
                          (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "pass")) |
                          (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "pass")), 1, 0),
         rush = ifelse(play_type == "Rush" | (play_type == "Safety" & str_detect(play_text, "run")) |
                         (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "run")) | 
                         (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "run")), 1, 0),
         stuffed_run = ifelse((rush == 1 & yards_gained <=0), 1, 0),
         opp_rate_run = ifelse((rush == 1 & yards_gained >= 4), 1, 0),
         exp_play = ifelse((yards_gained >= 13), 1, 0),
         success = ifelse(yards_gained >= .5* distance & down == 1, 1, 
                          ifelse(yards_gained >= .7* distance & down == 2, 1,
                                 ifelse(yards_gained >= distance & down == 3, 1,
                                        ifelse(yards_gained>= distance & down ==4, 1, 0)))),
         epa_success = ifelse(EPA >= 0, 1, 0),
         short_rush_attempt = ifelse(distance <= 2 & rush == 1, 1, 0),
         short_rush_success = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, 1, 0),
         std.down = ifelse(down == 2 & distance < 8, 1, 
                           ifelse(down == 3 & distance < 5, 1,
                                  ifelse(down == 4 & distance < 5, 1, 0))),
         pass.down = ifelse(down == 2 & distance > 8, 1, 
                            ifelse(down == 3 & distance > 5, 1, 
                                   ifelse(down == 4 & distance > 5, 1, 0)))
  )



## make sacked player name  = passing player name
cfb_regular_play_2017$passing_player_name <- as.character(cfb_regular_play_2017$passing_player_name)
cfb_regular_play_2017$sacked_player_name <- as.character(cfb_regular_play_2017$sacked_player_name)

cfb_regular_play_2017$passing_player_name <- ifelse(cfb_regular_play_2017$play_type == "Sack", 
                                                    cfb_regular_play_2017$sacked_player_name, cfb_regular_play_2017$passing_player_name)




## box score stats
box_score_stats<- cfb_regular_play_2017 %>%
  group_by(offense, defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yards_gained),
    plays = n(), 
    drives = n_distinct(drive_id),
    overall_sr = mean(success),
    pass_sr = mean(success[pass==1]),
    rush_sr = mean(success[rush==1]),
    ypp_rush = mean(yards_gained[rush==1]),
    ypp_pass = mean(yards_gained[pass==1]),
    stuffed_rate = mean(stuffed_run[rush==1]),
    opp_rate = mean(opp_rate_run[rush==1]),  
    overall_exp_rate = mean(exp_play),
    exp_rate_rush = mean(exp_play[rush == 1]),
    exp_rate_pass = mean(exp_play[pass == 1]),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    run_rate = sum(rush==1) / plays,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(drive_id[so_play == 1]),
    touchdown_total = n_distinct(drive_id[play_type == "(Passing Touchdown) | (Rushing Touchdown)"]), 
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    rz_td_rate = touchdown_total / n_distinct(drive_id[rz_play == 1]),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1])
  ) %>% ungroup()


## new all season stats
season_stats <- cfb_regular_play_2017 %>%
  group_by(offense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    ypp = mean(yards_gained),
    plays = n(), 
    drives = n_distinct(drive_id),
    overall_sr = mean(success),
    pass_sr = mean(success[pass==1]),
    rush_sr = mean(success[rush==1]),
    ypp_rush = mean(yards_gained[rush==1]),
    ypp_pass = mean(yards_gained[pass==1]),
    stuffed_run_rate = mean(stuffed_run[rush==1]),
    opp_rate = mean(opp_rate_run[rush==1]),  
    overall_exp_rate = mean(exp_play),
    exp_rate_rush = mean(exp_play[rush == 1]),
    exp_rate_pass = mean(exp_play[pass == 1]),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  ) %>% ungroup()

## fix player name stats


## skill player stats
rusher_stats_17 <- cfb_regular_play_2017 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 4)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats <- cfb_regular_play_2017 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiving_player_name != 0 & receiving_player_name != "TEAM") %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

passer_stats <- cfb_regular_play_2017 %>%
  group_by(offense, passing_player_name) %>%
  filter(passing_player_name != 0 & passing_player_name != "TEAM") %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()


######################

uc_ucla <- box_score_stats %>%
  filter(offense == "Cincinnati" | defense == "Cincinnati")

write.csv(uc_ucla, file = "uc_ucla2.csv")

