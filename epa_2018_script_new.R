library(tidyverse)
library(ggplot2)

work_dir <- "C:/Users/eqa47693/Desktop/CFB/cfb_2018_epa"
setwd(work_dir)

cfb_regular_play_2018 <- read_csv("cfb_regular_play_18.csv")

## new play type and successful play variables
cfb_regular_play_2018 <- cfb_regular_play_2018 %>%
  mutate(pass = if_else(play_type == "Pass Reception" | play_type == "Passing Touchdown" |
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
cfb_regular_play_2018$passing_player_name <- as.character(cfb_regular_play_2018$passing_player_name)
cfb_regular_play_2018$sacked_player_name <- as.character(cfb_regular_play_2018$sacked_player_name)

cfb_regular_play_2018$passing_player_name <- ifelse(cfb_regular_play_2018$play_type == "Sack", 
                                                    cfb_regular_play_2018$sacked_player_name, cfb_regular_play_2018$passing_player_name)






## box score stats
box_score_stats_2018 <- cfb_regular_play_2018 %>%
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
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    run_rate = sum(rush==1) / plays,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1])
  ) %>% ungroup()


## new all season stats
season_stats_2018_off <- cfb_regular_play_2018 %>%
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
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  ) %>% ungroup()


season_stats_2018_def <- cfb_regular_play_2018 %>%
  group_by(defense) %>%
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
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt)))
  ) %>% ungroup()

## fix player name stats


## skill player stats
rusher_stats_18 <- cfb_regular_play_2018 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 40)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats_2018 <- cfb_regular_play_2018 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiving_player_name != 0 & receiving_player_name != "TEAM") %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

passer_stats_2018 <- cfb_regular_play_2018 %>%
  group_by(offense, passing_player_name) %>%
  filter(passing_player_name != 0 & passing_player_name != "TEAM" & pass == 1 & sum(pass) > 50) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()


###################
## export team stats
uc_off <- season_stats_2018_off %>%
    filter(offense == "Cincinnati")

uc_def <- season_stats_2018_def %>%
  filter(defense == "Cincinnati")

uc_off <- select(uc_off, - offense)
uc_def <- select(uc_def, - defense)

uc_epa_2018<- rbind(uc_off, uc_def)
write.csv(uc_epa_2018, file = "uc_epa_2018_2.csv")
                  

########################

## make charts
ggplot(data = rusher_stats_2018, aes(x = epa_sr_rate, y = avg_epa)) +
    geom_point() +
    geom_point(data = rusher_stats_2018[75:76,], 
               aes(x = epa_sr_rate, y = avg_epa), color = "blue", size = 3) +
    geom_point(data = rusher_stats_2018[270,], 
              aes(x = epa_sr_rate, y = avg_epa), color = "firebrick1", size = 3)
  

ggplot(data = season_stats_2018_def, aes(x = epa_sr_rate, y = avg_epa)) +
  geom_point() +
  geom_point(data = season_stats_2018_def[32,], 
             aes(x = epa_sr_rate, y = avg_epa), color = "blue", size = 3) +
  geom_point(data = season_stats_2018_def[123,], 
             aes(x = epa_sr_rate, y = avg_epa), color = "firebrick1", size = 3) +
  ylim(-1, .75)


