library(tidyverse)
library(ggplot2)

work_dir <- "C:/Users/eqa47693/Desktop/CFB/cfb_2018_epa"
setwd(work_dir)

cfb_regular_play_2018 <- read_csv("cfb_regular_play_18.csv")
cfb_regular_play_2019 <- read_csv("cfb_regular_play_19.csv")


## new play type and successful play variables
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
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
         rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown" | (play_type == "Safety" & str_detect(play_text, "run")) |
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
cfb_regular_play_2019$passing_player_name <- as.character(cfb_regular_play_2019$passing_player_name)
cfb_regular_play_2019$sacked_player_name <- as.character(cfb_regular_play_2019$sacked_player_name)

cfb_regular_play_2019$passing_player_name <- ifelse(cfb_regular_play_2019$play_type == "Sack", 
                                                    cfb_regular_play_2019$sacked_player_name, cfb_regular_play_2019$passing_player_name)



## fix passer names
passer_name <- ifelse(cfb_regular_play_2019$pass == 1, 
                      (str_split(cfb_regular_play_2019$play_text, 
                                 "(pass) | (sacked)", simplify = TRUE)), 0)
passer_name <- as.data.frame(passer_name)
passer_name$passer_name <- str_replace(passer_name$passer_name, regex("(.*Yd*)| (.*Yds*) "), "")
passer_name2 <- ifelse(cfb_regular_play_2019$play_type == "Passing Touchdown", 
                       (str_split(cfb_regular_play_2019$play_text,
                                  "[(]", simplify = TRUE)), 0)
passer_name2 <- (str_split(passer_name2,"from ", simplify = TRUE))
passer_name2 <- passer_name2[,2]
passer_name2 <- as.data.frame(passer_name2)
passer_name <- passer_name %>%
  mutate(col2 = passer_name2$passer_name2)
passer_name <- unite(passer_name, passer_name, sep = "")
passer_name <- str_trim(passer_name$passer_name)

cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    mutate(passer_name = passer_name)


## fix fumble problem with qbs 
cfb_regular_play_2019$passer_name <- ifelse(cfb_regular_play_2019$play_type == "Fumble Recovery (Opponent)" & cfb_regular_play_2019$pass==1,
                                            is.na(cfb_regular_play_2019$passer_name), cfb_regular_play_2019$passer_name)



## box score stats
box_score_stats<- cfb_regular_play_2019 %>%
  group_by(offense, defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1]),
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
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    run_rate = sum(rush==1) / plays,
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(drive_id[so_play == 1]),
    touchdown_total = n_distinct(drive_id[play_type == "(Passing Touchdown) | (Rushing Touchdown)"]), 
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    rz_td_rate = touchdown_total / n_distinct(drive_id[rz_play == 1]),
    ) %>% ungroup()


## new all season stats - offense
season_stats_offense <- cfb_regular_play_2019 %>%
  group_by(offense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1]),
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
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    run_rate = sum(rush==1) / plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(drive_id[so_play == 1]),
    touchdown_total = n_distinct(drive_id[play_type == "(Passing Touchdown) | (Rushing Touchdown)"]), 
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    rz_td_rate = touchdown_total / n_distinct(drive_id[rz_play == 1]),
  ) %>% ungroup()


season_stats_defense <- cfb_regular_play_2019 %>%
  group_by(defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1]),
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
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    run_rate = sum(rush==1) / plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(drive_id[so_play == 1]),
    touchdown_total = n_distinct(drive_id[play_type == "(Passing Touchdown) | (Rushing Touchdown)"]), 
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    rz_td_rate = touchdown_total / n_distinct(drive_id[rz_play == 1]),
  ) %>% ungroup()

national_season_stats <- cfb_regular_play_2019 %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    epa_sr_rate_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    epa_sr_rate_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    std_down_epa = mean(EPA[std.down==1]),
    pass_down_epa = mean(EPA[pass.down==1]),
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
    short_rush_sr = ((sum(short_rush_success)) / (sum(short_rush_attempt))),
    run_rate = sum(rush==1) / plays,
    std.down.rush.rte = sum(rush[std.down==1]) / sum(std.down),
    rz_sr = mean(success[rz_play == 1]),
    so_sr = mean(success[so_play == 1]),
    so_total = n_distinct(drive_id[so_play == 1]),
    touchdown_total = n_distinct(drive_id[play_type == "(Passing Touchdown) | (Rushing Touchdown)"]), 
    so_rate = so_total / drives, 
    so_td_rate = touchdown_total / so_total,
    rz_td_rate = touchdown_total / n_distinct(drive_id[rz_play == 1]),
  ) %>% ungroup()


## skill player stats
rusher_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 25)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiving_player_name != 0 & receiving_player_name != "TEAM" & pass == 1 & sum(pass) >=5) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE),
    plays = sum(pass)
  )%>% ungroup()

passer_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, passer_name) %>%
  filter(passer_name != 0 & passer_name != "TEAM" & pass == 1 & sum(pass) > 30) %>%
  summarize(
    passes = sum(pass),
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()


######################

## chart comparing qbs
ggplot(data = passer_stats_19, aes(x = epa_sr, y = avg_epa)) +
    geom_point() +
    geom_smooth(method=lm) +
    geom_text(x=0.58, y=0.54, label = "Justin Fields") +
    geom_text(x=0.44, y=0.14, label = "Adrian Martinez") +
    geom_point(data=passer_stats_19[102, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2) +
    geom_point(data=passer_stats_19[87, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2)

## chart comparing rbs
ggplot(data = passer_stats_19, aes(x = epa_sr, y = avg_epa)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_text(x=0.5, y=0.22, label = "J.K. Dobbins") +
  geom_text(x=0.48, y=0.13, label = "Master Teague III") +
  geom_text(x=0.47, y=-.02, label = "Adrian Martinez") +
  geom_text(x=0.34, y=-.15, label = "Dedrick Mills") +
  geom_text(x=0.46, y=0.26, label = "Maurice Washington") +
  geom_text(x=0.44, y=-.13, label = "Wan'Dale Robinson") +
  geom_point(data=rusher_stats_19[138:139, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2) +
  geom_point(data=rusher_stats_19[117:120, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2)


## chart comparing wrs
ggplot(data = receiver_stats_19, aes(x = epa_sr, y = avg_epa)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_text(x=0.55, y=1.2, label = "Maurice Washington", colour = "red") +
  geom_text(x=0.58, y=.35, label = "Wan'Dale Robinson", colour = "red") +
  geom_text(x=0.64, y=0.83, label = "JD Spielman", colour = "red") +
  geom_point(data=receiver_stats_19[507:512, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2)


write.csv(national_season_stats, file = "national_season_stats.csv")
write.csv(box_score_stats, file = "box_score_stats.csv")
