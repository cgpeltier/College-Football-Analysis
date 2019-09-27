cfb_regular_play_2017 <- read_csv("cfb_regular_play_17.csv")

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

cfb_regular_play_2018 <- read_csv("cfb_regular_play_18.csv")

cfb_regular_play_2018 <- cfb_regular_play_2018 %>%
  mutate(pass = if_else(play_type == "Pass Reception" | play_type == "Passing Touchdown" |
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


## by game
Dobbins_17 <- cfb_regular_play_2017 %>%
  filter(rush == 1 & rushing_player_name == "J.K. Dobbins") %>%
  group_by(defense) %>%
  summarize(
    mean_epa = mean(EPA),
    epa_sr = mean(epa_success),
    stuff_rte = mean(stuffed_run),
    exp_run = mean(exp_play)
  )

Dobbins_17$defense <- paste("17", Dobbins_17$defense, sep="_")


Dobbins_18 <- cfb_regular_play_2018 %>%
  filter(rush==1 & rushing_player_name == "J.K. Dobbins") %>%
  group_by(defense) %>%
  summarize(
    mean_epa = mean(EPA),
    epa_sr = mean(epa_success),
    stuff_rte = mean(stuffed_run),
    exp_run = mean(exp_play)
  )

Dobbins_18$defense <- paste("18", Dobbins_18$defense, sep="_")
                                 

Dobbins_19 <- cfb_regular_play_2019 %>%
  filter(rush==1 & rushing_player_name == "J.K. Dobbins") %>%
  group_by(defense) %>%
  summarize(
    mean_epa = mean(EPA),
    epa_sr = mean(epa_success),
    stuff_rte = mean(stuffed_run),
    exp_run = mean(exp_play)
  )

Dobbins_19$defense <- paste("19", Dobbins_19$defense, sep="_")

## combine
Dobbins <- rbind(Dobbins_17, Dobbins_18, Dobbins_19)

Dobbins$defense <- factor(Dobbins$defense, levels = c("17_Indiana", "17_Oklahoma", "17_Army", "17_UNLV", "17_Rutgers", "17_Maryland",
                                                            "17_Nebraska", "17_Penn State", "17_Iowa", "17_Michigan State", "17_Illinois", "17_Michigan",
                                                            "17_Wisconsin", "18_Oregon State", "18_Rutgers", "18_TCU", "18_Tulane", "18_Penn State", "18_Indiana",
                                                      "18_Minnesota", "18_Purdue", "18_Nebraska", "18_Michigan State", "18_Maryland", "18_Michigan", "18_Northwestern", 
                                                      "19_Florida Atlantic", "19_Cincinnati", "19_Indiana", "19_Miami (OH)"))


## pbp chart

ggplot(data = Dobbins, aes(x = defense, y = mean_epa, group = 1)) + 
    geom_point() +
    geom_line() + 
    geom_hline(yintercept = 0, color = "red") +
    theme(axis.text.x = element_text(angle = 45))


## all rbs chart

ggplot(data = rusher_stats_19, aes(x = epa_sr_rate, y = avg_epa)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_text(x=0.48, y=0.22, label = "J.K. Dobbins") +
  geom_text(x=0.48, y=0.14, label = "Master Teague") +
  geom_point(data=rusher_stats_19[99:100, ], aes(x=epa_sr_rate, y=avg_epa), colour="red", size=2)





