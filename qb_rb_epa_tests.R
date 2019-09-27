## 2018 data cleaning
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

cfb_regular_play_2018$passing_player_name <- as.character(cfb_regular_play_2018$passing_player_name)
cfb_regular_play_2018$sacked_player_name <- as.character(cfb_regular_play_2018$sacked_player_name)

cfb_regular_play_2018$passing_player_name <- ifelse(cfb_regular_play_2018$play_type == "Sack", 
                                                    cfb_regular_play_2018$sacked_player_name, cfb_regular_play_2018$passing_player_name)



rusher_stats_18 <- cfb_regular_play_2018 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 30)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()


passer_stats_18 <- cfb_regular_play_2018 %>%
  group_by(offense, passing_player_name) %>%
  filter(passing_player_name != 0 & passing_player_name != "TEAM" & pass == 1 & sum(pass) > 30) %>%
  summarize(
    passes = sum(pass),
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()



## 2018 QB rusher test
rusher_stats_18 <- rusher_stats_18 %>%
  mutate(position = str_detect(rusher_stats_18$rushing_player_name, paste(passer_stats_18$passing_player_name,
                                                                          collapse = '|')))

rusher_stats_18$position <- if_else(rusher_stats_18$position == "TRUE", "QB", "RB")

ggplot(data = rusher_stats_18, aes(x = epa_sr_rate, y = avg_epa, colour = position)) +
  geom_point(aes(colour = position)) +
  geom_smooth(method=lm) 




## t-tests
qb_18_epa <- rusher_stats_18 %>% filter(position=="QB") %>% select("avg_epa")
rb_18_epa <- rusher_stats_18 %>% filter(position=="RB") %>% select("avg_epa")
qb_19_epa <- rusher_stats_19 %>% filter(position=="QB") %>% select("avg_epa")
rb_19_epa <- rusher_stats_19 %>% filter(position=="RB") %>% select("avg_epa")



## 2019 QB rusher test
rusher_stats_19 <- rusher_stats_19 %>%
  mutate(position = str_detect(rusher_stats_19$rushing_player_name, paste(passer_stats_19$passer_name,
                                                                          collapse = '|')))

rusher_stats_19$position <- if_else(rusher_stats_19$position == "TRUE", "QB", "RB")

ggplot(data = rusher_stats_19, aes(x = epa_sr_rate, y = avg_epa, colour = position)) +
  geom_point(aes(colour = position)) +
  geom_smooth(method=lm) 