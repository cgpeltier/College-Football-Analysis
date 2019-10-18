library(tidyverse)

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
         epa_success = ifelse((rush == 1 | pass == 1) & EPA >= 0, 1, 0),
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


## remove UW - EWU
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    filter((offense != "Washington" & defense != "Eastern Washington") & 
             (offense != "Eastern Washington" & defense != "Washington"))


## box score stats
box_score_stats<- cfb_regular_play_2019 %>%
  group_by(offense, defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    epa_sr = mean(epa_success, na.rm=TRUE),
    epa_sr_z = NA,
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    avg_epa_rush_z = NA,
    epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    epa_sr_rush_z = NA,
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    short_rush_epa_z = NA,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    avg_epa_pass_z = NA,
    epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    epa_sr_pass_z = NA,
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_z = NA,
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    avg_rz_epa_sr_z = NA,
    std_down_epa = mean(EPA[std.down==1]),
    std_down_epa_z = NA,
    pass_down_epa = mean(EPA[pass.down==1]),
    pass_down_epa_z = NA
    ) %>% ungroup()

box_score_stats <- box_score_stats %>% 
  mutate(
    avg_epa_z = scale(avg_epa),
    epa_sr_z = scale(epa_sr),
    avg_epa_rush_z = scale(avg_epa_rush),
    epa_sr_rush_z = scale(epa_sr_rush),
    short_rush_epa_z = scale(short_rush_epa),
    avg_epa_pass_z = scale(avg_epa_pass),
    epa_sr_pass_z = scale(epa_sr_pass),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    std_down_epa_z = scale(std_down_epa),
    pass_down_epa_z = scale(pass_down_epa)
    )


## new all season stats - offense
season_stats_offense <- cfb_regular_play_2019 %>%
  group_by(offense, offense_conference) %>%
  filter(rush == 1 | pass == 1) %>%
    summarize(
      plays = n(),
      std_down_rush_rate = sum(rush[std.down==1]) / sum(std.down),
      avg_epa = mean(EPA, na.rm=TRUE),
      avg_epa_z = NA,
      epa_sr = mean(epa_success, na.rm=TRUE),
      epa_sr_z = NA,
      avg_epa_success = mean(EPA[epa_success==1], na.rm=TRUE),
      avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
      avg_epa_rush_z = NA,
      epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
      epa_sr_rush_z = NA,
      short_rush_epa = mean(EPA[short_rush_attempt==1]),
      short_rush_epa_z = NA,
      avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
      avg_epa_pass_z = NA,
      epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
      epa_sr_pass_z = NA,
      avg_rz_epa = mean(EPA[rz_play == 1]),
      avg_rz_epa_z = NA,
      avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
      avg_rz_epa_sr_z = NA,
      std_down_epa = mean(EPA[std.down==1]),
      std_down_epa_z = NA,
      pass_down_epa = mean(EPA[pass.down==1]),
      pass_down_epa_z = NA
    ) %>% ungroup()


season_stats_offense <- season_stats_offense %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    epa_sr_z = scale(epa_sr),
    avg_epa_rush_z = scale(avg_epa_rush),
    epa_sr_rush_z = scale(epa_sr_rush),
    short_rush_epa_z = scale(short_rush_epa),
    avg_epa_pass_z = scale(avg_epa_pass),
    epa_sr_pass_z = scale(epa_sr_pass),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    std_down_epa_z = scale(std_down_epa),
    pass_down_epa_z = scale(pass_down_epa))

season_stats_offense <- season_stats_offense %>%
    filter(offense_conference != is.na(offense_conference))

season_stats_defense <- cfb_regular_play_2019 %>%
  group_by(defense, defense_conference) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    epa_sr = mean(epa_success, na.rm=TRUE),
    epa_sr_z = NA,
    avg_epa_success = mean(EPA[epa_success==1], na.rm=TRUE),
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    avg_epa_rush_z = NA,
    epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    epa_sr_rush_z = NA,
    short_rush_epa = mean(EPA[short_rush_attempt==1]),
    short_rush_epa_z = NA,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    avg_epa_pass_z = NA,
    epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    epa_sr_pass_z = NA,
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_z = NA,
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    avg_rz_epa_sr_z = NA,
    std_down_epa = mean(EPA[std.down==1]),
    std_down_epa_z = NA,
    pass_down_epa = mean(EPA[pass.down==1]),
    pass_down_epa_z = NA
  ) %>% ungroup()


season_stats_defense <- season_stats_defense %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    epa_sr_z = scale(epa_sr),
    avg_epa_rush_z = scale(avg_epa_rush),
    epa_sr_rush_z = scale(epa_sr_rush),
    short_rush_epa_z = scale(short_rush_epa),
    avg_epa_pass_z = scale(avg_epa_pass),
    epa_sr_pass_z = scale(epa_sr_pass),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    std_down_epa_z = scale(std_down_epa),
    pass_down_epa_z = scale(pass_down_epa))

season_stats_defense <- season_stats_defense %>%
  filter(defense_conference != is.na(defense_conference))


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
    pass_down_epa = mean(EPA[pass.down==1])
  ) %>% ungroup()


## skill player stats
rusher_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 40)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiving_player_name != 0 & receiving_player_name != "TEAM" & pass == 1 & sum(pass) >=10) %>%
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


## avg EPA data frame
season_off_epa <- season_stats_offense %>%
    select(1,2,5) %>%
    rename(team = "offense")
season_def_epa <- season_stats_defense %>%
  select(1:3) %>%
  rename(team = "defense")


season_epa <- season_off_epa %>%
    full_join(season_def_epa, by = "team") %>%
    rename(avg_epa_off = avg_epa.x, avg_epa_def = avg_epa.y) 

######################
## write csvs
write.csv(national_season_stats, file = "national_season_stats.csv")
write.csv(box_score_stats, file = "box_score_stats.csv")
write.csv(season_stats_offense, file = "season_stats_off.csv")
write.csv(season_stats_defense, file = "season_stats_def.csv")

######################
## OSU
osu_off <- season_stats_offense %>%
    filter(offense == "Ohio State")
write.csv(osu_off, file = "osu_off_epa.csv")

osu_def <- season_stats_defense %>%
  filter(defense == "Ohio State")
write.csv(osu_def, file = "osu_def_epa.csv")


######################
## logos
## logos regex - \\/((?i)\\w+\\s{0,1}\\w+\\s{0,1}\\w+\\s{0,1}\\({0,1}\\w+\\){0,1}).png -- needs something to capture A&M

library(ggimage)
logos_list <- list.files("C:/Users/eqa47693/Desktop/CFB/logos", pattern = "*.png", full.names = TRUE)
logos_list_df <- as.data.frame(logos_list)
logo_team <- str_split(logos_list_df$logos_list, "C:/Users/eqa47693/Desktop/CFB/logos/", simplify = TRUE)
logo_team <- as_tibble(logo_team)
logo_team <- logo_team[,2]
logo_team <- logo_team %>% 
  mutate(team = str_replace(V2, ".png", ""))
logo_team <- logo_team[,2]
logo_team <- cbind(logo_team, logos_list_df)

## chart comparing qbs
passer_stats_19 <- passer_stats_19 %>%
    rename(team = offense)
passer_stats_19 <- passer_stats_19 %>%
  left_join(logo_team, by = "team")

ggplot(data = passer_stats_19, aes(x = epa_sr, y = avg_epa)) +
    geom_point() +
    geom_smooth(method=lm) +
    geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8)

## chart comparing rbs
rusher_stats_19 <- rusher_stats_19 %>%
    rename(team = offense) %>%
    left_join(logo_team, by = "team")

ggplot(data = rusher_stats_19, aes(x = epa_sr, y = avg_epa)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8)


## chart comparing wrs
ggplot(data = receiver_stats_19, aes(x = epa_sr, y = avg_epa)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_point(data=receiver_stats_19[507:512, ], aes(x=epa_sr, y=avg_epa), colour="red", size=2)


## density plot 
season_epa_gather <- season_epa %>%
    gather(key = "off_def", value = avg_epa, avg_epa_off, avg_epa_def)

ggplot(data = season_epa_gather, aes(x = avg_epa, fill = off_def)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0.3090757, linetype = "dashed") +
    geom_vline(xintercept = -0.3593487, linetype = "dashed")


## team offense vs defense
season_epa <- season_epa %>%
    left_join(logo_team, by = "team")

ggplot(data = season_epa, aes(x = avg_epa_off, y = avg_epa_def)) +
    geom_point() + 
    geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
    xlab("Offensive EPA per play") +
    ylab("Defensive EPA per play")

## success x avg epa 
season_stats_offense <- season_stats_offense %>%
    rename(team = offense) %>%
    left_join(logo_team, by = "team")

ggplot(data = season_stats_offense, aes(x = epa_sr, y = avg_epa_success)) +
    geom_point() + 
    geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
    xlab("Offensive EPA success rate") +
    ylab("EPA per successful play")



