library(tidyverse)
library(cfbscrapR)
library(ggimage)

##Pull season data from scrapR
cfb_regular_play_2019 <- data.frame()
for(i in 1:15){
  model <- cfb_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE)
  df <- data.frame(model)
  cfb_regular_play_2019 <- bind_rows(cfb_regular_play_2019, df)
}


cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
  rename(adjusted_yardline = adj_yd_line,
         offense = offense_play,
         defense = defense_play)

## Clean data
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
  mutate(rz_play = ifelse((adjusted_yardline <= 20), 1, 0), 
         so_play = ifelse((adjusted_yardline <= 40 | play_type == "(Passing Touchdown) | (Rushing Touchdown"), 1, 0),
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
         std.down = ifelse(down == 1, 1,
                        ifelse(down == 2 & distance < 8, 1, 
                           ifelse(down == 3 & distance < 5, 1,
                                  ifelse(down == 4 & distance < 5, 1, 0)))),
         pass.down = ifelse(down == 2 & distance > 8, 1, 
                            ifelse(down == 3 & distance > 5, 1, 
                                   ifelse(down == 4 & distance > 5, 1, 0)))
)
  
## Add logos
logos_list <- list.files("C:/Users/chad.peltier/OneDrive - IHS Markit/Desktop/CFB/logos", pattern = "*.png", full.names = TRUE)
logos_list_df <- as.data.frame(logos_list)
logo_team <- str_split(logos_list_df$logos_list, "C:/Users/chad.peltier/OneDrive - IHS Markit/Desktop/CFB/logos", simplify = TRUE)
logo_team <- as_tibble(logo_team)
logo_team <- logo_team[,2]
logo_team <- logo_team %>% 
    mutate(team = str_replace(V2, ".png", ""))
logo_team <- logo_team[,2]
logo_team <- logo_team %>%
    mutate(team = str_replace(team, "/", ""))
logo_team <- cbind(logo_team, logos_list_df)

teams <- read_csv("teams.csv")
teams <- teams %>%
    rename(team = school)

teams_logo <- logo_team %>%
    inner_join(teams, by = "team") %>%
    select(team, logo = logos_list)


## Extract player names
# RB names 
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    mutate(rush_player = ifelse(rush == 1, str_extract(play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA)) %>%
    mutate(rush_player = str_remove(rush_player, " run | \\d+ Yd Run"))

# QB names 
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    mutate(pass_player = ifelse(pass==1, str_extract(play_text, "pass from (.*?) \\(|(.{0,30} )pass |(.{0,30} )sacked|(.{0,30} )incomplete "), NA)) %>%
    mutate(pass_player = str_remove(pass_player, "pass | sacked| incomplete")) %>%
    mutate(pass_player = if_else(play_type == "Passing Touchdown", str_extract(play_text, "from(.+)"), pass_player),
          pass_player = str_remove(pass_player, "from "), 
          pass_player = str_remove(pass_player, "\\(.+\\)"),
          pass_player = str_remove(pass_player, " \\,"))

## Receiver names
cfb_regular_play_2019 <- cfb_regular_play_2019 %>%
    mutate(receiver_player = ifelse(pass==1, str_extract(play_text, "to (.+)"), NA)) %>%
    mutate(receiver_player = if_else(str_detect(play_text, "Yd pass"), str_extract(play_text, "(.+)\\d"), receiver_player)) %>%
    mutate(receiver_player = ifelse(play_type == "Sack", NA, receiver_player)) %>%
    mutate(receiver_player = str_remove(receiver_player, "to "),
           receiver_player = str_remove(receiver_player, "\\,.+"),
           receiver_player = str_remove(receiver_player, "for (.+)"),
           receiver_player = str_remove(receiver_player, "( \\d{1,2})"))


## box score stats
box_score_stats <- cfb_regular_play_2019 %>%
  group_by(offense, defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA,
    epa_sr = mean(epa_success, na.rm=TRUE),
    epa_sr_z = NA,
    epa_sr_p = NA,
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    avg_epa_rush_z = NA,
    avg_epa_rush_p = NA,
    epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    epa_sr_rush_z = NA,
    epa_sr_rush_p = NA,
    short_rush_epa = mean(epa_success[short_rush_attempt==1]),
    short_rush_epa_z = NA,
    short_rush_epa_p = NA,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    avg_epa_pass_z = NA,
    avg_epa_pass_p = NA,
    epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    epa_sr_pass_z = NA,
    epa_sr_pass_p = NA,
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_z = NA,
    avg_rz_epa_p = NA,
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    avg_rz_epa_sr_z = NA,
    avg_rz_epa_sr_p = NA,
    std_down_epa = mean(EPA[std.down==1]),
    std_down_epa_z = NA,
    std_down_epa_p = NA,
    pass_down_epa = mean(EPA[pass.down==1]),
    pass_down_epa_z = NA,
    pass_down_epa_p = NA,
    scoring_opp_epa = mean(EPA[scoring_opp==1]),
    scoring_opp_epa_z = NA,
    scoring_opp_epa_p = NA,
    rz_epa = mean(EPA[rz_play==1]),
    rz_epa_z = NA,
    rz_epa_p = NA
    ) %>% ungroup()

box_score_stats <- box_score_stats %>% 
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z),
    epa_sr_z = scale(epa_sr),
    epa_sr_p = pnorm(epa_sr_z),
    avg_epa_rush_z = scale(avg_epa_rush),
    avg_epa_rush_p = pnorm(avg_epa_rush_z),
    epa_sr_rush_z = scale(epa_sr_rush),
    epa_sr_rush_p = pnorm(epa_sr_rush_z),
    short_rush_epa_z = scale(short_rush_epa),
    short_rush_epa_p = pnorm(short_rush_epa_z),
    avg_epa_pass_z = scale(avg_epa_pass),
    avg_epa_pass_p = pnorm(avg_epa_pass_z),
    epa_sr_pass_z = scale(epa_sr_pass),
    epa_sr_pass_p = pnorm(epa_sr_pass_z),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_p = pnorm(avg_rz_epa_z),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    avg_rz_epa_sr_p = pnorm(avg_rz_epa_sr_z),
    std_down_epa_z = scale(std_down_epa),
    std_down_epa_p = pnorm(std_down_epa_z),
    pass_down_epa_z = scale(pass_down_epa),
    pass_down_epa_p = pnorm(pass_down_epa_z),
    scoring_opp_epa_z = scale(scoring_opp_epa),
    scoring_opp_epa_p = pnorm(scoring_opp_epa_z),
    rz_epa_z = scale(rz_epa),
    rz_epa_p = pnorm(rz_epa_z)
    )


## new all season stats - offense
season_stats_offense <- cfb_regular_play_2019 %>%
  group_by(offense) %>%
  filter(rush == 1 | pass == 1) %>%
    summarize(
      avg_epa = mean(EPA, na.rm=TRUE),
      avg_epa_z = NA,
      avg_epa_p = NA,
      epa_sr = mean(epa_success, na.rm=TRUE),
      epa_sr_z = NA,
      epa_sr_p = NA,
      avg_epa_success = mean(EPA[epa_success==1]),
      avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
      avg_epa_rush_z = NA,
      avg_epa_rush_p = NA,
      epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
      epa_sr_rush_z = NA,
      epa_sr_rush_p = NA,
      short_rush_epa = mean(epa_success[short_rush_attempt==1]),
      short_rush_epa_z = NA,
      short_rush_epa_p = NA,
      avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
      avg_epa_pass_z = NA,
      avg_epa_pass_p = NA,
      epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
      epa_sr_pass_z = NA,
      epa_sr_pass_p = NA,
      avg_rz_epa = mean(EPA[rz_play == 1]),
      avg_rz_epa_z = NA,
      avg_rz_epa_p = NA,
      avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
      avg_rz_epa_sr_z = NA,
      avg_rz_epa_sr_p = NA,
      std_down_epa = mean(EPA[std.down==1]),
      std_down_epa_z = NA,
      std_down_epa_p = NA,
      pass_down_epa = mean(EPA[pass.down==1]),
      pass_down_epa_z = NA,
      pass_down_epa_p = NA,
      scoring_opp_epa = mean(EPA[scoring_opp==1]),
      scoring_opp_epa_z = NA,
      scoring_opp_epa_p = NA,
      rz_epa = mean(EPA[rz_play==1]),
      rz_epa_z = NA,
      rz_epa_p = NA
    ) %>% ungroup()


season_stats_offense <- season_stats_offense %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z),
    epa_sr_z = scale(epa_sr),
    epa_sr_p = pnorm(epa_sr_z),
    avg_epa_rush_z = scale(avg_epa_rush),
    avg_epa_rush_p = pnorm(avg_epa_rush_z),
    epa_sr_rush_z = scale(epa_sr_rush),
    epa_sr_rush_p = pnorm(epa_sr_rush_z),
    short_rush_epa_z = scale(short_rush_epa),
    short_rush_epa_p = pnorm(short_rush_epa_z),
    avg_epa_pass_z = scale(avg_epa_pass),
    avg_epa_pass_p = pnorm(avg_epa_pass_z),
    epa_sr_pass_z = scale(epa_sr_pass),
    epa_sr_pass_p = pnorm(epa_sr_pass_z),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_p = pnorm(avg_rz_epa_z),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    avg_rz_epa_sr_p = pnorm(avg_rz_epa_sr_z),
    std_down_epa_z = scale(std_down_epa),
    std_down_epa_p = pnorm(std_down_epa_z),
    pass_down_epa_z = scale(pass_down_epa),
    pass_down_epa_p = pnorm(pass_down_epa_z),
    scoring_opp_epa_z = scale(scoring_opp_epa),
    scoring_opp_epa_p = pnorm(scoring_opp_epa_z),
    rz_epa_z = scale(rz_epa),
    rz_epa_p = pnorm(rz_epa_z)
  )


season_stats_offense <- season_stats_offense %>%
  rename(team = offense) %>%
  left_join(teams_logo, by = "team") %>%
  filter(logo != is.na(logo))

## season stats - defense
season_stats_defense <- cfb_regular_play_2019 %>%
  group_by(defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA,
    epa_sr = mean(epa_success, na.rm=TRUE),
    epa_sr_z = NA,
    epa_sr_p = NA,
    avg_epa_rush = mean(EPA[rush == 1], na.rm=TRUE),
    avg_epa_rush_z = NA,
    avg_epa_rush_p = NA,
    epa_sr_rush = mean(epa_success[rush == 1], na.rm=TRUE),
    epa_sr_rush_z = NA,
    epa_sr_rush_p = NA,
    short_rush_epa = mean(epa_success[short_rush_attempt==1]),
    short_rush_epa_z = NA,
    short_rush_epa_p = NA,
    avg_epa_pass = mean(EPA[pass == 1], na.rm=TRUE),
    avg_epa_pass_z = NA,
    avg_epa_pass_p = NA,
    epa_sr_pass = mean(epa_success[pass == 1], na.rm=TRUE),
    epa_sr_pass_z = NA,
    epa_sr_pass_p = NA,
    avg_rz_epa = mean(EPA[rz_play == 1]),
    avg_rz_epa_z = NA,
    avg_rz_epa_p = NA,
    avg_rz_epa_sr = mean(epa_success[rz_play == 1]),
    avg_rz_epa_sr_z = NA,
    avg_rz_epa_sr_p = NA,
    std_down_epa = mean(EPA[std.down==1]),
    std_down_epa_z = NA,
    std_down_epa_p = NA,
    pass_down_epa = mean(EPA[pass.down==1]),
    pass_down_epa_z = NA,
    pass_down_epa_p = NA,
    scoring_opp_epa = mean(EPA[scoring_opp==1]),
    scoring_opp_epa_z = NA,
    scoring_opp_epa_p = NA,
    rz_epa = mean(EPA[rz_play==1]),
    rz_epa_z = NA,
    rz_epa_p = NA
  ) %>% ungroup()


season_stats_defense <- season_stats_defense %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z),
    epa_sr_z = scale(epa_sr),
    epa_sr_p = pnorm(epa_sr_z),
    avg_epa_rush_z = scale(avg_epa_rush),
    avg_epa_rush_p = pnorm(avg_epa_rush_z),
    epa_sr_rush_z = scale(epa_sr_rush),
    epa_sr_rush_p = pnorm(epa_sr_rush_z),
    short_rush_epa_z = scale(short_rush_epa),
    short_rush_epa_p = pnorm(short_rush_epa_z),
    avg_epa_pass_z = scale(avg_epa_pass),
    avg_epa_pass_p = pnorm(avg_epa_pass_z),
    epa_sr_pass_z = scale(epa_sr_pass),
    epa_sr_pass_p = pnorm(epa_sr_pass_z),
    avg_rz_epa_z = scale(avg_rz_epa),
    avg_rz_epa_p = pnorm(avg_rz_epa_z),
    avg_rz_epa_sr_z = scale(avg_rz_epa_sr),
    avg_rz_epa_sr_p = pnorm(avg_rz_epa_sr_z),
    std_down_epa_z = scale(std_down_epa),
    std_down_epa_p = pnorm(std_down_epa_z),
    pass_down_epa_z = scale(pass_down_epa),
    pass_down_epa_p = pnorm(pass_down_epa_z),
    scoring_opp_epa_z = scale(scoring_opp_epa),
    scoring_opp_epa_p = pnorm(scoring_opp_epa_z),
    rz_epa_z = scale(rz_epa),
    rz_epa_p = pnorm(rz_epa_z)
  )


## national averages
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

season_stats_defense <- season_stats_defense %>%
    rename(team = defense) %>%
    left_join(teams_logo, by = "team") %>%
    filter(logo != is.na(logo))

## skill player stats
rusher_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, rush_player) %>%
  filter(rush_player != is.na(rush_player) & rush_player != "TEAM " & rush == 1 & (sum(rush) > 40)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiver_player != is.na(receiver_player) & receiver_player != "TEAM" & pass == 1 & sum(pass) >= 10) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE),
    plays = sum(pass)
  )%>% ungroup()

passer_stats_19 <- cfb_regular_play_2019 %>%
  group_by(offense, passer_player) %>%
  filter(passer_player != is.na(pass_player) & passer_player != "TEAM" & pass == 1 & sum(pass) > 40) %>%
  summarize(
    passes = sum(pass),
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()


## avg EPA data frame
season_off_epa <- season_stats_offense %>%
    select(1,4,7) 
season_def_epa <- season_stats_defense %>%
  select(1,4,7) 

season_epa <- season_off_epa %>%
    full_join(season_def_epa, by = "team") %>%
    rename(avg_epa_p_off = avg_epa_p.x, avg_epa_p_def = avg_epa_p.y) %>%
    mutate(avg_epa_p_def = 1-avg_epa_p_def)

season_epa <- season_epa %>%
    left_join(teams_logo, by = "team") %>%
    filter(logo != is.na(logo))

######################
## write csvs
write.csv(box_score_stats, file = "box_score_stats.csv")
write.csv(season_stats_offense, file = "season_stats_off.csv")
write.csv(season_stats_defense, file = "season_stats_def.csv")



