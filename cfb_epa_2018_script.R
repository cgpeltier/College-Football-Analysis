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
         epa_success = ifelse(EPA > 0, 1, 0),
         short_rush_attempt = ifelse(distance <= 2 & rush == 1, 1, 0),
         short_rush_success = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, 1, 0),
         std.down = ifelse(down == 2 & distance < 8, 1, 
                           ifelse(down == 3 & distance < 5, 1,
                                  ifelse(down == 4 & distance < 5, 1, 0))),
         pass.down = ifelse(down == 2 & distance > 8, 1, 
                            ifelse(down == 3 & distance > 5, 1, 
                                   ifelse(down == 4 & distance > 5, 1, 0)))
  )
  

## box score stats
box_score_stats<- cfb_regular_play_2019 %>%
  group_by(offense, defense) %>%
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
season_stats <- cfb_regular_play_2019 %>%
  group_by(offense) %>%
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
rusher_stats <- cfb_regular_play_2019 %>%
  group_by(offense, rushing_player_name) %>%
  filter(rushing_player_name != 0 & rushing_player_name != "TEAM " & rush == 1 & (sum(rush) > 4)) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

receiver_stats <- cfb_regular_play_2019 %>%
  group_by(offense, receiving_player_name) %>%
  filter(receiving_player_name != 0 & receiving_player_name != "TEAM") %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE),
    plays = n()
  )%>% ungroup()

passer_stats <- cfb_regular_play_2019 %>%
  group_by(offense, passing_player_name) %>%
  filter(passing_player_name != 0 & passing_player_name != "TEAM") %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    epa_sr_rate = mean(epa_success, na.rm=TRUE)
  ) %>% ungroup()











########################################
## charts
### epa charts - offense
## total plays by school
plays_by_school <- cfb_regular_play_2018 %>%
  group_by(offense, offense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  count(pass, rush)

plays_by_school <- plays_by_school %>%
  na.omit(offense_conference)

total_plays_by_school <- plays_by_school %>%
  group_by(offense) %>%
  summarize(total_plays = sum(n))

## mean epa by school
mean_epa_by_school <- cfb_regular_play_2018 %>%
  group_by(offense, offense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  summarize(mean_epa = mean(EPA))

mean_epa_by_school <- mean_epa_by_school %>%
  na.omit(offense_conference)

## success rate by school
success_by_school <- cfb_regular_play_2018 %>%
  group_by(offense, offense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  count(success = success == 1)

success_by_school <- success_by_school %>%
  na.omit(offense_conference)

success_by_school <- success_by_school %>%
  filter(success == "TRUE") %>%
  select(offense, success = n)

## combine columns
success_rate_by_school <- merge(total_plays_by_school, success_by_school,
                                by.x = "offense", by.y = "offense")

success_rate_by_school <- success_rate_by_school %>%
  mutate(success_rate = (success / total_plays)) %>%
  select(offense, success_rate)

epa_sr_off <- merge(success_rate_by_school, mean_epa_by_school,
                    by.x = "offense", by.y = "offense")
epa_sr_off %>%
  summarize(mean(mean_epa))

epa_sr_off %>%
  summarize(mean(success_rate))

### make offense epa chart
ggplot(data = epa_sr_off, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_hline(yintercept= 0, color = "firebrick1") +
  geom_hline(yintercept = 0.01913031, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.4364425, linetype = "dashed", color = "black")

rownames(epa_sr_off) <- epa_sr_off[, 1]

ggplot(data = epa_sr_off, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_text(label = rownames(epa_sr_off)) +
  geom_text_repel()

### epa chart - defense
## total plays by school
plays_by_school_def <- cfb_regular_play_2018 %>%
  group_by(defense, defense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  count(pass, rush)

plays_by_school_def <- plays_by_school_def %>%
  na.omit(defense_conference)

total_plays_by_school_def <- plays_by_school_def %>%
  group_by(defense) %>%
  summarize(total_plays = sum(n))

## mean epa by school
mean_epa_by_school_def <- cfb_regular_play_2018 %>%
  group_by(defense, defense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  summarize(mean_epa = mean(EPA))

mean_epa_by_school_def <- mean_epa_by_school_def %>%
  na.omit(defense_conference)

## success rate by school
success_by_school_def <- cfb_regular_play_2018 %>%
  group_by(defense, defense_conference) %>%
  filter(pass == 1 | rush == 1) %>%
  count(success = success == 1)

success_by_school_def <- success_by_school_def %>%
  na.omit(defense_conference)

success_by_school_def <- success_by_school_def %>%
  filter(success == "TRUE") %>%
  select(defense, success = n)

## combine columns
success_rate_by_school_def <- merge(total_plays_by_school_def, success_by_school_def,
                                    by.x = "defense", by.y = "defense")

success_rate_by_school_def <- success_rate_by_school_def %>%
  mutate(success_rate = (success / total_plays)) %>%
  select(defense, success_rate)

epa_sr_def <- merge(success_rate_by_school_def, mean_epa_by_school_def,
                    by.x = "defense", by.y = "defense")


epa_sr_def %>%
  summarize(mean(mean_epa))

epa_sr_def %>%
  summarize(mean(success_rate))

### make chart comparing sr and mean_epa for all 2018 teams
ggplot(data = epa_sr_def, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_hline(yintercept= 0, color = "firebrick1") +
  geom_hline(yintercept = -0.003948178, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0.4279912, linetype = "dashed", color = "black")

rownames(epa_sr_def) <- epa_sr_def[, 1]

ggplot(data = epa_sr_def, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_text(label = rownames(epa_sr_def))






########################################

### team-specific epa                                         
## filter to OSU
osu_2018_epa_plays <- cfb_regular_play_2018 %>%
  filter(off_full_name == "Ohio State" | def_full_name == "Ohio State")

# filter off
off_osu_2018_epa_plays <- osu_2018_epa_plays %>%
  filter(off_full_name == "Ohio State")

# filter def
def_osu_2018_epa_plays <- osu_2018_epa_plays %>%
  filter(def_full_name == "Ohio State")

## calculate mean epa (offense by game)
osu_off_epa_by_game <- osu_2018_epa_plays %>%
  group_by(defense) %>%
  filter(off_full_name == "Ohio State") %>%
  summarise(mean_epa = mean(EPA))

osu_off_epa_by_game$defense <- factor(osu_off_epa_by_game$defense, levels = c("Oregon State", "Rutgers", "TCU", "Tulane",
                                                                              "Penn State", "Indiana","Minnesota", 
                                                                              "Purdue", "Nebraska", "Michigan State", 
                                                                              "Maryland", "Michigan","Northwestern"))
# make plot of mean off epa by game
ggplot(data = osu_off_epa_by_game, aes(x = defense, y = mean_epa, group = 1)) +
  geom_point(color = "firebrick1") +
  geom_line(color = "gray") + 
  geom_hline(yintercept= 0, color = "black")

## make plot of EPA and adj_yard_line
ggplot(data = off_osu_2018_epa_plays, aes(x = adjusted_yardline, y = EPA)) + 
  geom_point(color = "firebrick1") +
  geom_hline(yintercept= 0, color = "black")

#############################################
## filter to UGA
uga_2018_epa_plays <- cfb_regular_play_2018 %>%
  filter(off_full_name == "Georgia" | def_full_name == "Georgia")

# filter off
off_uga_2018_epa_plays <- uga_2018_epa_plays %>%
  filter(off_full_name == "Georgia")

# filter def
def_uga_2018_epa_plays <- uga_2018_epa_plays %>%
  filter(def_full_name == "Georgia")

# fix issue with both pass and rush 
uga_2018_epa_plays$rush[uga_2018_epa_plays$X1 == 5820] <- 0

## calculate mean epa (offense by game)
uga_off_epa_by_game <- uga_2018_epa_plays %>%
  group_by(defense, rush, pass) %>%
  filter(off_full_name == "Georgia", (pass == 1 | rush == 1)) %>%
  summarise(mean_epa = mean(EPA))

uga_off_epa_by_game$defense <- factor(uga_off_epa_by_game$defense, levels = c("Austin Peay", "South Carolina", "Middle Tennessee",
                                                                              "Missouri", "Tennessee", "Vanderbilt",
                                                                              "LSU", "Florida", "Kentucky", "Auburn",
                                                                              "UMass", "Georgia Tech", "Alabama"))

# make plot of mean epa by game
ggplot(data = uga_off_epa_by_game, aes(x = defense, y = mean_epa, group = play_type_simple)) +
  geom_point(color = "firebrick1") +
  geom_line(aes(color = play_type_simple)) + 
  scale_color_manual(values=c("firebrick1", "black")) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept= 0, color = "black")

## make plot of EPA and adj_yard_line
ggplot(data = off_uga_2018_epa_plays, aes(x = adjusted_yardline, y = EPA)) + 
  geom_point(color = "firebrick1") +
  geom_hline(yintercept= 0, color = "black") 


# EPA by down
uga_2018_epa_plays %>%
  group_by(down, rush, pass) %>%
  filter(offense == "Georgia", (rush == 1 | pass == 1)) %>%
  summarise(mean_epa = mean(EPA))

