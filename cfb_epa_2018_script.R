#test2

library(tidyverse)
library(ggplot2)

work_dir <- "C:/Users/eqa47693/Desktop/CFB/cfb_2018_epa"
setwd(work_dir)

cfb_regular_play_2018 <- read_csv("cfb_regular_play_18.csv")

## new play type and successful play variables
cfb_regular_play_2018 <- cfb_regular_play_2018 %>%
  mutate(pass = if_else(str_detect(play_text, "(pass)|(sacked)"), 1, 0),
         rush = if_else(str_detect(play_text, " run"), 1, 0),
         success = if_else(EPA > 0, 1, 0))

# combine run/pass into single column
cfb_regular_play_2018 <- cfb_regular_play_2018 %>%
  mutate(play_type_simple = 0)

cfb_regular_play_2018$play_type_simple[cfb_regular_play_2018$pass == 1] <- "pass"
cfb_regular_play_2018$play_type_simple[cfb_regular_play_2018$rush == 1] <- "rush"

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

