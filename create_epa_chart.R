## The code below allows you to create an average EPA offense x defense chart.
# The only thing you'll need to change in the code below is for the logos:
# You can either download a folder of team logos and update the code below with that directory, 
# or use the "teams.csv" column `logos[1]`. 

library (tidyverse)
devtools::install_github("meysubb/cfbscrapR")
library(cfbscrapR)

## Pull data from cfbscrapR
season_df <- data.frame()
for(i in 1:14){
  model <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE)
  df <- data.frame(model)
  season_df <- bind_rows(season_df, df)
}


## Clean the dataframe
season_df <- season_df %>%
    rename(adjusted_yardline = adj_yd_line,
         offense = offense_play,
         defense = defense_play) %>%
    mutate(pass = if_else(play_type == "Pass Reception" | play_type == "Passing Touchdown" |
                             play_type == "Sack" | play_type == "Pass Interception Return" |
                             play_type == "Pass Incompletion" | play_type == "Sack Touchdown" |
                             (play_type == "Safety" & str_detect(play_text, "sacked")) |
                             (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "pass")) |
                             (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "pass")), 1, 0),
            rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown" | (play_type == "Safety" & str_detect(play_text, "run")) |
                            (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "run")) | 
                            (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "run")), 1, 0))


## Make EPA summary dataframe
# Note: This calculates the average EPA, avg EPA z-scores, and z-scores pecentiles.
# The z-score percentile can be interpreted as "the percentage likelihood of a random FBS team's average EPA being lower than team x"
season_df_off <- season_df %>%
  group_by(offense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA)

season_df_off <- season_df_off %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z))

season_df_def <- season_df %>%
  group_by(defense) %>%
  filter(rush == 1 | pass == 1) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA)

season_df_def <- season_df_def %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z))


season_df_off <- season_df_off %>%
  select(1,4) %>%
  rename(team = "offense")
season_df_def <- season_df_def %>%
  select(1,4) %>%
  rename(team = "defense")

season_df_epa <- season_df_off %>%
  full_join(season_df_def, by = "team") %>%
  rename(avg_epa_p_off = avg_epa_p.x, avg_epa_p_def = avg_epa_p.y) %>%
  mutate(avg_epa_p_def = 1-avg_epa_p_def)


## Pull in logos
# Note: the file "teams.csv" below is from collegefootballdata.com and includes a list of all FBS teams. 
# I use it primarily to filter down to only FBS teams. It is located in the "C:/directory location of team logos" below. Update the logo directory on your computer
# My team logos came from collegefootballdata.com. I will update this with the link after I confirm it is OK to share.
# Note 2: I have a folder of all team logos, which the code below uses. You could also use the `logo[1]` column in the teams.csv df
# The teams.csv file can be queried here: https://collegefootballdata.com/exporter/teams/fbs

library(ggimage)
logos_list <- list.files("C:/directory location of team logos", pattern = "*.png", full.names = TRUE)
logos_list_df <- as.data.frame(logos_list)
logo_team <- str_split(logos_list_df$logos_list, "C:/directory location of team logos", simplify = TRUE)
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


## Join epa data with logo dataframe
season_df_epa <- season_df_epa %>%
  left_join(teams_logo, by = "team") %>%
  filter(logo != is.na(logo))

## Make chart
ggplot(data = season_df_epa, aes(x = avg_epa_p_off, y = avg_epa_p_def)) +
  geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
  xlab("Offensive EPA per play") +
  ylab("Defensive EPA per play") +
  labs(caption = "EPA model data from cfbscrapR, play-by-play data from @CFB_Data")
ggsave("epa_off_def.png", height = 9/1.2, width = 16/1.2)
