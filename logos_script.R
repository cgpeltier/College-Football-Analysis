library(ggimage)
library(tidyverse)


## for R
logos_list <- list.files("C:/Users/eqa47693/Desktop/CFB/logos", pattern = "*.png", full.names = TRUE)
logos_list_df <- as.data.frame(logos_list)
logo_team <- str_split(logos_list_df$logos_list, "C:/Users/eqa47693/Desktop/CFB/logos/", simplify = TRUE)
logo_team <- as_tibble(logo_team)
logo_team <- logo_team[,2]
logo_team <- logo_team %>% 
  mutate(team = str_replace(V2, ".png", ""))
logo_team <- logo_team[,2]
logo_team <- cbind(logo_team, logos_list_df)

## for tableau
logos_list <- list.files("C:/Users/eqa47693/Documents/My Tableau Repository/Shapes/cfb_logos", pattern = "*.png", full.names = TRUE)
logos_list_df <- as.data.frame(logos_list)
logo_team <- str_split(logos_list_df$logos_list, "C:/Users/eqa47693/Documents/My Tableau Repository/Shapes/cfb_logos", simplify = TRUE)
logo_team <- as_tibble(logo_team)
logo_team <- logo_team[,2]
logo_team <- logo_team %>% 
  mutate(team = str_replace(V2, ".png", ""))
logo_team <- logo_team[,2]
logo_team <- cbind(logo_team, logos_list_df)
logo_team$team <- as.character(logo_team$team) 
logo_team$team <- str_sub(logo_team$team, 2, -1)
write.csv(logo_team, file = "logo_team2.csv")


season_stats_off2 <- season_stats_offense %>%
    left_join(logo_team, by = "team")
season_stats_defense <- season_stats_defense %>%
    rename(team = defense)
season_stats_def2 <- season_stats_defense %>%
  left_join(logo_team, by = "team")
write.csv(season_stats_off2, file = "season_stats_off2.csv")
write.csv(season_stats_def2, file = "season_stats_def2.csv")

## directory for tableau team logos
"C:/Users/eqa47693/Documents/My Tableau Repository/Shapes/cfb_logos"