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