library(ggimage)

## chart comparing rbs -- may need to combine w/ logos
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
ggplot(data = season_epa, aes(x = avg_epa_p_off, y = avg_epa_p_def)) +
    geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
    xlab("Offensive EPA per play") +
    ylab("Defensive EPA per play") +
    labs(caption = "Chart by Chad Peltier, EPA data from cfbscrapR, PBP data from @CFB_Data")+
    ggsave("epa_off_def.png", height = 9/1.2, width = 16/1.2)

## success x avg epa 
ggplot(data = season_stats_offense, aes(x = epa_sr, y = avg_epa_success)) +
    geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
    xlab("Offensive EPA success rate") +
    ylab("EPA per successful play") +
    labs(caption = "EPA model data from cfbscrapR, play-by-play data from @CFB_Data")+
    ggsave("epa_success_avg_epa.png", height = 9/1.2, width = 16/1.2)


## explosive x avg explosive epa
ggplot(data = season_stats_offense, aes(x = epa_er, y = avg_epa_explosive)) +
  geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
  xlab("Offensive EPA explosiveness rate") +
  ylab("EPA per successful play") +
  labs(caption = "Chart by Chad Peltier, EPA model data from cfbscrapR, play-by-play data from @CFB_Data")+
  ggsave("epa_explosive_avg_epa.png", height = 9/1.2, width = 16/1.2)

## off SR vs avg EPA
ggplot(data=season_stats_offense, aes(x = epa_sr, y = avg_epa)) +
    geom_smooth(method = lm) + 
    geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
    xlab("EPA success rate") +
    ylab("EPA per play")
    ggsave("epa_off_sr_avg_epa.png", height = 9/1.2, width = 16/1.2)

## def SR vs avg EPA
ggplot(data=season_stats_defense, aes(x = epa_sr, y = avg_epa)) +
    geom_smooth(method = lm) + 
    geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
    xlab("EPA success rate") +
    ylab("EPA per play")
    ggsave("epa_def_sr_avg_epa.png", height = 9/1.2, width = 16/1.2)
    
## standard down run rate(y), pass EPA - run EPA(x)
season_offense_test <- season_stats_offense %>%
      mutate(net_epa = avg_epa_pass - avg_epa_rush) %>%
      filter(plays > 100) %>%
      select(-ends_with("_z")) %>%
      rename(team = offense) %>%
      left_join(logo_team, by = "team")
    
season_offense_test <- season_offense_test %>%
      mutate(quadrant = ifelse((net_epa >=0 & std_down_rush_rate >=0.617), 1, 
                               ifelse((net_epa<=0 & std_down_rush_rate >=0.617), 2,
                                      ifelse((net_epa<0 & std_down_rush_rate <0.617), 3,
                                             ifelse((net_epa>=0 & std_down_rush_rate <0.617), 4, 0)))))
    
ggplot(data=season_offense_test, aes(x=net_epa, y = std_down_rush_rate)) +
      geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
      geom_hline(yintercept = 0.617, linetype="dashed") +
      geom_vline(xintercept = 0, linetype="dashed") +
      ylab(aes(label="Standard down rush rate")) +
      xlab(aes(label="Avg Pass EPA - Avg Rush EPA")) +
      geom_text(x=-0.375, y=.35, label="Rushing more efficient, 
              tend to pass") +
      geom_text(x=-0.375, y=.95, label="Rushing more efficient, 
              tend to run") +
      geom_text(x=0.625, y=.35, label="Passing more efficient, 
              tend to pass") +
      geom_text(x=0.625, y=.95, label="Passing more efficient, 
              tend to run") +
      facet_wrap(~ quadrant, ncol=4)
    


## Visualizing all plays in a season by game
osu_plays <- cfb_regular_play_2019 %>%
  filter(!is.na(rush_pass) & offense == "Ohio State") %>%
  mutate(defense = if_else(game_id == 401132983, "Wisconsin_2", defense))

osu_plays$defense <- factor(osu_plays$defense, levels = c("Florida Atlantic", "Cincinnati", "Indiana",
                                                          "Miami (OH)", "Nebraska", "Michigan State",
                                                          "Northwestern", "Wisconsin", "Maryland",
                                                          "Rutgers", "Penn State", "Michigan",
                                                          "Wisconsin_2"))


ggplot(data = osu_plays, aes(x = defense, y = EPA)) + 
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  ggsave("epa_game_osu.png", height = 9/1.2, width = 16/1.2)
