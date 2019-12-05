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
season_epa <- season_epa %>%
  left_join(teams_logo, by = "team") %>%
  filter(logo != is.na(logo))

ggplot(data = season_epa, aes(x = avg_epa_p_off, y = avg_epa_p_def)) +
  geom_image(aes(image = logo), size = .03, by = "width", asp = 1.8) +
  xlab("Offensive EPA per play") +
  ylab("Defensive EPA per play") +
  labs(caption = "EPA model data from cfbscrapR, play-by-play data from @CFB_Data")
  ggsave("epa_off_def.png", height = 9/1.2, width = 16/1.2)


## success x avg epa 
season_stats_offense <- season_stats_offense %>%
  rename(team = offense) %>%
  left_join(logo_team, by = "team")

ggplot(data = season_stats_offense, aes(x = epa_sr, y = avg_epa_success)) +
    geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
    xlab("Offensive EPA success rate") +
    ylab("EPA per successful play") +
    labs(caption = "EPA model data from @statowar + 903124S, play-by-play data from @CFB_Data")



## off SR vs avg EPA
ggplot(data=season_stats_offense, aes(x = epa_sr, y = avg_epa)) +
  geom_point() + 
  geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
  xlab("EPA success rate") +
  ylab("EPA per play")

## def SR vs avg EPA
season_stats_defense <- season_stats_defense %>%
  rename(team = defense) %>%
  left_join(logo_team, by = "team")

ggplot(data=season_stats_defense, aes(x = epa_sr, y = avg_epa)) +
  geom_point() + 
  geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8) +
  xlab("EPA success rate") +
  ylab("EPA per play")
