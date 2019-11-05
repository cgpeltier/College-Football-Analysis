## chart comparing rbs -- may need to combine w/ logos
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
