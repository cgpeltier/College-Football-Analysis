### quarterback analysis


osu_plays_2018 <- cfb_regular_play_2018 %>%
    filter(offense == "Ohio State" | defense == "Ohio State")

osu_plays_2018 %>%
    filter(passing_player_name == "Dwayne Haskins") %>%
    summarize(mean_epa = mean(EPA))


#############
### calculate qb mean epa / sr
## total plays by school
plays_by_qb <- cfb_regular_play_2018 %>%
  group_by(passing_player_name) %>%
  filter(pass == 1) %>%
  count(pass)

plays_by_qb <- plays_by_qb %>%
  na.omit(passing_player_name) %>%
  filter(n > 75)


## mean epa by qb
qb_mean_epa <- cfb_regular_play_2018 %>%
  filter(pass == 1) %>%
  group_by(passing_player_name) %>%
  summarise(mean_epa = mean(EPA))

qb_mean_epa <- qb_mean_epa %>%
  na.omit(passing_player_name)

## success rate by school
success_by_qb<- cfb_regular_play_2018 %>%
  group_by(passing_player_name) %>%
  filter(pass == 1) %>%
  count(success = success == 1)

success_by_qb <- success_by_qb %>%
  na.omit(passing_player_name)

success_by_qb <- success_by_qb %>%
  filter(success == "TRUE") %>%
  select(passing_player_name, success = n)

## combine columns
success_rate_by_qb <- merge(plays_by_qb, success_by_qb,
                                by.x = "passing_player_name", by.y = "passing_player_name")

success_rate_by_qb <- success_rate_by_qb %>%
  mutate(success_rate = (success / n)) %>%
  select(passing_player_name, success_rate)

qb_epa_sr_off <- merge(success_rate_by_qb, qb_mean_epa,
                    by.x = "passing_player_name", by.y = "passing_player_name")
qb_epa_sr_off %>%
  summarize(mean(mean_epa))

qb_epa_sr_off %>%
  summarize(mean(success_rate))

### make offense epa chart
ggplot(data = qb_epa_sr_off, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_point(data = qb_epa_sr_off[54, ], 
             aes(x = success_rate, y = mean_epa), color = "firebrick1", size = 3) +
  geom_hline(yintercept= 0, color = "firebrick1") +
  geom_hline(yintercept = .1356729, linetype = "dashed", color = "black") +
  geom_vline(xintercept = .4499524, linetype = "dashed", color = "black")


