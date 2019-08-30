plays_by_rb <- cfb_regular_play_2018 %>%
  group_by(rushing_player_name) %>%
  filter(rush == 1) %>%
  count(rush)

plays_by_rb <- plays_by_rb %>%
  na.omit(rushing_player_name) %>%
  filter(n > 50)


## mean epa by rb
rb_mean_epa <- cfb_regular_play_2018 %>%
  filter(rush == 1) %>%
  group_by(rushing_player_name) %>%
  summarise(mean_epa = mean(EPA))

rb_mean_epa <- rb_mean_epa %>%
  na.omit(rushing_player_name)

## success rate by school
success_by_rb<- cfb_regular_play_2018 %>%
  group_by(rushing_player_name) %>%
  filter(rush == 1) %>%
  count(success = success == 1)

success_by_rb <- success_by_rb %>%
  na.omit(rushing_player_name)

success_by_rb <- success_by_rb %>%
  filter(success == "TRUE") %>%
  select(rushing_player_name, success = n)

## combine columns
success_rate_by_rb <- merge(plays_by_rb, success_by_rb,
                            by.x = "rushing_player_name", by.y = "rushing_player_name")

success_rate_by_rb <- success_rate_by_rb %>%
  mutate(success_rate = (success / n)) %>%
  select(rushing_player_name, success_rate)

rb_epa_sr_off <- merge(success_rate_by_rb, rb_mean_epa,
                       by.x = "rushing_player_name", by.y = "rushing_player_name")

rb_epa_sr_off <- rb_epa_sr_off %>%
    filter(rushing_player_name != "TEAM" & rushing_player_name != "Team")

rb_epa_sr_off %>%
  summarize(mean(mean_epa))

rb_epa_sr_off %>%
  summarize(mean(success_rate))


ggplot(data = rb_epa_sr_off, aes(x = success_rate, y = mean_epa)) +
  geom_point() +
  geom_point(data = rb_epa_sr_off[148, ], 
             aes(x = success_rate, y = mean_epa), color = "firebrick1", size = 3) +
  geom_hline(yintercept= 0, color = "firebrick1") +
  geom_hline(yintercept = .0340925, linetype = "dashed", color = "black") +
  geom_vline(xintercept = .4501733, linetype = "dashed", color = "black")


## 13+ yard runs
exp_rushers <-  cfb_regular_play_2018 %>%
    group_by(rushing_player_name) %>%
    filter(rush == 1, yards_gained >= 13, sum(rush) > 50) %>%
    summarize(n = n())

## stuffs
stuff_rushers <-  cfb_regular_play_2018 %>%
  group_by(rushing_player_name) %>%
  filter(rush == 1, yards_gained <= 0, sum(rush) > 50, 
         (rushing_player_name != "TEAM" & rushing_player_name != "Team")) %>%
  summarize(n = n())


## stuff vs exp
stuff_exp_rushers <- merge(stuff_rushers, exp_rushers,
                           by.x = "rushing_player_name", by.y = "rushing_player_name")

# plot of stuff vs. exp
ggplot(data = stuff_exp_rushers, aes(x = n.x, y = n.y)) + 
    geom_point() + 
    geom_point(data = stuff_exp_rushers[148, ], 
             aes(x = n.x, y = n.y), color = "firebrick1", size = 3) + 
    xlab("stuffed runs") + 
    ylab("explosive runs (13+ yards)")
