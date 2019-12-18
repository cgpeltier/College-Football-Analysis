## new all season stats - offense
off_15_19 <- pbp_15_19_rp %>%
  group_by(offense, season) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA) %>% ungroup()


off_15_19 <- off_15_19 %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z))


## season stats - defense
def_15_19 <- pbp_15_19_rp %>%
  group_by(defense, season) %>%
  summarize(
    avg_epa = mean(EPA, na.rm=TRUE),
    avg_epa_z = NA,
    avg_epa_p = NA) %>% ungroup()


def_15_19 <- def_15_19 %>%
  mutate(
    avg_epa_z = scale(avg_epa),
    avg_epa_p = pnorm(avg_epa_z))


## avg EPA data frame
off_15_19 <- off_15_19 %>%
  select(1,2,5) %>%
  rename(team = offense)
def_15_19 <- def_15_19 %>%
  select(1,2,5) %>%
  rename(team = defense)

season_epa_15_19 <- off_15_19 %>%
  full_join(def_15_19, by = c("team", "season")) %>%
  rename(avg_epa_p_off = avg_epa_p.x, avg_epa_p_def = avg_epa_p.y) %>%
  mutate(avg_epa_p_def = 1-avg_epa_p_def) %>%
  inner_join(teams_logo, by = "team") %>%
  select(1:4)


season_epa_15_19 <- season_epa_15_19 %>%
    mutate(avg_epa_p_off = as.numeric(avg_epa_p_off),
           avg_epa_p_def = as.numeric(avg_epa_p_def), 
           avg_total_epa = (avg_epa_p_off + avg_epa_p_def)/2)


write.csv(season_epa_15_19, file = "season_epa_15_19.csv")


