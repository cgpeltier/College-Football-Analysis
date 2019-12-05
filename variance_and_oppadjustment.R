# note that epa_p should be read as: "probability of having an avg_epa less than this"

## avg_epa_p game variance
p <- c(0, 0.25, 0.5, 0.75, 1)

p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

box_variance_off <- box_score_stats %>%
    group_by(offense) %>%
    summarize_at(vars(avg_epa_p), p_funs) 

box_variance_def <- box_score_stats %>%
  group_by(defense) %>%
  summarize_at(vars(avg_epa_p), p_funs) 

## above code is from here: https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/


## add avg_epa
avg_off_epa <- season_stats_offense %>%
    select(offense, avg_epa_p)
avg_def_epa <- season_stats_defense %>%
  select(defense, avg_epa_p)

box_variance_off <- box_variance_off %>%
  inner_join(avg_off_epa, by = "offense")
box_variance_def <- box_variance_def %>%
  inner_join(avg_def_epa, by = "defense")

write.csv(box_variance_off, file = "box_variance_off.csv")
write.csv(box_variance_def, file = "box_variance_def.csv")


###################################
## offensive opp adjustment -- correct, can't do absolute value 
box_margin_off <- box_score_stats %>%
    select(1,2,5) %>%
    inner_join(season_stats_defense, by = "defense") %>%
    select(1:3, 7) %>%
    rename(off_avg_epa = avg_epa_p.x,
           def_season_epa = avg_epa_p.y) %>%
    mutate(avg_epa_var = off_avg_epa - def_season_epa)


box_margin_off_summary <- box_margin_off %>%
    group_by(offense) %>%
    summarise(avg_epa_var = mean(avg_epa_var, na.rm = TRUE))

box_margin_off_summary <- box_margin_off_summary %>%
    inner_join(season_stats_offense, by = "offense") %>%
    select(1,2)

## defensive opp adjustment
box_margin_def <- box_score_stats %>%
  select(1,2,5) %>%
  inner_join(season_stats_offense, by = "offense") %>%
  select(1:3, 7) %>%
  rename(def_avg_epa = avg_epa_p.x,
         off_season_epa = avg_epa_p.y) %>%
  mutate(avg_epa_var = def_avg_epa - off_season_epa)

box_margin_def_summary <- box_margin_def %>%
  group_by(defense) %>%
  summarise(avg_epa_var = mean(avg_epa_var, na.rm = TRUE))

box_margin_def_summary <- box_margin_def_summary %>%
  inner_join(season_stats_defense, by = "defense") %>%
  select(1,2)

## combine
box_margin_off_summary <- box_margin_off_summary %>%
    rename(off_epa_margin = avg_epa_var, team = offense)

box_margin_def_summary <- box_margin_def_summary %>%
  rename(def_epa_margin = avg_epa_var, team = defense)

opp_adj_epa <- box_margin_off_summary %>%
    inner_join(box_margin_def_summary, by = "team") %>%
    left_join(logo_team, by = "team")

opp_adj_epa$def_epa_margin <- abs(opp_adj_epa$def_epa_margin)
    
opp_adj_epa <- opp_adj_epa %>%
    mutate(total_epa_margin = off_epa_margin + def_epa_margin)

## plot
ggplot(data = opp_adj_epa, aes(x = off_epa_margin, y = def_epa_margin)) + 
    geom_image(aes(image = logos_list), size = .03, by = "width", asp = 1.8)
