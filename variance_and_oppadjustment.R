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

box_margin_off <- box_score_stats %>%
    select(1:5) %>%
    inner_join(season_stats_defense, by = "defense") %>%
    select(1:3, 7) %>%
    rename(off_avg_epa = avg_epa.x,
           def_season_epa = avg_epa.y)

box_margin_off$def_season_epa <- abs(box_margin_off$def_season_epa)

box_margin_off <- box_margin_off %>%
    mutate(
        avg_epa_var = off_avg_epa - def_season_epa,
        avg_epa_per_var = (off_avg_epa - def_season_epa)/def_season_epa
    )
box_margin_off_summary <- box_margin__off %>%
    group_by(offense) %>%
    summarise(avg_epa_var = mean(avg_epa_var, na.rm = TRUE)) %>%

box_margin_off_summary <- box_margin_off_summary %>%
    inner_join(season_stats_offense, by = "offense") %>%
    select(1,2)

