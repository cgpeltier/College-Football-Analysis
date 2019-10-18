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
    geom_point() + 
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
    
  