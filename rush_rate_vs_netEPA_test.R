## standard down run rate(y), pass EPA - run EPA(x)

season_offense_test <- season_stats_offense %>%
    mutate(net_epa = avg_epa_pass - avg_epa_rush) %>%
    filter(plays > 100)

season_offense_test <- season_offense_test %>%
    mutate(quadrant = ifelse((net_epa >=0 & std.down.rush.rte >=0.6154122), 1, 
                             ifelse((net_epa<=0 & std.down.rush.rte >=0.6154122), 2,
                                    ifelse((net_epa<0 & std.down.rush.rte <0.6154122), 3,
                                            ifelse((net_epa>=0 & std.down.rush.rte <0.6154122), 4, 0)))))

ggplot(data=season_offense_test, aes(x=net_epa, y = std.down.rush.rte, color="red")) +
    geom_point(aes(size=avg_epa)) + 
    geom_text_repel(aes(label=season_offense_test$offense), color="black") +
    geom_hline(yintercept = 0.6154122, linetype="dashed") +
    geom_vline(xintercept = 0, linetype="dashed") +
    ylab(aes(label="Standard down rush rate")) +
    xlab(aes(label="Avg Pass EPA - Avg Rush EPA")) +
    geom_text(x=-0.75, y=.35, label="Rushing more efficient, 
              tend to pass") +
    geom_text(x=-0.75, y=.8, label="Rushing more efficient, 
              tend to run") +
    geom_text(x=0.75, y=.35, label="Passing more efficient, 
              tend to pass") +
    geom_text(x=0.75, y=.8, label="Passing more efficient, 
              tend to run") +
    facet_wrap(~ quadrant, ncol=4)
    
  