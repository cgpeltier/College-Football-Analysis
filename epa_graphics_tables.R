

## EPA boxplots
uga_nd_def <- box_score_stats %>%
    filter(offense == "Notre Dame" & defense == "Georgia")

uga_nd_all <- rbind(uga_nd_all, uga_nd_def)

uga_nd_all_plays <- cfb_regular_play_2019 %>%
    mutate(penalty = ifelse(play_type == "Penalty", 1, 0)) %>%
    filter((offense == "Notre Dame" & defense == "Georgia") | (offense == "Georgia" & defense == "Notre Dame"))

uga_nd_all_plays <- uga_nd_all_plays %>%
  filter(penalty == 0 & (rush==1 | pass==1))

uga_nd_med <- summarize(group_by(uga_nd_all_plays, offense), med = median(EPA))

uga_nd_med$med <- round(uga_nd_med$med, digits = 2)

ggplot(data=uga_nd_all_plays, aes(x=offense, y=EPA, color=offense)) +
  geom_boxplot() +
  scale_color_manual(values=c("red", "navy")) +
  geom_text(data = uga_nd_med, aes(offense, med, label = med), 
            position = position_dodge(width = 0.8), size = 3, vjust = -0.5)


## tables not done
osu_epa_boxscore <- read_csv("osu_epa_boxscore.csv")
osu_epa_boxscore %>%
gt() %>%
  tab_header(
    title = "OSU vs. Miami (OH) EPA Box Score"
  ) %>%
  cols_label(X1 = "Stat",
             num.pass = "#",
             mean.epa = "EPA/Attempt") %>%
  tab_source_note(
    source_note = "Data from @CFB_Data and @903124"
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_data(
      rows = passing_player_name == "Max Duggan"
    )
  )%>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_data(
      rows = passing_player_name == "Alex Delton"
    )
  ) %>%
  data_color(
    columns = vars(mean.epa),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#008000"),
      domain = NULL
    )
  )