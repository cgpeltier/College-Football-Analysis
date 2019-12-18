---
title: "How Players Switch Positions in College"
author: "Chad Peltier"
date: "12/16/2019"
output: 
  html_document:
    keep_md: true
---
This project is intended to analyze what positions players switch to in college based on how they were described as recruits. 

Often the most talented athlete on a team will play whatever position is of highest need at the high school level. Usually that's quarterback -- Ohio State's Darron Lee was a high school quarterback before moving to linebacker in college. He was designated as an "Athlete" on most recruiting bios. Florida State's Cam Akers was a quarterback in high school as well, even though he was recruited as a running back -- his position in college. Sometimes players get bigger than expected and move from defensive back to linebacker, or linebacker to defensive line, and sometimes linemen will switch sides of the ball, as Georgia's Netori Johnson did (from offensive guard to defensive line). Other times a team will go through a schematic switch, so a defensive end becomes a rush linebacker, for example. 

So this project attempts to look at the rate of position switches at the college level based on the players' positions from their 247 Composite recruiting profiles versus their positions as listed on official rosters. 

First we'll load the required packages.

```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.2.1     v purrr   0.3.3
## v tibble  2.1.3     v dplyr   0.8.3
## v tidyr   1.0.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.4.0
```

```
## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(ggalluvial)
library(cfbscrapR)
```

```
## Warning: replacing previous import 'mgcv::multinom' by 'nnet::multinom' when
## loading 'cfbscrapR'
```

# Creating the dataset 
First we'll need to create a dataset that matches players' recruiting profiles with their acutal college team roster data. 

We'll do that by writing a function to pull roster data for all teams using the cfbscrapR package, which pulls data from collegefootballdata.com. The original cfbscrapR function, cfb_team_roster_data, as well as the website, allows you to pull roster data for a single team at a time, so we'll use a function with a for loop to pull every FBS team at once and combine the rosters into a single data frame. 

Note that I use the "talent_test" data frame below for a list of all FBS teams. This data frame was used in another one of my projects, from "Analysis of Talent and Production in College Football." You can also get a list of teams from collegefootballdata.com -- we just need a vector of teams to run in the for loop. Also note that I had to remove San Jose State from the vector, because the accent mark wasn't allowing me to pull their roster data with the function. 

Finally, roster data is only available for the 2019 season, so that's what we're testing here -- it's only a snapshot look at how players change positions.


```r
talent_test <- read_csv("talent_test.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   team = col_character(),
##   year = col_double(),
##   talent = col_double(),
##   rating = col_double(),
##   predict_rating = col_double(),
##   residual = col_double()
## )
```

```r
teams_for_rosters <- talent_test %>%
    distinct(team) 

teams_for_rosters[91,1] <- NA 
teams_for_rosters <- teams_for_rosters %>%
    drop_na(team) 
teams_for_rosters <- as.vector(teams_for_rosters$team)


pull_team_rosters <- function(team_x) {  
    list_rosters <- list()
    roster_df <- data.frame()

    for(i in 1:length(team_x)){
        roster <- cfb_team_roster_data(team_x[i])
        df <- data.frame(roster)
        df <- df %>%
            mutate(team = team_x[i])
        roster_df <- bind_rows(roster_df, df)
        
    }
    list_rosters[[i]] <- roster_df
}

all_rosters <- pull_team_rosters(teams_for_rosters)
```

### Join roster data with recruiting data
Next we can actually join the roster data with the recruiting data. First we'll need to clean two things in the roster data we pulled above to help with the join. We'll create a new variable for "name", since the roster data has first and last names separated into two columns. We'll also rename the position variable so that we can distinguish between roster position and high school position.

Then we'll do the join based on name and college team. Since there's not a common player ID shared between the data sets, each player doesn't have a single unique identifier to use for the join. As a result we do the best we can using the players' names and their college team, which should do a decent job of matching the players up. 

The two biggest potential problems with this is that some players/colleges might have slight spelling differences between the two datasets (i.e. Hawaii vs. Hawai'i) and there might be players with the same name on the same team. This actually does happen at least once, as Louisville had a Chandler Jones, offensive guard, and a Chandler Jones, cornerback. 

While those are definite problems, this is more of an exploratory analysis. Assuming we get enough matches using an inner join, this can still be a useful proof of concept study to justify a more labor intensive look that gathers mutiple years of roster data and spends significantly more time reconciling player and team names.

Finally, this uses the "talent_data" data frame I created in the "Analysis of Talent and Production in College Football" project I mentioned previously. It was created using a for loop based on the cfb_recruiting function from cfbscrapR.


```r
talent_data <- read_csv("talent_data.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   recruitType = col_character(),
##   year = col_double(),
##   ranking = col_double(),
##   name = col_character(),
##   school = col_character(),
##   committedTo = col_character(),
##   position = col_character(),
##   height = col_double(),
##   weight = col_double(),
##   stars = col_double(),
##   rating = col_double(),
##   city = col_character(),
##   stateProvince = col_character(),
##   country = col_logical(),
##   off_def = col_character()
## )
```

```
## Warning: 22 parsing failures.
##   row     col           expected    actual              file
## 20944 country 1/0/T/F/TRUE/FALSE Germany   'talent_data.csv'
## 21184 country 1/0/T/F/TRUE/FALSE Italy     'talent_data.csv'
## 21251 country 1/0/T/F/TRUE/FALSE Germany   'talent_data.csv'
## 21990 country 1/0/T/F/TRUE/FALSE Australia 'talent_data.csv'
## 22006 country 1/0/T/F/TRUE/FALSE Sweden    'talent_data.csv'
## ..... ....... .................. ......... .................
## See problems(...) for more details.
```

```r
all_rosters <- all_rosters %>%
    mutate(name = str_c(first_name, last_name, sep = " ")) %>%
    rename(position_team = position)

all_rosters_join <- all_rosters %>%
    inner_join(talent_data, by = c("name", "team" = "committedTo")) %>%
    mutate(position_team = str_trim(position_team),
           position = str_trim(position))
```

### Clean roster data 
Next we need to clean the data and create a summary frequency dataset. The first step will be to clean up some of the roster positions. The positions don't quite line up with those used by the recruiting data (i.e. rosters will list nose tackles instead of just "DL" or "DT"). 

So, we'll make some edits to ensure some consistency on that side. We'll also remove special teams players, create a dummy variable noting whether a player switched positions or not (recognizing that being listed as a cornerback during recruitment and then a defensive back on a roster does not count as a position switch, for example), and then make a frequency table based on specific position switches (i.e. "ATH" to linebacker).


```r
## Cleaning
all_rosters_join <- all_rosters_join %>%
    mutate(position_team = replace(position_team, position_team == "C" | position_team == "OT"
                                   | position_team == "G", "OL"),
           position_team = replace(position_team, position_team == "CB" | position_team == "S", "DB"), 
           position_team = replace(position_team, position_team == "DT" | position_team == "DE" 
                                   | position_team == "NT", "DL"))

## Create summary frequency table for each position switch
position_summary <- all_rosters_join %>%
    group_by(position, position_team) %>%
    summarize(freq = n())


## Clean, create "switch" dummy variable and filter to just position switches
position_summary_filter <- position_summary %>%
    filter(position_team != "P" & position_team != "LS" & position_team != position) %>%
    mutate(same = if_else((position == "DT" | position == "DE") & position_team == "DL", 1,
                          if_else((position == "C" | position == "OT" | position == "G") & 
                            position_team == "OL", 1, 
                            if_else((position == "CB" | position == "S") & 
                                      position_team == "DB", 1, 0)))) %>%
             filter(same == 0)
```


# Alluvial chart
Next we can make an alluvial chart to visualize these position moves. This uses the ggalluvial package. The stratum colors are based on the roster position to help the viewer see where each roster position came from (rather than where high school positions went to).


```r
is_alluvia_form(position_summary_filter)
```

```
## Missing alluvia for some stratum combinations.
```

```
## [1] TRUE
```

```r
ggplot(data = position_summary_filter, aes(y = freq, axis1 = position, axis2 = position_team)) + 
    geom_alluvium(aes(fill = position_team), width = 1/12) + 
    geom_stratum(width = 1/12, fill = "white") +
    geom_text(stat = "stratum", infer.label = TRUE, size = 4)+
    labs(caption = "Chart by Chad Peltier, data from @CFB_Data. 
         College position data from 2019, HS position data 2015-2019.") +
    scale_x_discrete(limits = c("HS position (247)", "College position"), expand = c(.05,.05)) +
    ggtitle("How Players Switch Positions in College") +
    theme(legend.position = "none")+
    ggsave("positions.png", height = 9/1.2, width = 16/1.2)
```

```
## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.

## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.

## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.

## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.

## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.

## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
## params$discern): Some strata appear at multiple axes.
```

![](roster_position_project_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

This is interesting, and largely confirms some trends that we might have suspected based on anecdotal player switches. For example, college linebackers (who weren't recruited as linebackers) largely come from players tagged as athletes, safeties, and defensive ends. The latter two moves are obvious -- players either get bigger than expected (or have scheme switches that prioritize speedier linebackers) or high school defensive ends might play rush linebacker roles (as sometimes you see when elite pass rushing defensive ends get to the NFL). Defensive backs come from athletes and wide receivers -- the latter of whom likely just don't have the hands for receiver, but are otherwise excellent athletes (Mark Webb from Georgia is an example of this position switch). Athletes can go a lot of ways, but usually become defensive backs, wide receivers, then running backs and linebackers. 

### Probability 
While the chart above is definitely interesting to look at, it might also be worth seeing the probabilities of those same position switches in a table form. This code adds a column for probability of switching for each high school position group. 


```r
## For players that do switch positions
position_summary_filter <- position_summary_filter %>%
    mutate(perc = round(freq/sum(freq), 2))
```

### Probability of switching positions
While the above chart and table show probabilities for players that do switch positions, what if we wanted to just see the probability of a player switching positions in the first place? This final section will produce a probability of switching positions for each high school position.


```r
## Probability of switching
position_summary_test <- position_summary %>%
    filter(position_team != "P" & position_team != "LS") %>%
    mutate(position_team = if_else((position_team == "C" | position_team == "G" | position_team == "OL"
                                    | position_team == "OT"), "OL",
                                   if_else((position_team == "DE" | position_team == "DT" |
                                              position_team == "NT"), "DL",
                                           if_else((position_team == "S" | position_team == "CB"), "DB",
                                           position_team))), 
        same = if_else((position == "DT" | position == "DE") & position_team == "DL", 1,
                          if_else((position == "C" | position == "OT" | position == "G") & 
                            position_team == "OL", 1, 
                            if_else((position == "CB" | position == "S") & 
                                      position_team == "DB", 1,
                                    if_else(position_team == position, 1, 0)))))

    
position_summary_test2 <- position_summary_test %>%
    group_by(position) %>%
    summarize(switch_perc = sum(freq[same==0])/sum(freq))


position_summary_test2 %>%
    arrange(desc(switch_perc))
```

```
## # A tibble: 11 x 2
##    position switch_perc
##    <chr>          <dbl>
##  1 ATH           1     
##  2 S             0.231 
##  3 DE            0.218 
##  4 LB            0.188 
##  5 TE            0.181 
##  6 WR            0.164 
##  7 QB            0.151 
##  8 RB            0.148 
##  9 DT            0.136 
## 10 CB            0.0606
## 11 OL            0.0432
```

Interestingly, cornerbacks and offensive linemen rarely switch positions, while safeties and defensive ends are slightly more likely to switch positions than others. 





