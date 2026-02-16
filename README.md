---
title: "2025 MLB Player-Pitch Rankings"
author: "By Cole Geller"
output: html_document
---
## Objective
Identify the most effective pitch during the 2025 MLB season using a publicly available performance-metric dataset with a player-pitch unit-of-observation.

## Methods
Using the __[Baseball Savant Pitch Arsenal Stats Leaderboard (2025)][id]__, I analyzed all 508 player-pitches thrown at least 405 times (Baseball Savant’s qualifier). To evaluate overall effectiveness, each pitch was ranked across these key performance metrics:

* Run Value ___(RV)___ and Run Value per 100 pitches ___(RV/100)___
* Batting Average ___(BA)___ and Expected Batting Average ___(xBA)___
* Slugging Percentage ___(SLG)___ and Expected Slugging Percentage ___(xSLG)___
* Weighted On-Base Average ___(wOBA)___ and Expected Weighted On-Base Average ___(xwOBA)___
* Whiff Percentage ___(Whiff %)___ and Strikeout Percentage ___(K%)___
* Put-Away Percentage ___(Put Away %)___ and Hard-Hit Percentage ___(Hard Hit %)___

## Data Wrangling
```{r}

# Loading dataset
player_pitch <- read.csv('pitch-arsenal-stats.csv')

# Removing unnecessary columns
player_pitch_ranked <- subset(player_pitch, select = -c(team_name_alt, player_id, pitch_name, pitches, pitch_usage, pa))

# Unranked DataFrame
head(player_pitch_ranked[,c("last_name..first_name", "pitch_type", "run_value", "slg", "woba", "k_percent", "put_away")],10)
```
^ A subset of the ___player_pitch___ DataFrame with ten player-pitch observations, along with raw, unranked statistical values for several metrics.

### Rankings
Depending on the metric, a higher value can imply better or worse performance. For example, a higher RV indicates better performance, whereas a higher Hard Hit % indicates worse performance. This factor was considered when generating rankings.


```{r, message = FALSE}
library(dplyr)

# Metrics where higher is better
higher_is_better_metrics <- c("run_value_per_100", "run_value", "whiff_percent", "k_percent", "put_away")

# Metrics where lower is better
lower_is_better_metrics <- c("ba", "slg", "woba", "est_ba", "est_slg", "est_woba", "hard_hit_percent")

# All metrics
performance_metrics <- c(lower_is_better_metrics, higher_is_better_metrics)


# Generating rankings
player_pitch_ranked <- player_pitch_ranked %>% 
  
  mutate(
    across(all_of(higher_is_better_metrics), ~ rank(-.x, ties.method = "min")),
    across(all_of(lower_is_better_metrics), ~ rank(.x, ties.method = "min"))
  )

#Ranked DataFrame
head(player_pitch_ranked[,c("last_name..first_name", "pitch_type", "run_value", "slg", "woba", "k_percent", "put_away")],10)
```
^ A subset of the ___player_pitch_ranked___ DataFrame with ten player-pitch observations, along with rankings for several metrics.

### Overall Performance Score
Rankings across all twelve metrics were summed to produce an overall performance score, where lower values represent stronger effectiveness. A pitch that ranked first in every metric would receive a score of 12 x 1(st) = 12, while a pitch that ranked last in every metric would receive 12 x 508(th) = `r 508*12`.

```{r}
# Generating overall performance scores
player_pitch_ranked$overall_performance_score <- rowSums(player_pitch_ranked[, performance_metrics])
player_pitch_ranked %>%
  select(last_name..first_name, pitch_type, overall_performance_score) %>%
  head(10)
```
^ A subset of the ___player_pitch_ranked___ DataFrame with ten player-pitch observations, along with their overall performance scores.

## Results
### Section 1 - Overall Patterns
The distribution of overall performance scores appears approximately uniform to slightly mound-shaped, with scores spread relatively evenly across the full range. There is no strong skew, and no single cluster dominates the distribution, which suggests that pitch effectiveness varies widely but without a strong concentration of highly exceptional or extremely poor performers.

There are more observations near the center of the distribution (roughly 2,000–4,000), consistent with many pitches being “middle of the pack,” while far fewer pitches fall at the extreme low (elite) or extreme high (weak) ends of the spectrum. This aligns with expectations in a large dataset of MLB pitches: only a small number of pitches perform exceptionally across all metrics, and only a small number perform poorly across all metrics.

Overall, the histogram reflects a broad and evenly dispersed performance landscape, with the best pitches being clear outliers relative to the majority.
```{r, message = FALSE}
library(ggplot2)

ggplot(player_pitch_ranked, aes(x = overall_performance_score)) + 
  geom_histogram(bins = 30, color = 'white', linewidth = 0.3) + 
  labs(
    title = "Distribution of Overall Performance Scores",
    x = "Overall Performance Score",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)
```

```{r}
# Top 10 Overall Performance Scores
sorted <- player_pitch_ranked %>%
  select(last_name..first_name, pitch_type, overall_performance_score) %>%
  arrange(overall_performance_score)

head(sorted, 10)
```
^ Best overall performance scores

```{r}
# Bottom 10 Overall Performance Scores
tail(sorted, 10)
```
^ Worst overall performance scores



### Section 2 - Top Performer: Mason Miller's Slider
Mason Miller's slider ranked as the top-performing pitch among qualifiers by generating the minimum overall-performance score. 
```{r}
# Best Player-Pitch (Minimum Overall Performance Score)
best_player_pitch <- player_pitch_ranked %>%
  filter(overall_performance_score == min(overall_performance_score))

cat(
  paste(
    "Best Pitch:", best_player_pitch$last_name..first_name, "-", best_player_pitch$pitch_type)
)
```

#### Mason Miller's Slider Rankings:
* Run Value: (`r best_player_pitch$run_value`)
* RV/100: (`r best_player_pitch$run_value_per_100`)
* Batting Average: (`r best_player_pitch$ba`)
* Slugging Percentage: (`r best_player_pitch$slg`)
* Weighted On-Base Average: (`r best_player_pitch$woba`)
* Strikeout Percentage: (`r best_player_pitch$k_percent`)
* Put-Away Percentage: (`r best_player_pitch$put_away`)
* Expected Batting Average: (`r best_player_pitch$est_ba`)
* Expected Slugging Percentage: (`r best_player_pitch$est_slg`)
* Expected Weighted On-Base Average: (`r best_player_pitch$est_woba`)
* Hard-Hit Percentage: (`r best_player_pitch$hard_hit_percent`)
* Whiff Percentage: (`r best_player_pitch$whiff_percent`)

## Conclusion
Mason Miller’s slider stands out as the most effective pitch of the 2025 MLB season, ranking at or near the top across both contact-quality and swing-and-miss metrics. Its combination of velocity, late vertical depth, and sharp glove-side movement allows it to suppress damage while generating elite strikeout rates. The consistency of its performance across all twelve evaluated metrics underscores its dominance relative to other high-usage pitches league-wide. Future work could incorporate pitch-tracking characteristics (e.g. vertical drop, horizontal break) to better understand why Miller’s slider performs at such an exceptional level and how its underlying traits compare to other top-tier MLB pitches.

```{r, echo = FALSE}
write.csv(player_pitch_ranked, file = "player_pitch_ranked.csv")
```


[id]: https://baseballsavant.mlb.com/leaderboard/pitch-arsenal-stats "Baseball Savant Pitch Arsenal Stats Leaderboard (2025)"
