setwd("~/Application Documents/2025_MLB_Player-Pitch_Rankings")
# Loading dataset
player_pitch <- read.csv('pitch-arsenal-stats.csv')

# Removing unnecessary columns
player_pitch_ranked <- subset(player_pitch, select = -c(team_name_alt, player_id, pitch_name, pitches, pitch_usage, pa))

# Unranked DataFrame
head(player_pitch_ranked[,c("last_name..first_name", "pitch_type", "run_value", "slg", "woba", "k_percent", "put_away")],10)

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

# Generating overall performance scores
player_pitch_ranked$overall_performance_score <- rowSums(player_pitch_ranked[, performance_metrics])
player_pitch_ranked %>%
  select(last_name..first_name, pitch_type, overall_performance_score) %>%
  head(10)

library(ggplot2)

ggplot(player_pitch_ranked, aes(x = overall_performance_score)) + 
  geom_histogram(bins = 30, color = 'white', linewidth = 0.3) + 
  labs(
    title = "Distribution of Overall Performance Scores",
    x = "Overall Performance Score",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

# Top 10 Overall Performance Scores
sorted <- player_pitch_ranked %>%
  select(last_name..first_name, pitch_type, overall_performance_score) %>%
  arrange(overall_performance_score)

head(sorted, 10)
# Bottom 10 Overall Performance Scores
tail(sorted, 10)
# Best Player-Pitch (Minimum Overall Performance Score)
best_player_pitch <- player_pitch_ranked %>%
  filter(overall_performance_score == min(overall_performance_score))

cat(
  paste(
    "Best Pitch:", best_player_pitch$last_name..first_name, "-", best_player_pitch$pitch_type)
)

write.csv(player_pitch_ranked, file = "player_pitch_ranked.csv")
