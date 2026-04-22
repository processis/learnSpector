#spotify

library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)
# Load data
data(spotify)

write.csv(spotify, "spotify.csv")

######################333

running_spotify <- spotify %>%
  select(genre, popularity)

# 查看行数（类似示例中的 nrow(running)）
nrow(running_spotify)

# 绘制箱线图：x = genre（音乐类型），y = popularity（流行度）
ggplot(running_spotify, aes(x = genre, y = popularity)) +
  geom_boxplot() +
  labs(title = "Popularity Distribution by Genre",
       x = "Music Genre",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 避免标签重叠


################################




# Load required packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

# Load and prepare data
data(spotify)
spotify <- spotify %>%
  select(artist, title, popularity) %>%
  mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))

# Fit complete pooling model (same as original)
spotify_complete_pooled <- stan_glm(
  popularity ~ 1,
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735
)

# Obtain posterior predictive draws for each artist (using artist_means as newdata)
artist_means <- spotify %>%
  group_by(artist) %>%
  summarise(count = n(), popularity = mean(popularity))

set.seed(84735)
predictions_complete <- posterior_predict(spotify_complete_pooled,
                                          newdata = artist_means)

# Compute 80% predictive intervals (10th and 90th percentiles) for each artist
pred_intervals <- tibble(
  artist = artist_means$artist,
  lower = apply(predictions_complete, 2, quantile, probs = 0.1),
  upper = apply(predictions_complete, 2, quantile, probs = 0.9),
  median = apply(predictions_complete, 2, median)
)

# Create the plot: boxplots of observed song popularities + predictive intervals
# Artist order is already a factor ordered by mean popularity
ggplot() +
  # Boxplots for each artist (original song popularities)
  geom_boxplot(data = spotify,
               aes(x = artist, y = popularity),
               fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  # Overlay posterior predictive intervals (grey error bars)
  geom_errorbar(data = pred_intervals,
                aes(x = artist, ymin = lower, ymax = upper),
                width = 0.3, color = "grey30", linewidth = 0.8) +
  # Optional: add median predictive line
  geom_point(data = pred_intervals,
             aes(x = artist, y = median),
             color = "grey30", size = 1.5) +
  labs(x = "Artist", y = "Song Popularity",
       title = "Complete pooling model: observed popularities (boxplots) vs 80% posterior predictive intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


library(lme4)

data("sleepstudy")

#############################


# Load required packages
library(tidyverse)
library(forcats)

# Load Spotify data
data(spotify, package = "bayesrules")

# Prepare data: keep artist and popularity, reorder artist by mean popularity
spotify_box <- spotify %>%
  select(artist, popularity) %>%
  mutate(artist = fct_reorder(artist, popularity, .fun = mean))

# Create boxplot (only)
ggplot(spotify_box, aes(x = artist, y = popularity)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = 16) +
  labs(x = "Artist", y = "Song Popularity",
       title = "Distribution of song popularity by artist") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

