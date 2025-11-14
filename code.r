`# Load core libraries for analysis and visualization
library(tidyverse)
library(rpart)
library(rpart.plot)

# Load the dataset
file_path <- 'musicX.csv'
df_clean <- readr::read_csv(file_path)


# Create a separate, long-format dataframe for genres
df_long_genres <- df_clean %>%
  select(timestamp, genres, life_satisfaction) %>%
  separate_rows(genres, sep = ";") %>%
  mutate(genres = str_trim(genres)) %>%
  filter(genres!= "")

# Create a separate, long-format dataframe for feelings
df_long_feelings <- df_clean %>%
  select(timestamp, feelings, life_satisfaction) %>%
  separate_rows(feelings, sep = ";") %>%
  mutate(feelings = str_trim(feelings)) %>%
  filter(feelings!= "")


# Define the correct logical order for ordinal factors
hours_levels <- c("less than 1", "1-3 hours", "3-5 hours", "more than 5")
sleep_levels <- c("less than 4 hours", 
                  "5- 6 hours",
                  "7-8 hours", 
                  "8+  hours")
social_levels <- c("Just Me(Lonewolf)", "1 close friends", 
                   "2-3 close friends", "More than 3")

# Define a color palette for visualizations
plot_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Apply new data types and factor levels
df_clean <- df_clean %>%
  mutate(
    life_satisfaction = as.numeric(life_satisfaction),
    
    hours_per_day = factor(hours_per_day, levels = hours_levels, ordered = TRUE),
    sleep = factor(sleep, levels = sleep_levels, ordered = TRUE),
    social_circle = factor(social_circle, levels = social_levels, ordered = TRUE),
    
    platform = as.factor(platform),
    productivity_time = as.factor(productivity_time)
  )
#------------------------------------------------------------------
# Plot 1: Histogram of Life Satisfaction
ggplot(df_clean, aes(x = life_satisfaction)) +
  geom_histogram(aes(y =..density..), binwidth = 1, fill = "#0072B2", color = "white", alpha = 0.7) +
  geom_density(alpha = 0.5, fill = "#D55E00") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Distribution of Student Life Satisfaction",
    x = "Life Satisfaction (1-10)",
    y = "Density"
  ) +
  theme_minimal()

# Plot 2: Bar Chart of Sleep Duration
df_clean %>%
  filter(!is.na(sleep)) %>%
  ggplot(aes(x = fct_infreq(sleep), fill = sleep)) +
  geom_bar(show.legend = FALSE) +
  # Add text labels for the exact counts
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    hjust = -0.2, # Position text to the right of the bar
    size = 3.5
  ) +
  labs(title = "Sleep Duration Distribution", x = "Sleep Habits", y = "Count") +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none") +
  # Expand the y-axis (now horizontal) to make space for labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Plot 3: Bar Chart of Daily Listening Hours
df_clean %>%
  filter(!is.na(hours_per_day)) %>%
  ggplot(aes(x = hours_per_day, fill = hours_per_day)) +
  geom_bar(show.legend = FALSE) +
  # Add text labels for the exact counts
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5, # Position above the bar
    size = 3.5
  ) +
  labs(title = "Daily Music Listening Duration", x = "Hours per Day", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") +
  # Expand y-axis for labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Plot 4: Pie Chart of Top Listening Platforms
platform_data <- df_clean %>%
  count(platform) %>%
  mutate(
    prop = n / sum(n),
    prop_text = scales::percent(prop, accuracy = 1)
  )

ggplot(platform_data, aes(x = "", y = prop, fill = platform)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = prop_text), 
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(values = plot_palette) +
  theme_void() +
  labs(title = "Most Used Listening Platforms", fill = "Platform")

# Plot 5: Top 10 Most Popular Genres
df_long_genres %>%
  count(genres, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(genres, n), y = n, fill = genres)) +
  geom_col(show.legend = FALSE) +
  # Add text labels for the exact counts
  geom_text(
    aes(label = n),
    hjust = -0.2, # Position to the right of the bar
    size = 3.5
  ) +
  scale_fill_viridis_d(option = "C") + # Use a colorful palette
  labs(title = "Top 10 Most Popular Genres", x = "Genre", y = "Number of Listeners") +
  coord_flip() +
  theme_minimal() +
  # Expand y-axis (now horizontal) for labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Plot 6: Life Satisfaction vs. Sleep Duration
df_clean %>%
  filter(!is.na(sleep)) %>%
  ggplot(aes(x = sleep, y = life_satisfaction, fill = sleep)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_viridis_d(option = "E", direction = -1) +
  labs(title = "Life Satisfaction by Sleep Duration", x = "Sleep Habits", y = "Life Satisfaction (1-10)") +
  theme_minimal()

# Plot 7: Life Satisfaction vs. Social Circle
ggplot(df_clean, aes(x = social_circle, y = life_satisfaction, fill = social_circle)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Life Satisfaction by Social Circle", x = "Social Circle", y = "Life Satisfaction (1-10)") +
  theme_minimal()

# Plot 8: Life Satisfaction vs. Daily Listening Hours
ggplot(df_clean, aes(x = hours_per_day, y = life_satisfaction, fill = hours_per_day)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_viridis_d(option = "B") +
  labs(title = "Life Satisfaction by Daily Listening Hours", x = "Hours per Day", y = "Life Satisfaction (1-10)") +
  theme_minimal()

# Create a joined dataframe for genres and feelings
df_genre_feel <- df_clean %>%
  separate_rows(genres, sep = ";") %>%
  separate_rows(feelings, sep = ";") %>%
  mutate(genres = str_trim(genres), feelings = str_trim(feelings)) %>%
  filter(genres!= "" & feelings!= "") %>%
  count(genres, feelings, name = "n") %>%
  group_by(genres) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

# Plot 9: Heatmap of Genre vs. Reported Feeling
ggplot(df_genre_feel, aes(x = genres, y = feelings, fill = proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(proportion, 2)), size = 3, color = "black") +
  scale_fill_viridis_c(option = "plasma") + # Using Viridis palette
  labs(
    title = "Proportion of Feelings Reported by Genre Listeners",
    x = "Genre",
    y = "Reported Feeling",
    fill = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 10: Life Satisfaction by Reported Emotional Effect
ggplot(df_long_feelings, aes(x = reorder(feelings, life_satisfaction, median), y = life_satisfaction, fill = reorder(feelings, life_satisfaction, median))) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_viridis_d() + # Use Viridis palette
  coord_flip() +
  labs(
    title = "Life Satisfaction by Reported Emotional Effect of Music",
    x = "Reported Feeling",
    y = "Life Satisfaction (1-10)"
  ) +
  theme_minimal()

# Plot 11: Heatmap of Sleep vs. Social Circle on Life Satisfaction
df_clean %>%
  # Filter out NA rows to remove them from the heatmap
  filter(!is.na(sleep) &!is.na(social_circle)) %>%
  group_by(sleep, social_circle) %>%
  summarize(Avg_Satisfaction = mean(life_satisfaction, na.rm = TRUE),.groups = 'drop') %>%
  ggplot(aes(x = sleep, y = social_circle, fill = Avg_Satisfaction)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Avg_Satisfaction, 1)), color = "black") +
  scale_fill_viridis_c(direction = 1) + # Viridis palette
  labs(
    title = "Average Life Satisfaction by Sleep and Social Circle",
    x = "Sleep Habits",
    y = "Social Circle",
    fill = "Avg. Satisfaction"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#----------------------------------------------------------------
# Create the model dataframe
df_model <- df_clean %>%
  # Dummify key feelings
  mutate(
    feel_relaxed = as.integer(str_detect(feelings, "Relaxed and calm")),
    feel_energetic = as.integer(str_detect(feelings, "More energetic")),
    feel_emotional = as.integer(str_detect(feelings, "More emotional")),
    feel_focused = as.integer(str_detect(feelings, "Focused"))
  ) %>%
  # Dummify key genres
  mutate(
    genre_rock = as.integer(str_detect(genres, "Rock")),
    genre_pop = as.integer(str_detect(genres, "Pop")),
    genre_sad = as.integer(str_detect(genres, "Sad / Emotional")),
    genre_instrumental = as.integer(str_detect(genres, "Instrumental"))
  ) %>%
  # Select only the variables for modeling
  select(
    life_satisfaction, sleep, social_circle, hours_per_day,
    platform, productivity_time,
    feel_relaxed, feel_energetic, feel_emotional, feel_focused,
    genre_rock, genre_pop, genre_sad, genre_instrumental
  ) %>%
  na.omit() # Remove any rows with missing data to ensure a clean model build

# Set seed for reproducibility and split the data (70% train, 30% test)
set.seed(123)
split <- rsample::initial_split(df_model, prop = 0.7)
train_data <- rsample::training(split)
test_data <- rsample::testing(split)

# Grow a full tree
set.seed(123)
fit_full <- rpart(
  life_satisfaction ~., 
  data = train_data, 
  method = "anova",
  control = rpart.control(cp = 0.001)
)

# Find the optimal CP value
optimal_cp <- fit_full$cptable[which.min(fit_full$cptable[,"xerror"]),"CP"]

# Prune the tree to the optimal size
fit_pruned <- prune(fit_full, cp = optimal_cp)

# Plot 12: Pruned Decision Tree
rpart.plot(
  fit_pruned,
  type = 2, 
  extra = 101, 
  under = TRUE, 
  box.palette = "GnBu", # Changed palette
  fallen.leaves = TRUE,
  main = "Regression Tree for Student Life Satisfaction"
)
#------------------------------------------------
# Extract and plot variable importance
importance_df <- data.frame(
  Variable = names(fit_pruned$variable.importance),
  Importance = fit_pruned$variable.importance
) %>%
  arrange(Importance) %>%
  mutate(Variable = fct_inorder(Variable))

# Plot 13: Variable Importance
ggplot(importance_df, aes(x = Variable, y = Importance, fill = Importance)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "rocket", direction = -1) + # Gradient fill
  coord_flip() +
  labs(
    title = "Predictor Importance for Life Satisfaction",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_minimal()
``