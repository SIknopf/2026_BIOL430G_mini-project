############################################################
# Music Motor Study Analysis
# Purpose:
# This script creates simple graphs to explore:
# 1. Valence ratings in Active vs Sham conditions
# 2. Arousal ratings in Active vs Sham conditions
# 3. The relationship between Gold-MSI subscales and arousal
############################################################


##############################
# 1. Load required package
##############################

library(ggplot2)


##############################
# 2. Load the dataset
##############################

data <- read.csv("data/processed/music_task_data_participant-level.csv")


##############################
# 3. Preview the data
##############################

# View the first few rows
head(data)

# Check variable names
names(data)


############################################################
# PART A: Valence and Arousal Ratings
# Compare Active vs Sham conditions across stimulus types
############################################################


##############################
# 4. Valence ratings plot
##############################

# This plot shows how each participant's mean valence rating
# changes between the Active and Sham conditions.
# Each panel represents one stimulus type.

ggplot(data, aes(x = condition,
                 y = meanValenceRating,
                 group = participant)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~ stimuliType) +
  labs(
    title = "Participant Changes in Valence Ratings by Condition",
    x = "Condition",
    y = "Mean Valence Rating"
  ) +
  theme_minimal()


##############################
# 5. Arousal ratings plot
##############################

# This plot shows how each participant's mean arousal rating
# changes between the Active and Sham conditions.
# Each panel represents one stimulus type.

ggplot(data, aes(x = condition,
                 y = meanArousalRating,
                 group = participant)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~ stimuliType) +
  labs(
    title = "Participant Changes in Arousal Ratings by Condition",
    x = "Condition",
    y = "Mean Arousal Rating"
  ) +
  theme_minimal()


############################################################
# PART B: Gold-MSI and Arousal Ratings
# Explore whether musical engagement is related to arousal
############################################################


##############################
# 6. Active engagement vs arousal
##############################

# This plot examines whether people with higher
# Gold-MSI Active Engagement scores tend to give
# higher arousal ratings.

ggplot(data, aes(x = GoldMSI_ActiveEngagement_63,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Gold-MSI Active Engagement and Arousal Ratings",
    x = "Gold-MSI Active Engagement",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal()


##############################
# 7. Emotional engagement vs arousal
##############################

# This plot examines whether people with higher
# Gold-MSI Emotional Engagement scores tend to give
# higher arousal ratings.

ggplot(data, aes(x = GoldMSI_Emotions_42,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Gold-MSI Emotional Engagement and Arousal Ratings",
    x = "Gold-MSI Emotional Engagement",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)