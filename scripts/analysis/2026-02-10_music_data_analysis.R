data <- read.csv("music_task_data.csv")

#Delete excluded participants

data_included <- data[ !data$participant %in% c("P09","P26","P28","P29"), ]

#*************Run MANOVA

# Factors are coded properly
data_included$participant <- factor(data_included$participant)
data_included$condition <- factor(data_included$condition, levels = c("Sham", "Active"))
data_included$stimuliType <- factor(data_included$stimuliType)

# 8 rows per participant
table(data_included$participant)

#Do I have duplicates
sum(duplicated(data_included[data_included$participant == "P10", ]))

#Get rid of duplicates
data_included <- data_included[!duplicated(data_included), ]

#Recheck 8 rows per participant
table(data_included$participant)

#Now run MANOVA- independent sham vs active
manova_model <- manova(
  cbind(meanValenceRating, meanArousalRating) ~ condition * stimuliType,
  data = data
)

summary(manova_model, test = "Pillai")

#Valence plot
library(ggplot2)

ggplot(data, aes(x = stimuliType,
                 y = meanValenceRating,
                 color = condition,
                 group = condition)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(title = "Interaction Plot for Valence",
       x = "Stimuli Type",
       y = "Mean Valence Rating") +
  theme_minimal()

#Arousal plot
ggplot(data, aes(x = stimuliType,
                 y = meanArousalRating,
                 color = condition,
                 group = condition)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(title = "Interaction Plot for Arousal",
       x = "Stimuli Type",
       y = "Mean Arousal Rating") +
  theme_minimal()

***
  #Anova
data$set <- as.factor(data$set)
data$condition <- as.factor(data$condition)
data$day <- as.factor(data$day)
shamdata <- subset(data, condition=="Sham")
  anova_model <- aov(meanArousalRating ~ set * stimuliType * day,
    data = shamdata
  )

summary(anova_model)

anova_model_v <- aov(meanValenceRating ~ set * stimuliType * day,
                   data = shamdata
)

summary(anova_model_v)

****
#mixed (between–within) repeated-measures MANOVA- is there an effect between sets

#factors coded properly
  data_included$participant <- factor(data_included$participant)
data_included$set <- factor(data_included$set)        # Set1 vs Set2
data_included$condition <- factor(data_included$condition, 
                                  levels = c("Sham","Active"))
data_included$stimuliType <- factor(data_included$stimuliType)

#Check data- 7 participants (set 1) & 5 participants (set 2)
table(data_included$set, data_included$participant)

#mixed manova
manova_mixed <- manova(
  cbind(meanValenceRating, meanArousalRating) ~ 
    set * condition * stimuliType + 
    Error(participant/(condition*stimuliType)),
  data = data_included
)

summary(manova_mixed, test = "Pillai")

#set x stimuliType PLOTS

library(dplyr)
library(ggplot2)

# participant means (so each person contributes equally)
plot_dat <- data_included %>%
  group_by(participant, set, stimuliType) %>%
  summarise(
    meanValenceRating = mean(meanValenceRating, na.rm = TRUE),
    meanArousalRating = mean(meanArousalRating, na.rm = TRUE),
    .groups = "drop"
  )

# Valence plot
ggplot(plot_dat, aes(x = stimuliType, y = meanValenceRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(x = "Stimuli Type", y = "Mean Valence Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Arousal plot
ggplot(plot_dat, aes(x = stimuliType, y = meanArousalRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(x = "Stimuli Type", y = "Mean Arousal Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#set x StimuliType X condition plots

library(dplyr)

plot_dat3 <- data_included %>%
  group_by(participant, set, condition, stimuliType) %>%
  summarise(
    meanValenceRating = mean(meanValenceRating, na.rm = TRUE),
    meanArousalRating = mean(meanArousalRating, na.rm = TRUE),
    .groups = "drop"
  )

#valence plot
library(ggplot2)

ggplot(plot_dat3, aes(x = stimuliType, y = meanValenceRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  facet_wrap(~ condition) +
  labs(x = "Stimuli Type", y = "Mean Valence Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Arousal plot
ggplot(plot_dat3, aes(x = stimuliType, y = meanArousalRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  facet_wrap(~ condition) +
  labs(x = "Stimuli Type", y = "Mean Arousal Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

## Plots- set x StimuliType X day X condition

library(dplyr)

plot_day <- data_included %>%
  group_by(participant, set, day, condition, stimuliType) %>%
  summarise(
    meanValenceRating = mean(meanValenceRating, na.rm = TRUE),
    meanArousalRating = mean(meanArousalRating, na.rm = TRUE),
    .groups = "drop"
  )

facet_grid(day ~ condition)   # or facet_wrap(~ day + condition)


# Valence plot
ggplot(plot_day, aes(x = stimuliType, y = meanValenceRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  facet_grid(day ~ condition) +
  labs(x = "Stimuli Type", y = "Mean Valence Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Arousal plot
ggplot(plot_day, aes(x = stimuliType, y = meanArousalRating, group = set)) +
  stat_summary(aes(linetype = set), fun = mean, geom = "line") +
  stat_summary(aes(shape = set), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = set), fun.data = mean_se, geom = "errorbar", width = 0.15) +
  facet_grid(day ~ condition) +
  labs(x = "Stimuli Type", y = "Mean Arousal Rating") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

## linear mixed-effects model- set x StimuliType X day plots X condition

library(afex)
library(dplyr)

# Make sure all factors are coded properly
data_anova <- plot_day %>%
  mutate(
    participant = factor(participant),
    set = factor(set),
    day = factor(day),
    condition = factor(condition),
    stimuliType = factor(stimuliType)
  )

with(data_anova, table(day, condition))


#valence
mod_valence <- lmer(
  meanValenceRating ~ set * day * condition * stimuliType +
    (1 | participant),
  data = data_anova,
  REML = FALSE
)

anova(mod_valence, type = 3)

ggsave(filename = "C:/Users/knopf/Downloads/MME_SKn_Project/MME_SKn_Project/ valence_day_plot.png"
, plot= mod_valence
, width = 6
, height = 6)

#There is a significant overall difference in valence ratings between Set 1 and Set 2.
#The difference between Active vs Sham, across Days, depends on the Stimuli Type, and this entire pattern is different for Set 1 versus Set 2.
#Valence ratings differ across stimulus categories

#Arousal
mod_arousal <- lmer(
  meanArousalRating ~ set * day * condition * stimuliType +
    (1 | participant),
  data = data_anova,
  REML = FALSE
)

anova(mod_arousal, type = 3)

#Arousal ratings differ strongly across stimulus categories.
#The effect of condition (Active vs Sham) changes from Day 1 to Day 2.
#Set 1 and Set 2 respond differently to the stimulus categories in terms of arousal.
#The difference between Active vs Sham, across Days, depends on the Stimuli Type.
#Note- no 4 way interaction unlike valence