####################################################### Music Motor Study Analysis ################################################################

data <- read.csv("music_task_data_participant-level.csv")

install.packages("ez")
library(ez)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
install.packages("effectsize")
library(effectsize)
install.packages("renv")

### mean and SD of days between sessions
# M = 13.54
# SD = 19.01

data %>%
  distinct(participant, daysBetween) %>%
  summarise(
    mean_days = mean(daysBetween, na.rm = TRUE),
    sd_days = sd(daysBetween, na.rm = TRUE),
  )

### mean and SD of GoldMSI scales
# Active Engagement (M = 40.77; SD = 8.33)
# Perceptual Ability (M = 44.54; SD = 6.88)
# Musical Training (M = 23.14; SD = 10.69)
# Emotions (M = 33.68; SD = 4.41)
# Singing (M = 28.82; SD = 8.16)
# General (M = 74.68; SD = 16.45)

data %>%
  distinct(participant, .keep_all = TRUE) %>%
  summarise(
    mean_activeEngagement = mean(`GoldMSI_ActiveEngagement_63`, na.rm = TRUE),
    sd_activeEngagement = sd(`GoldMSI_ActiveEngagement_63`, na.rm = TRUE),
    
    mean_perceptual = mean(`GoldMSI_PerceptualAbilities_63`, na.rm = TRUE),
    sd_perceptual = sd(`GoldMSI_PerceptualAbilities_63`, na.rm = TRUE),
    
    mean_training = mean(`GoldMSI_MusicalTraining_49`, na.rm = TRUE),
    sd_training = sd(`GoldMSI_MusicalTraining_49`, na.rm = TRUE),
    
    mean_emotions = mean(`GoldMSI_Emotions_42`, na.rm = TRUE),
    sd_emotions = sd(`GoldMSI_Emotions_42`, na.rm = TRUE),
    
    mean_singing = mean(`GoldMSI_Singing.Abilities_49`, na.rm = TRUE),
    sd_singing = sd(`GoldMSI_Singing.Abilities_49`, na.rm = TRUE),
    
    mean_general = mean(`GoldMSI_GeneralMusicalSophistication_126`, na.rm = TRUE),
    sd_general = sd(`GoldMSI_GeneralMusicalSophistication_126`, na.rm = TRUE)
  )

################################################ Valence and Arousal Ratings VS Active and Sham conditions 
#Look at participant individual change between active and sham for meanValenceRating
ggplot(data, aes(x = condition, y = meanValenceRating, group = participant)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~stimuliType) +
  labs(title = "Individual Differences in Valence Ratings by Condition and Stimulus Type") +
  theme_minimal()

#Look at participant individual change between active and sham for meanArousalRating
ggplot(data, aes(x = condition, y = meanArousalRating, group = participant)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~stimuliType) +
  labs(title = "Individual Differences in Arousal Ratings by Condition and Stimulus Type") +
  theme_minimal()

#Each participant has their own plot meanValenceRating 

ggplot(data, aes(x = condition,
                 y = meanValenceRating,
                 group = stimuliType,
                 color = stimuliType)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ participant) +
  theme_minimal()


#Each participant has their own plot meanArousalRating
ggplot(data, aes(x = condition,
                 y = meanArousalRating,
                 group = stimuliType,
                 color = stimuliType)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ participant) +
  theme_minimal()

#convert to factor 
data$participant <- as.factor(data$participant)
data$condition <- as.factor(data$condition)
data$stimuliType <- as.factor(data$stimuliType)

# Repeated-measures ANOVA examining the effects of condition (Active vs Sham) and stimulus type on mean valence ratings.
# No significant effects were found between condition and stimulus type on mean valence ratings. 
# Results show a significant main effect of stimulus type (p = 6.31e-29, sphericity corrected), suggesting that valence ratings differ across the four emotional stimulus categories.

anova_valence <- ezANOVA(
  data = data,
  dv = meanValenceRating,
  wid = participant,
  within = .(condition, stimuliType),
  type = 3,
  detailed = TRUE
)

anova_valence

### Repeated-measures ANOVA examining the effects of condition (Active vs Sham) and stimulus type on mean arousal ratings.
# No significant effects were found between condition and stimulus type on mean arousal ratings. 
# Results show a significant main effect of stimulus type (p = 2.35e-28, sphericity corrected), suggesting that arousal ratings differ across the four emotional stimulus categories.

anova_arousal <- ezANOVA(
  data = data,
  dv = meanArousalRating,
  wid = participant,
  within = .(condition, stimuliType),
  type = 3,
  detailed = TRUE
)

anova_arousal


### Effect size for condition and stimulus type for meanValenceRating
# Active stimulation did not meaningfully change valence ratings compared to Sham.
# High Valence – Low Arousal (d = 0.37) small-to-moderate effect Active produced higher ratings than sham
# Low Valence – Low Arousal (d = 0.23) small effect Active produced higher ratings than sham 
# Low Valence – High Arousal (d = -0.15) very small effect Active produced lower ratings than sham
# High Valence – High Arousal (d = 0.01) no effect

cohens_d(meanValenceRating ~ condition, data = data)

data %>%
  group_by(stimuliType) %>%
  summarise(
    d = cohens_d(meanValenceRating ~ condition)$Cohens_d
  )

### Effect size for condition and stimulus type for meanArousalRating
# Active stimulation did not meaningfully change arousal ratings compared to Sham stimulation.
# High Valence – Low Arousal (d = -0.225) small effect Active produced lower ratings than sham
# Low Valence – High Arousal (d = 0.145) very small effect Active produced higher ratings than sham 
# Low Valence – Low Arousal (d = 0.065) no effect
# High Valence – High Arousal (d = 0.023) no effect

cohens_d(meanArousalRating ~ condition, data = data)

data %>%
  group_by(stimuliType) %>%
  summarise(
    d = cohens_d(meanArousalRating ~ condition)$Cohens_d
  )

### Z Scores: Does condition change valence and arousal ratings in SD units? 
#Valence model
# Active stimulation did not significantly change standardized mean valence ratings compared to sham. 
# Results show a significant main effect of stimulus type (p = 7.91e-29, sphericity corrected), suggesting that standardized valence ratings differed across the four emotional stimulus categories.
anova_valence_z <- ezANOVA(
  data = data,
  dv = meanZValenceRating,
  wid = participant,
  within = .(condition, stimuliType),
  type = 3,
  detailed = TRUE
)

anova_valence_z

#plot 
ggplot(data, aes(x = stimuliType, y = meanZValenceRating, fill = condition)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = .2) +
  ylab("Standardized Valence (z-score)") +
  theme_minimal()

#Arousal model
# Active stimulation did not significantly change standardized mean arousal ratings compared to sham. 
# Results show a significant main effect of stimulus type (p = 2.32e-26, sphericity corrected), suggesting that standardized arousal ratings differed across the four emotional stimulus categories.
anova_arousal_z <- ezANOVA(
  data = data,
  dv = meanZArousalRating,
  wid = participant,
  within = .(condition, stimuliType),
  type = 3,
  detailed = TRUE
)

anova_arousal_z

#plot
ggplot(data, aes(x = stimuliType, y = meanZArousalRating, fill = condition)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = .2) +
  ylab("Standardized Arousal (z-score)") +
  theme_minimal()

##################################################### Valence and Arousal Ratings VS Reaction Time 
#Look at participant individual relationship between RT and meanValenceRating
ggplot(data, aes(x = meanValenceRating,
                 y = meanValenceRT,
                 color = stimuliType,
                 group = stimuliType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#Look at participant individual relationship between RT and meanArousalRating
ggplot(data, aes(x = meanArousalRating,
                 y = meanArousalRT,
                 color = stimuliType,
                 group = stimuliType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#Each participant has their own plot meanValenceRating vs RT

ggplot(data, aes(x = meanValenceRating,
                 y = meanValenceRT,
                 color = stimuliType,
                 group = stimuliType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ participant) +
  theme_minimal()

#Each participant has their own plot meanArousalRating vs RT
ggplot(data, aes(x = meanArousalRating,
                 y = meanArousalRT,
                 color = stimuliType,
                 group = stimuliType)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ participant) +
  theme_minimal()

###Does reaction time predict changes in meanValenceRating or meanArousalRating, and does this differ across the four stimulus types?
# Linear mixed-effects models were used to assess whether reaction time predicts
# mean valence and arousal ratings, and whether this relationship differs across
# the four stimulus types. High Valence – High Arousal was treated as the reference
# category. The results indicated that neither stimulus type nor the interaction
# between stimulus type and reaction time was significant, suggesting that reaction
# time did not differentially predict valence or arousal ratings across the stimulus
# categories.

#Valence
model_valence_rt <- lmer(meanValenceRating ~ meanValenceRT * stimuliType + (1 | participant), 
                      data = data)

summary(model_valence_rt)

#Arousal
model_arousal_rt <- lmer(meanArousalRating ~ meanArousalRT * stimuliType + (1 | participant), 
                      data = data)

summary(model_arousal)

############################################################################ Condition VS Reaction Time

#Look at participant individual relationship between condition and meanValenceRT
ggplot(data, aes(x = condition,
                 y = meanValenceRT,
                 group = interaction(participant, stimuliType),
                 color = stimuliType)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Valence Reaction Time by Condition and Stimulus Type",
    x = "Condition",
    y = "Mean Valence RT",
    color = "Stimulus Type"
  )
#Look at participant individual relationship between condition and meanArousalRT
ggplot(data, aes(x = condition,
                 y = meanArousalRT,
                 group = interaction(participant, stimuliType),
                 color = stimuliType)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Arousal Reaction Time by Condition and Stimulus Type",
    x = "Condition",
    y = "Mean Arousal RT",
    color = "Stimulus Type"
  )

#Each participant has their own plot condition vs. meanValenceRT
ggplot(data, aes(x = condition,
                 y = meanValenceRT,
                 group = stimuliType,
                 color = stimuliType)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~ participant) +
  theme_minimal() +
  labs(
    title = "Valence RT by Condition and Stimulus Type",
    x = "Condition",
    y = "Mean Valence RT",
    color = "Stimulus Type"
  )

##Each participant has their own plot condition vs. meanArousalRT
ggplot(data, aes(x = condition,
                 y = meanArousalRT,
                 group = stimuliType,
                 color = stimuliType)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~ participant) +
  theme_minimal() +
  labs(
    title = "Arousal RT by Condition and Stimulus Type",
    x = "Condition",
    y = "Mean Arousal RT",
    color = "Stimulus Type"
  )

###Does condition (Active vs Sham) predict reaction time, and does that effect differ across the four stimulus types?
#A linear mixed-effects model examined whether stimulation condition and stimulus category predicted reaction time. Active stimulation and the High Valence–High Arousal category were treated as the reference levels. No significant main effects of condition or stimulus type were observed, and no significant condition × stimulus type interactions emerged, suggesting that reaction time did not differ meaningfully between Active and Sham conditions or across stimulus categories.
#Valence
model_condition_valencert <- lmer(meanValenceRT ~ condition * stimuliType +
                   (1 | participant),
                 data = data)

summary(model_condition_valencert)

#Arousal
model_condition_arousalrt <- lmer(meanArousalRT ~ condition * stimuliType +
                                    (1 | participant),
                                  data = data)

summary(model_condition_arousalrt)

#################################################################################################### Gold MSI Scales & Musical Ratings 

########## Active Engagement influence on stimuli type ratings

###Valence model
#Active engagement does not influence how people rate different stimulus type
model_valence_engagement <- lmer(meanValenceRating ~ GoldMSI_ActiveEngagement_63 * stimuliType + 
                        (1 | participant), 
                      data = data)

summary(model_valence_engagement)
anova(model_valence_engagement)

#plot
library(ggplot2)

ggplot(data, aes(x = GoldMSI_ActiveEngagement_63,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "GoldMSI Active Engagement Predicting Valence Ratings",
    x = "GoldMSI Active Engagement",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal()

###Arousal model
# Active engagement significantly predicts arousal ratings for Low Valence – Low Arousal stimuli (p = .004)
model_arousal_engagement <- lmer(meanArousalRating ~ GoldMSI_ActiveEngagement_63 * stimuliType + 
                        (1 | participant), 
                      data = data)

summary(model_arousal_engagement)
anova(model_arousal_engagement)

#Plot
ggplot(data, aes(x = GoldMSI_ActiveEngagement_63,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "GoldMSI Active Engagement Predicting Arousal Ratings",
    x = "GoldMSI Active Engagement",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal()

################### Active Engagement influence on the relationship between condition and stimuli type ratings

###Valence model
# Active engagement does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_engagement <- lmer(
  meanValenceRating ~ GoldMSI_ActiveEngagement_63 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_engagement)
anova(model_valence_condition_engagement)

###Arousal model
# Active engagement does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_engagement <- lmer(
  meanArousalRating ~ GoldMSI_ActiveEngagement_63 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_engagement)
anova(model_arousal_condition_engagement)

############################### Perceptual Ability influence on stimuli type ratings

###Valence model
#Perceptual Ability does not influence how people rate different stimulus type
model_valence_perceptual <- lmer(meanValenceRating ~ GoldMSI_PerceptualAbilities_63 * stimuliType + 
                                   (1 | participant), 
                                 data = data)

summary(model_valence_perceptual)
anova(model_valence_perceptual)

#Plot 
ggplot(data, aes(x = GoldMSI_PerceptualAbilities_63,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Perceptual Abilities and Valence Ratings",
    x = "GoldMSI Perceptual Abilities",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

###Arousal model
#Perceptual Ability does not influence how people rate different stimulus type
model_arousal_perceptual <- lmer(meanArousalRating ~ GoldMSI_PerceptualAbilities_63 * stimuliType + 
                                   (1 | participant), 
                                 data = data)

summary(model_arousal_perceptual)
anova(model_arousal_perceptual)

#Plot
ggplot(data, aes(x = GoldMSI_PerceptualAbilities_63,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Perceptual Abilities and Arousal Ratings",
    x = "GoldMSI Perceptual Abilities",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

################### Perceptual ability influence on the relationship between condition and stimuli type ratings

###Valence model
#Perceptual Ability does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_perceptual <- lmer(
  meanValenceRating ~ GoldMSI_PerceptualAbilities_63 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_perceptual)
anova(model_valence_condition_perceptual)

###Arousal model
#Perceptual Ability does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_perceptual <- lmer(
  meanArousalRating ~ GoldMSI_PerceptualAbilities_63 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_perceptual)
anova(model_arousal_condition_perceptual)

#################################### Musical Training influence on stimuli type ratings

###Valence model
#Musical Training significantly predicts valence ratings for High Valence – Low Arousal stimuli (p = .034)
# HOWEVER the whole model is not significant 
model_valence_training <- lmer(meanValenceRating ~ GoldMSI_MusicalTraining_49 * stimuliType + 
                                   (1 | participant), 
                                 data = data)

summary(model_valence_training)
anova(model_valence_training)

#plot 
ggplot(data, aes(x = GoldMSI_MusicalTraining_49,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Musical Training and Valence Ratings",
    x = "GoldMSI Musical Training",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)


###Arousal model
#Musical Training does not influence how people rate different stimulus type
model_arousal_training <- lmer(meanArousalRating ~ GoldMSI_MusicalTraining_49 * stimuliType + 
                                   (1 | participant), 
                                 data = data)

summary(model_arousal_training)
anova(model_arousal_training)

#Plot 
ggplot(data, aes(x = GoldMSI_MusicalTraining_49,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Musical Training and Arousal Ratings",
    x = "GoldMSI Musical Training",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

################### Musical Training influence on the relationship between condition and stimuli type ratings

###Valence model
#Musical Training does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_training <- lmer(
  meanValenceRating ~ GoldMSI_MusicalTraining_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_training)
anova(model_valence_condition_training)

###Arousal model
#Musical Training does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_perceptual <- lmer(
  meanArousalRating ~ GoldMSI_MusicalTraining_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_perceptual)
anova(model_arousal_condition_perceptual)

###################################################### Emotions influence on stimuli type ratings

###Valence model
#Emotions does not influence how people rate different stimulus type
model_valence_emotions <- lmer(meanValenceRating ~ GoldMSI_Emotions_42 * stimuliType + 
                                 (1 | participant), 
                               data = data)

summary(model_valence_emotions)
anova(model_valence_emotions)

#plot 
ggplot(data, aes(x = GoldMSI_Emotions_42,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Emotions and Valence Ratings",
    x = "GoldMSI Emotions",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

###Arousal model
#Emotions predicts arousal ratings for Low Valence - Low Arousal stimuli (p = .005)
model_arousal_emotions <- lmer(meanArousalRating ~ GoldMSI_Emotions_42 * stimuliType + 
                                 (1 | participant), 
                               data = data)

summary(model_arousal_emotions)
anova(model_arousal_emotions)

#plot 
ggplot(data, aes(x = GoldMSI_Emotions_42,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Emotions and Arousal Ratings",
    x = "GoldMSI Emotional Engagement",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

################### Emotions influence on the relationship between condition and stimuli type ratings

###Valence model
#Emotions does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_training <- lmer(
  meanValenceRating ~ GoldMSI_MusicalTraining_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_training)
anova(model_valence_condition_training)

###Arousal model
#Emotions does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_perceptual <- lmer(
  meanArousalRating ~ GoldMSI_MusicalTraining_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_perceptual)
anova(model_arousal_condition_perceptual)


####################### Singing Abilities influence on stimuli type ratings

###Valence model
#Singing Abilities does not influence how people rate different stimulus type
model_valence_singing <- lmer(meanValenceRating ~ GoldMSI_SingingAbilities_49 * stimuliType + 
                                 (1 | participant), 
                               data = data)

summary(model_valence_singing)
anova(model_valence_singing)

#plot 
ggplot(data, aes(x = GoldMSI_SingingAbilities_49,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Singing Abilities and Valence Ratings",
    x = "GoldMSI Singing Abilities",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

###Arousal model
#Singing Abilities does not influence how people rate different stimulus type
model_arousal_singing <- lmer(meanArousalRating ~ GoldMSI_SingingAbilities_49 * stimuliType + 
                                 (1 | participant), 
                               data = data)

summary(model_arousal_singing)
anova(model_arousal_singing)

#plot
ggplot(data, aes(x = GoldMSI_SingingAbilities_49,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Singing Abilities and Arousal Ratings",
    x = "GoldMSI Singing Abilities",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)


################### Singing Ability influence on the relationship between condition and stimuli type ratings

###Valence model
#Singing Ability does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_singing <- lmer(
  meanValenceRating ~ GoldMSI_SingingAbilities_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_singing)
anova(model_valence_condition_singing)

###Arousal model
#Singing Ability does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_singing <- lmer(
  meanArousalRating ~ GoldMSI_SingingAbilities_49 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_singing)
anova(model_arousal_condition_singing)

#################### General Musical Sophistication influence on stimuli type ratings

###Valence model
#General Musical Sophistication does not influence how people rate different stimulus type
model_valence_general <- lmer(meanValenceRating ~ GoldMSI_GeneralMusicalSophistication_126 * stimuliType + 
                                (1 | participant), 
                              data = data)

summary(model_valence_general)
anova(model_valence_general)

#plot
ggplot(data, aes(x = GoldMSI_GeneralMusicalSophistication_126,
                 y = meanValenceRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "General Musical Sophistication and Valence Ratings",
    x = "GoldMSI General Musical Sophistication",
    y = "Mean Valence Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

###Arousal model
#General Musical Sophistication significantly predicts arousal ratings for Low Valence – Low Arousal stimuli (p = .0289)
# HOWEVER the whole model is not significant 
model_arousal_general <- lmer(meanArousalRating ~ GoldMSI_GeneralMusicalSophistication_126 * stimuliType + 
                                (1 | participant), 
                              data = data)

summary(model_arousal_general)
anova(model_arousal_general)

#plot 
ggplot(data, aes(x = GoldMSI_GeneralMusicalSophistication_126,
                 y = meanArousalRating,
                 color = stimuliType)) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "General Musical Sophistication and Arousal Ratings",
    x = "GoldMSI General Musical Sophistication",
    y = "Mean Arousal Rating",
    color = "Stimulus Type"
  ) +
  theme_minimal(base_size = 13)

################### General Musical Sophistication influence on the relationship between condition and stimuli type ratings

###Valence model
#General Musical Sophistication does not influence the relationship between condition and mean valence stimulus type ratings
model_valence_condition_general <- lmer(
  meanValenceRating ~ GoldMSI_GeneralMusicalSophistication_126 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_valence_condition_general)
anova(model_valence_condition_general)

###Arousal model
#General Musical Sophistication does not influence the relationship between condition and mean arousal stimulus type ratings
model_arousal_condition_general <- lmer(
  meanArousalRating ~ GoldMSI_GeneralMusicalSophistication_126 * condition * stimuliType +
    (1 | participant),
  data = data
)
summary(model_arousal_condition_general)
anova(model_arousal_condition_general)

############################################################################ KVIQ VS Valence and Arousal Rating
###Do KVIQ scores moderate the relationship between condition and stimulus type rating 
#Valence model
# KVIQ scores do not influence the relationship between condition and mean Valence rating of stimulus type rating 
model_valence_KVIQ <- lmer(
  meanValenceRating ~ condition * stimuliType *
    KVIQ_Kinesthetic_Score +
    KVIQ_Visual_Score +
    (1 | participant),
  data = data
)
summary(model_valence_KVIQ)
anova(model_valence_KVIQ)

#Arousal model
# KVIQ scores do not influence the relationship between condition and mean Arousal rating of stimulus type rating 
model_arousal_KVIQ <- lmer(
  meanArousalRating ~ condition * stimuliType *
    KVIQ_Kinesthetic_Score +
    KVIQ_Visual_Score +
    (1 | participant),
  data = data
)
summary(model_arousal_KVIQ)
anova(model_arousal_KVIQ)
