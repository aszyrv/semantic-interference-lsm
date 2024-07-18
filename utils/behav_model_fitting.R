# ==== Loading packages ====
library(tidyverse)
library(lmerTest)
library(lubridate)

# ==== Reading data ====
pwi_data <- read_csv("pwi_data_clean_withpreds.csv")

demo <- read_csv("data/demo.csv") %>% 
  mutate(Birth.date = dmy(Birth.date),
         Onset.date = dmy(Onset.date),
         Behaviour.date = dmy(Behaviour.date),
         Age = (Birth.date %--% Behaviour.date)/dyears(x = 1),
         Post.onset.weeks = (Onset.date %--% Behaviour.date)/dweeks(x = 1))

pwi_data <- pwi_data %>% 
  left_join(demo %>% select(ID, Age, Sex, Post.onset.weeks))

# z-scoring continuous predictors
pred_to_zscore <- c("Age", "Post.onset.weeks",
                    "consec_dissimilarity", "distr_dissimilarity", "logfreq")
pwi_data[pred_to_zscore] = scale(pwi_data[pred_to_zscore],
                                 center=TRUE, scale=TRUE)
print("z-scoring continuous predictors:")
print(pred_to_zscore)

# sum-coding gender
pwi_data$Sex <- as.factor(pwi_data$Sex)
contrasts(pwi_data$Sex) <- contr.sum(levels(pwi_data$Sex))
print("gender coding:")
print(contrasts(pwi_data$Sex))

# re-leveling the condition variable
pwi_data$Condition <- relevel(as.factor(pwi_data$Condition), "semantic")
print("distractor condition coding")
print(contrasts(pwi_data$Condition))

#==== Model fitting ====
behav_model <- lmer(data = pwi_data,
                    logRT ~ Condition*consec_dissimilarity + logfreq +
                      Age + Sex + Post.onset.weeks +
                      (1|ID) + (1|Critical.segment),
                    control=lmerControl(optimizer="bobyqa"))
print(summary(behav_model))

# model with continuous target-distractor dissimilarity
distr_cont_model <- lmer(data = pwi_data %>% filter(Condition != "congr"),
                         logRT ~ distr_dissimilarity*consec_dissimilarity + logfreq + 
                           Age + Sex + Post.onset.weeks +
                           (1|ID) + (1|Critical.segment),
                         control=lmerControl(optimizer="bobyqa"))
print(summary(distr_cont_model))