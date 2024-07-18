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
frontal_subset <- c("POper", "PTri", "PreCG", "SLF_I", "SLF_II")
pred_to_zscore <- c("Age", "Post.onset.weeks",
                    "consec_dissimilarity", "distr_dissimilarity", "logfreq", frontal_subset)
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
print("condition coding:")
print(contrasts(pwi_data$Condition))

#==== Model fitting ====
pwi_model <- lmer(data = pwi_data,
                  logRT ~ 
                    POper*(Condition + consec_dissimilarity) + 
                    PTri*(Condition + consec_dissimilarity) + 
                    SLF_I*(Condition + consec_dissimilarity) + 
                    SLF_II*(Condition + consec_dissimilarity) + 
                    PreCG*(Condition + consec_dissimilarity) + 
                    logfreq + Age + Sex + Post.onset.weeks +
                    (1|ID) + (1|Critical.segment),
                  control=lmerControl(optimizer="bobyqa"))

print("variance inflation factors:")
print(car::vif(pwi_model))
print(summary(pwi_model))

# model with continuous target-distractor dissimilarity
print("fitting a model with continuous target-distractor dissimilarity")
pwi_model_distrcont <- lmer(data = pwi_data %>% filter(Condition != "congr"),
                            logRT ~ 
                              POper*(distr_dissimilarity + consec_dissimilarity) + 
                              PTri*(distr_dissimilarity + consec_dissimilarity) + 
                              SLF_I*(distr_dissimilarity + consec_dissimilarity) + 
                              SLF_II*(distr_dissimilarity + consec_dissimilarity) + 
                              PreCG*(distr_dissimilarity + consec_dissimilarity) + 
                              Age + Sex + Post.onset.weeks + logfreq +
                              (1|ID) + (1|Critical.segment),
                            control=lmerControl(optimizer="bobyqa"))

print("variance inflation factors:")
print(car::vif(pwi_model_distrcont))
print(summary(pwi_model_distrcont))
