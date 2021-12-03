###Assignment_1_ZK

## Assignment PART 1

# Import the data

data1 <- read.csv("https://tinyurl.com/yxm5rd89")

# Load in the libraries (there might be some I did not use in the end)

library(psych) 
library(tidyverse)
library(car)
library(lmnest)
library(sandwich)
library(boot)
library(gridExtra)
library(Metrics)
library(AICcmodavg)
library(lmtest)
library(lme4)

# CLEANING the data

data1 %>% 
  ggplot(aes(STAI_trait)) +
  geom_bar()

# STAI_trait are values <80 and >20 only

data1 <- data1 %>% 
  filter(STAI_trait > 20)

data1 <- data1 %>% 
  filter(STAI_trait < 80)

data1 %>% 
  ggplot(aes(STAI_trait)) +
  geom_bar()

#Pain are values up to 10

data1 %>% 
  ggplot(aes(pain)) +
  geom_bar()

data1 <- data1 %>% 
  filter(pain < 10)

data1 %>% 
  ggplot(aes(pain)) +
  geom_bar()

# Extra:I filtered IQ to keep those participants with IQs over 80 for reliability of the responses on the questionnary

data1 <- data1 %>% 
  filter(IQ > 80)

# Linear regression for model 1

model1 <- lm(pain ~ sex + age, data = data1)
summary(model1)

# Linear regression for model 2

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
summary(model2)

# Checking assumptions of linear regression for model 1

# Histogram normality of residuals

hist(residuals(model1))

# Skew and kurtosis 

describe(residuals(model1))

# Checking for linearity and homoscedasticity of variance

model1 %>%
  plot(which = 3) 

plot(model1)

# Confidence Interval for model 1

confint(model1, level =0.95)

# Assumptions of linear regression for model 2

# Histogram normality of residuals

hist(residuals(model2))

# Skew and kurtosis 

describe(residuals(model2))

# Checking for linearity and homoscedasticity of variance

model2 %>%
  plot(which = 3)

plot(model2)

# Checking for multicollinearity

model2 %>% vif()

#  Confidence Interval for model 1

confint(model2, level =0.95)

#Function ZK code for final regression coeficinets table

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
coef_table(model2) 

summary(model2)

# Extra the AIC for each model

AIC(model1, model2) 

models <- list(model1, model2)
aictab(models)

#Anova for model 1

anova(model1)

#Anova for model 2

anova(model2)


## Assignment PART 2

# Import the DATA

data2 <- read.csv("https://tinyurl.com/87v6emky")

# Linear regression on the backward model

backward_model<- lm(pain ~ sex +age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ, data = data1)
summary(backward_model)

#  Confidence Interval for backward model

confint(backward_model, level =0.95)


# Linear regression on the initial model

theory_based_model <- model2

summary(theory_based_model)

# Calculating the AIC for the two models

AIC(model1, model2)

models2 <- list(backward_model, theory_based_model)

aictab(models2)

# Prediction on the new data with the backward model

backward_pred <- predict(backward_model, newdata = data2)

# Prediction on the new dataset with the theory based model

theory_pred <- predict(theory_based_model, newdata = data2)

# Looking at the scores for the backward model

mean(table(backward_pred, data2$pain))
mae(data2$pain, backward_pred)

# Looking at the scores for the theory based model

mean(table(theory_pred, data2$pain))
mae(data2$pain, theory_pred)



## Assignment PART 3

# Import the DATA

data3 <- read.csv("https://tinyurl.com/b385chpu")
data4 <- read.csv("https://tinyurl.com/4f8thztv")

# Filtering the IQ scores(>80) - same reason

data3 <- data1 %>% 
  filter(IQ > 80)

# Removing the ID variable from data 3

data3 <- data3 %>% 
  select(-ID)

# Linear regression on data 3

model3 <- glm(pain ~., data = data3)

summary(model3)

# Confidence interval for model 3

confint(model3, level = 0.95)

# Predicting values on data 4 with model 3

model3_pred <- predict(model3, newdata = data4)

mae(data4$pain, model3_pred)

# Plot for the predictions

plot(model3_pred, data4$pain)
abline(0,1)

