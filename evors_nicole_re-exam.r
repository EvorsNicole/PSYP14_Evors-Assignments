library(psych) 
library(tidyverse)
library(lmnest)
library(sandwich)
library(boot)
library(gridExtra)
library(Metrics)
library(AICcmodavg)
library(lmtest)
library(lme4)
library(car)
library(performance)
library(see)
library(tidymodels)


# Import the data

data1 <- read.csv("https://tinyurl.com/yxm5rd89")




model1 <- lm(pain ~ sex + age, data = data1)
summary(model1)

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
summary(model2)


confint(model2)

# checking for collinearity
library(performance)

check_collinearity(model1)
check_collinearity(model2)

# OR 

car::vif(model1)
car::vif(model2)


#the VIF score for the predictor variables are very low under 5, we conclude that there is no multicollinearity



############CHECKING for homoscedasticity, normality, outliers, and linearity of the two models
############ 4 Graphs
par(mfrow = c(2, 2))
plot(model1)

par(mfrow = c(2, 2))
plot(model2)

# checking for outliers


#Checking influential outliers of the two models
influenceIndexPlot(model1) 
influenceIndexPlot(model2) 

# Cook's distance
plot(model1, 4)
plot(model2, 4)

check_outliers(model1)
check_outliers(model2)
# 1 outliers detected (cases 88) (model 1 and 2)




#Checking normality of of the two models
check_normality(model1) #or
shapiro.test(residuals(model1))
######Non-normality of residuals is detected (p < .001) (model1)

check_normality(model2) #or
shapiro.test(residuals(model2))
######Non-normality of residuals is detected (p < .001) (model2)



#Checking homoscedasticity of the two models
check_heteroscedasticity(model1)#or
ncvTest(model1)

check_heteroscedasticity(model2)#or
ncvTest(model2)

#### no homoscedasticity (Heteroscedasticity) detected for both models

# Linearity of the data of the two models
plot(model1, 1)
plot(model2, 1)

################ dealing with non normality and heteroscedasticity of residuals 
################ #residuals are not normally distributed and homoscedastic due to the outliers (ID-88) 
################ detected by check_outliers function and Cook's distance 



#deletion of outlied row 88
library(dplyr)

data1 <- data1 %>% slice(-c(88))

model1 <- lm(pain ~ sex + age, data = data1)
summary(model1)
confint(model1)

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
summary(model2)

shapiro.test(residuals(model1))
shapiro.test(residuals(model2))

ncvTest(model1)
ncvTest(model2)


#AFTER THE FILTRATION OF THE OUTLIED row (ID88) the assumption 
#of normality and homogeneity of the residuals are accepted


summary(model1)  
summary(model2)  



#To compare these two models, we used this following code.

anova(model1, model2)

AIC(model1)  # AIC => 576.9372
BIC(model1)  # BIC => 589.2128

AIC(model2)  # AIC => 481.5118
BIC(model2)  # BIC => 506.063

#the model with the lowest AIC and BIC score is preferred (model2)



############################## Assignment PART 2################################


# Importing the DATA

data2 <- read.csv("https://tinyurl.com/87v6emky")

#excluding participant ID 88
data2 <- data2 %>% slice(-c(88))


# Linear regression on the backward model

library(tidyverse)
library(caret)
library(leaps)



train.control <- trainControl(method = "cv", number = 10)

step.model <- train(pain ~ sex +age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data2,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:9),
                    trControl = train.control)
step.model$results

step.model$bestTune
#only 4 variables are retained

summary(step.model$finalModel)

#An asterisk specifies that a given variable is included in the  model, the best 4-variables model contains
# (sex; age ; pain_cat; cortisol_serum)

######## Running new model with retained predictors (sex; age ; pain_cat; cortisol_serum)


backwardmodel <- lm(pain ~ sex + pain_cat + age + cortisol_serum, 
   data = data2)

########Compare the initial model (the model submitted to backward regression) 
#####and the backward model and report the AIC 

anova(submbackwardmod, backwardmodel)

AIC(submbackwardmod)  # AIC => 535.3447
AIC(backwardmodel)  # AIC => 528.2183

confint(submbackwardmod)
######## Running the "theory-based model"
Tbasedmodel <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data2)


####Comparing the backward model and the theory-based model

anova(Tbasedmodel, backwardmodel)

library(performance)
compare_performance(Tbasedmodel, backwardmodel, rank = TRUE)


#Just look at the RSS (in bold) to find out what the best model
#############the best model is the backward model 


AIC(backwardmodel)  # AIC => 528.2183
BIC(backwardmodel)  # BIC => 546.6318

AIC(Tbasedmodel)  # AIC => 530.5249
BIC(Tbasedmodel)  # BIC => 555.0761

#The smaller the AIC, the better the model!

Ndata2 <- read.csv("https://tinyurl.com/87v6emky")



# Prediction on the new data with the backward model

backward_pred <- predict(backwardmodel, newdata = Ndata2)
summary(backward_pred)

# Prediction on the new dataset with the theory based model

theory_pred <- predict(Tbasedmodel, newdata = Ndata2)

# Looking at the scores for the backward model

summary(backward_pred)
summary(theory_pred)

#Compute the prediction error, RMSE and the R-square 
#between the observed known outcome values and the predicted values by the model
RMSE(backward_pred, Ndata2$pain)
RMSE(theory_pred, Ndata2$pain)

R2(backward_pred, Ndata2$pain)
R2(theory_pred, Ndata2$pain)

################################ The third assignment ########################################



data3 <- read.csv("https://tinyurl.com/b385chpu")
data4 <- read.csv("https://tinyurl.com/4f8thztv")


library(ggplot2)
library(lme4)
library(arm)
library(AICcmodavg)


##building a linear mixed model with hospital as random effect
mixedmodel<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data3, REML=TRUE)

####the model coefficients 
m_summary <- summary(mixedmodel)
m_summary$coefficient
##or 
fixef(mixedmodel)

####confidence intervals of the coefficients 
confint<- confint(mixedmodel)

#first load in the packages you want
require(lme4)
require(MuMIn)


#The marginal R squared values associated with thz fixed effects and 
#conditional ones of the (fixed effects plus the random effects)

r2_nakagawa(mixedmodel, by_group = FALSE, tolerance = 1e-05) #or

r.squaredGLMM(mixedmodel)


# R2 for Mixed Models

#Conditional R2: 0.466
#Marginal R2: 0.387

#prediction

predictionDATA4 <-predict(mixedmodel, newdata = data4, allow.new.levels = TRUE)
summary(predictionDATA4)



mixedmodel4<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data4, REML=FALSE)



#Build a new linear mixed effects model with only influential predictor 

install.packages("lmerTest")
library(lmerTest)

mixedmodel<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data3, REML=TRUE)

summary(mixedmodel)

## the most influential predictor are (age + pain_cat + mindfulness + cortisol_serum)
m1=lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital),data3)


##visualising the fitted regression lines for each hospital separately

library(effects)
##Bilding model Allow for both random intercept and random slope

randslop=lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(age+pain_cat+mindfulness+cortisol_serum | hospital),data3)
summary(randslop)

r.squaredGLMM(randslop)


data3$fit <- predict(m1)   #Add model fits to dataframe

fitlme =lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital),data3)


newdat.lme = data.frame(hospital = data3$hospital,
                        pain = data3$pain,
                        age = data3$age,
                        pain_cat = data3$pain_cat,
                        mindfulness = data3$mindfulness,
                        cortisol_serum = data3$cortisol_serum)
head(newdat.lme)


newdat.lme$predlme = predict(fitlme, newdata = newdat.lme, level = 0)




