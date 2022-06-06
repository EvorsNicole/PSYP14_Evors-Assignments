


#importing the data

data1 <- read.csv("https://tinyurl.com/yxm5rd89")


#visualization for checking and exploring data

library(ggplot2)
library(tidyverse)


data1 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_boxplot()	


data1 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_boxplot()	



model1 <- lm(pain ~ sex + age, data = data1)
summary(model1)



#check the dataset
#checking influential outliers of the two models


data1 %>% 	
  mutate(rownum = row.names(data1)) %>% 	
  ggplot() +	
  aes(x = pain, y = sex, label = rownum) +	
  geom_label()	


data1 %>% 	
  mutate(rownum = row.names(data1)) %>% 	
  ggplot() +	
  aes(x = pain, y = age, label = rownum) +	
  geom_label()



cooksD <- cooks.distance(model)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential



plot(model1, 4)



#checking normality

shapiro.test(residuals(model1))


residuals_mod = enframe(residuals(model1))	

residuals_mod %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	


#checking linearity

data1 %>% 	
  ggplot() +	
  aes(x = pain, y = age) +	
  geom_point() +	
  geom_smooth(method = "lm")

library(car)

model1 %>% 	
  residualPlots()	


#checking  homoscedasticty

model1 %>% 	
  plot(which = 3)	

model1 %>% 	
  ncvTest() # NCV test	

library(lmtest)

model1 %>% 	
  bptest() # Breush-Pagan test



#checking for homoscedasticity, normality, outliers, and linearity

par(mfrow = c(2, 1))
plot(model1)


#checking  multicollinearity


mod_house_geolocation %>% 	
  vif()	


#the VIF score for the predictor variables are very low under 5, I conclude that there is no multicollinearity



#elimnination of outlied row (row number 88)

library(dplyr)

data1 <- data1 %>% slice(-c(88))




model1 <- lm(pain ~ sex + age, data = data1)
summary(model1)


#normality
shapiro.test(residuals(model1))

#homoscedasticty
model1 %>% 	
  ncvTest() # NCV test	

#multicollinearity

library(car)
model1 %>% 	
  vif()	


#linearity
model1 %>% 	
  residualPlots()	


# I concluded that after the filtration of the outlied row (ID88) the assumption of normality and homogeneity linearity of the residuals are accepted




model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
summary(model2)

#normality

shapiro.test(residuals(model2))
shapiro.test(residuals(model1))

#homoscedasticty

model2 %>% 	
  ncvTest() # NCV test	


model1 %>% 	
  ncvTest() # NCV test	

#multicollinearity

model2 %>% 	
  vif()	

model1 %>% 	
  residualPlots()	


confint(model1)
confint(model1, level = 0.95)

confint(model2, level = 0.95)


#for comparing the two models I used this following code
summary(model1)

anova(model1, model2)

AIC(model1)  # AIC => 576.9372
BIC(model1)  # BIC => 589.2128

AIC(model2)  # AIC => 481.5118
BIC(model2)  # BIC => 506.063

#the model with the lowest AIC and BIC score is better (model2)







############################## Assignment PART 2################################


#importing the DATA

data2 <- read.csv("https://tinyurl.com/87v6emky")

#excluding participant ID 88
data2 <- data2 %>% slice(-c(88))


#linear regression on the backward model

library(tidyverse)
library(caret)
library(leaps)



train.control <- trainControl(method = "cv", number = 10)

step.model <- lm(pain ~ sex +age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data2)
summary(step.model)



step.model$results

step.model$bestTune
#only 4 variables are retained

summary(step.model$finalModel)

#an asterisk specifies that a given variable is included in the  model, the best 4-variables model contains
# (sex; age ; pain_cat; cortisol_serum)

######running new model with retained predictors (sex; age ; pain_cat; cortisol_serum)

#I will run a model with the 4 retained variables it'll be called the backward model


backwardmodel <- lm(pain ~ sex + pain_cat + age + cortisol_serum, 
                    data = data2)


summary(backwardmodel)


#####comparing the initial model (the model submitted to backward regression)
#####and the backward model and report the AIC 

#comparing the two models
anova(step.model, backwardmodel)

AIC(step.model)  # AIC => 535.3447
AIC(backwardmodel)  # AIC => 528.2183

confint(backwardmodel)
####running the "theory-based model"
Tbasedmodel <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data2)

AIC(Tbasedmodel)  # AIC => 530.5249
AIC(backwardmodel)  # AIC => 528.2183


####comparing the backward model and the theory-based model

anova(Tbasedmodel, backwardmodel)

library(performance)
compare_performance(Tbasedmodel, backwardmodel, rank = TRUE)


#Looking at the RSS (in bold) to find out what the best model
#############the best model is the backward model 


AIC(backwardmodel)  # AIC => 528.2183
BIC(backwardmodel)  # BIC => 546.6318

AIC(Tbasedmodel)  # AIC => 530.5249
BIC(Tbasedmodel)  # BIC => 555.0761

#The smaller the AIC, the better the model

#according to the AIC value and the RSS the best model is the backward model

#Loading the new data and we called it Ndata2

Ndata2 <- read.csv("https://tinyurl.com/87v6emky")



#Prediction on the new data with the backward model

#Making a prediction on the new data basing on the two model (backwardmodel and theory based model )

backward_pred <- predict(backwardmodel, newdata = Ndata2)
summary(backward_pred)

# Prediction on the new dataset with the theory based model

theory_pred <- predict(Tbasedmodel, newdata = Ndata2)
summary(theory_pred)


#looking at the scores for the backward model

summary(backward_pred)
summary(theory_pred)

#computing the prediction error, RMSE and the R-square
#between the observed known outcome values and the predicted values by the model
RMSE(backward_pred, Ndata2$pain)
RMSE(theory_pred, Ndata2$pain)

R2(backward_pred, Ndata2$pain)
R2(theory_pred, Ndata2$pain)

#the theory based model is more or less better in terms of pain prediction's than the backward model


################################ The third assignment########################################



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

m2_summary <- summary(model2)
m2_summary$coefficient


####confidence intervals of the coefficients 
confint<- confint(mixedmodel)
confint2<- confint(model2)

#first load in the packages 
require(lme4)
require(MuMIn)


#The marginal R-squared values associated with the fixed effects and
#conditional ones of the (fixed effects plus the random effects)

r2_nakagawa(mixedmodel, by_group = FALSE, tolerance = 1e-05) #or

r.squaredGLMM(mixedmodel)


# R2 for Mixed Models

#Conditional R2: 0.466
#Marginal R2: 0.387

#prediction

predictionDATA4 <-predict(mixedmodel, newdata = data4, allow.new.levels = TRUE)
summary(predictionDATA4)



#Calculating the sum of squared differences the same way as before, but for the model without any predictors = total sum of squared differences

mod_mean<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data3, REML=TRUE)


error_plotter(mod_mean, col = "red", x_var = "height") # visualize error	

TSS = sum((data3$pain - predict(mod_mean))^2)	
TSS	

TSS = sum((data4$pain - predict(mod_mean))^2)	
TSS	


#add up all the residual error (the length of these lines) and get a  measure of the overall efficiency of your model
#residual absolute difference - rarely used
#more commonresidual sum of squared differences (RSS)
#Similar application - gives an indication of the total amount of error when using the model



RAD = sum(abs(data3$pain - predict(mod_mean)))	
RAD	

RSS = sum((data3$pain - predict(mod_mean))^2)	
RSS	



RAD = sum(abs(data4$pain - predict(mod_mean)))	
RAD	






mixedmodel4<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data4, REML=FALSE)



r2_nakagawa(mixedmodel4, by_group = FALSE, tolerance = 1e-05) #or

r.squaredGLMM(mixedmodel4)

#building a new linear mixed effects model with only influential predictor

install.packages("lmerTest")
library(lmerTest)

mixedmodel<- lmer(pain~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data3, REML=TRUE)

summary(mixedmodel)



##the most influential predictor are (age + pain_cat + mindfulness + cortisol_serum)
m1=lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital),data3)



library(effects)
##Bilding model is allowing for both random intercept and random slope

randslop=lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital)+(age+pain_cat+mindfulness+cortisol_serum | hospital),data3)
summary(randslop)

r.squaredGLMM(randslop)


data3$fit <- predict(m1)   #Add model fits to data frame

fitlme =lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital),data3)


newdat.lme = data.frame(hospital = data3$hospital,
                        pain = data3$pain,
                        age = data3$age,
                        pain_cat = data3$pain_cat,
                        mindfulness = data3$mindfulness,
                        cortisol_serum = data3$cortisol_serum)
head(newdat.lme)


newdat.lme$predlme = predict(fitlme, newdata = newdat.lme, level = 0)



##visualising the fitted regression lines for each hospital separately


library(ggplot2)

m1=lmer(pain~age+pain_cat+mindfulness+cortisol_serum+(1|hospital),data3)


data3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line()+		
  facet_wrap( ~ hospital, ncol = 2)+ 
  geom_smooth(method = 'lm', se = F) +  
  geom_smooth(method = 'lm', se = F, aes(group = 1))

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line()+		
  facet_wrap( ~ hospital, ncol = 2)+ 
  geom_smooth(method = 'lm', se = F) +  
  geom_smooth(method = 'lm', se = F, aes(group = 1))


data3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line()+		
  facet_wrap( ~ hospital, ncol = 2)+ 
  geom_smooth(method = 'lm', se = F) +  
  geom_smooth(method = 'lm', se = F, aes(group = 1))

data3 %>% 		
  ggplot() +		
  aes(y = pain, x = age, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line()+		
  facet_wrap( ~ hospital, ncol = 2)+ 
  geom_smooth(method = 'lm', se = F) +  
  geom_smooth(method = 'lm', se = F, aes(group = 1))



