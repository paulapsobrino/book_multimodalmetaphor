##Paula P Sobrino
##13-14 June 2017
## New regressions for book "Multimodal Metaphor & Metonymy in Advertising": Chapter 8

***********************************************

## 1.a. FIGURATIVE COMPLEXITY - REACTION TIME?

read.csv("experiment.csv")

experiment = read.csv("experiment.csv")

summary(lm(Fig_complexity~Reaction, data=experiment))#BAAAAAD. 778 degrees of freedom, but the study only has 30 subjects

table (experiment$Participant) #only 30 subjects

aggregate(Reaction~Participant, data=experiment, mean) #this is the avergae RT by subject


#This gets rid of values over 4000, which were not recorded
> plot(density(experiment[experiment$Reaction<4000,]$Reaction))
> experiment2=experiment[experiment$Reaction<4000,]

#this gets rid of one negative value that should not be there
experiment2=experiment[experiment$Reaction>0 & experiment$Reaction<4000,]

#Remember: Transform RT by using 1/Reaction
#Some other possibilities, check which distribution looks more normal (Thanks Florent Perek)
> experiment2=experiment[experiment$Reaction>0 & experiment$Reaction<4000,]
> plot(density(experiment2$Reaction))
> plot(density(log(experiment2$Reaction),na.rm=T))
> plot(density(1/log(experiment2$Reaction),na.rm=T)
       + )
> plot(density(1/experiment2$Reaction))

#Distribution looks already normal with Reaction, no need to transform

#lets build the mixed model
library(lme4)
library(lmerTest)
xmdl=lmer (Reaction~Fig_complexity +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Reaction~Fig_complexity +
                (1|Participant) + (1|ID_advert), data = experiment2)  )

#look at the table of fixed effects - it doesnt have p values! you can use t values that are <>2
#random effects per subject means that every person is allowed to have a different intercept

summary(xmdl) #this is the good model with two random effects

fixef(xmdl) #output from the table - estimated fixed effects

hist(residuals(xmdl))
qqnorm (residuals(xmdl))
qqline (residuals(xmdl))

plot(fitted (xmdl), residuals (xmdl)) #good, there arent any patterns - otherwise, it would be a case of heteroskedasticity

#for p values

xmdl.null=lmer (Reaction~1 +
                  (1|Participant) + (1|ID_advert), data = experiment2) #create a null model
anova(xmodel.null, xmodel, test="Chisq")#compare the null model agaisnt the full model
barplot(experiment2$Fig_complexity, experiment2$1/Reaction)

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(xmdl)

## 1.b. REACTION TIME - OTHER VARIABLES?

read.csv("experiment.csv")

experiment = read.csv("experiment.csv")

##Now i want to know if there are other variables different from Figurative complexity, with an effect on RT           
xmdl=lmer (Reaction~Effectiveness + Saliency_interpr + Number_int +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Reaction~Effectiveness + Saliency_interpr + Number_int +
                (1|Participant) + (1|ID_advert), data = experiment2)  )           
r.squaredGLMM(xmdl)

##Now I am only interested in RT~Effectiveness
xmdl=lmer (Reaction~Effectiveness +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Reaction~Effectiveness +
                (1|Participant) + (1|ID_advert), data = experiment2)  )    

##Now I am only interested in RT~N interpretations
xmdl=lmer (Reaction~Number_int +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Reaction~Number_int +
                (1|Participant) + (1|ID_advert), data = experiment2)  )  

#now I aggregate results to draw a bar plot of effectiveness & RT
aggregate(Reaction~Effectiveness, data=experiment, mean) #this is the avergae RT by subject
           
***********************************************
  
  ## 2. FIGURATIVE COMPLEXITY - EFFECTIVENESS?
  
  xmdl=lmer (Effectiveness~Fig_complexity +
               (1|Participant) + (1|ID_advert), data = experiment) 
summary(lmer (Effectiveness~Fig_complexity +
                (1|Participant) + (1|ID_advert), data = experiment)  )

#look at the table of fixed effects - it doesnt have p values! you can use t values that are <>2
#random effects per subject means that every person is allowed to have a different intercept

summary(xmdl) #this is the good model with two random effects

fixef(xmdl) #output from the table - estimated fixed effects

hist(residuals(xmdl))
qqnorm (residuals(xmdl))
qqline (residuals(xmdl))

plot(fitted (xmdl), residuals (xmdl)) #good, there arent any patterns - otherwise, it would be a case of heteroskedasticity

#for p values

xmdl.null=lmer (Reaction~1 +
                  (1|Participant) + (1|ID_advert), data = experiment) #create a null model
anova(xmodel.null, xmodel, test="Chisq")#compare the null model agaisnt the full model
barplot(experiment$Fig_complexity, experiment$Effectiveness)

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(xmdl)

aggregate(Effectiveness~Fig_complexity, data=experiment, mean) #this is the avergae effectiveness by Fig language type
  ***********************************************
  
  ## 3. FIGURATIVE COMPLEXITY - NUMBER OF INTERPRETATIONS?

 #lets build the model 
  xmdl=lmer (Number_int~Fig_complexity +
               (1|Participant) + (1|ID_advert), data = experiment) 
summary(lmer (Number_int~Fig_complexity +
                (1|Participant) + (1|ID_advert), data = experiment)  )

summary(xmdl) #this is the good model with two random effects

fixef(xmdl) #output from the table - estimated fixed effects

hist(residuals(xmdl))
qqnorm (residuals(xmdl))
qqline (residuals(xmdl))

plot(fitted (xmdl), residuals (xmdl)) #good, there arent any patterns - otherwise, it would be a case of heteroskedasticity

#for p values

xmdl.null=lmer (Reaction~1 +
                  (1|Participant) + (1|ID_advert), data = experiment) #create a null model
anova(xmodel.null, xmodel, test="Chisq")#compare the null model agaisnt the full model
barplot(experiment$Fig_complexity, experiment$Effectiveness)

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(xmdl)

aggregate(Number_int~Fig_complexity, data=experiment, mean) #this is the avergae number of interpretations by subject


## 3.2. NUMBER OF INTERPRETATIONS - EFFECTIVENESS?


xmdl=lmer (Number_int~Effectiveness +
             (1|Participant) + (1|ID_advert), data = experiment) 
summary(lmer (Number_int~Effectiveness +
                (1|Participant) + (1|ID_advert), data = experiment)  )

summary(xmdl) #this is the good model with two random effects

fixef(xmdl) #output from the table - estimated fixed effects

hist(residuals(xmdl))
qqnorm (residuals(xmdl))
qqline (residuals(xmdl))

plot(fitted (xmdl), residuals (xmdl)) #good, there arent any patterns - otherwise, it would be a case of heteroskedasticity

#for p values

xmdl.null=lmer (Number_int~1 +
                  (1|Participant) + (1|ID_advert), data = experiment) #create a null model
anova(xmodel.null, xmodel, test="Chisq")#compare the null model agaisnt the full model
barplot(experiment$Number_int, experiment$Effectiveness)

##now I want to find out the multple R square
library(MuMIn)
r.squaredGLMM(xmdl)

aggregate(Number_int~Effectiveness, data=experiment, mean) #this is the avergae number of interpretations by subject

******************************
  
  ##### 4. INTERACTIONS BETWEEN NATIONALITY AND THE VARIABLES DISCUSSED ABOVE
  
  ##4.0 RT ~ Nationality
  xmdl=lmer (Reaction~Nationality +
               (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (1/log(Reaction)~Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )
experiment2$Nationality = factor(experiment2$Nationality)
  aggregate(Reaction~Nationality, data=experiment2, mean) #this is the avergae number of interpretations by subject

  
  
  ### 4.1. RT ~ FIG COMPLEXITY * NATIONALITY
  
  xmdl=lmer (Reaction~Fig_complexity*Nationality +
               (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (1/log(Reaction)~Fig_complexity*Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )
experiment$Nationality = factor(experiment$Nationality)

### 4.2. RT ~ EFFECTIVENESS * NATIONALITY

xmdl=lmer (Reaction~Effectiveness*Nationality +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (1/log(Reaction)~Effectiveness*Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )

##4.3 Effectiveness ~ Nationality
xmdl=lmer (Effectiveness~Nationality +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Effectiveness~Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )
aggregate(Effectiveness~Nationality, data=experiment2, mean) #this is the avergae number of interpretations by subject



### 4.4. Effectiveness ~ FIG COMPLEXITY * NATIONALITY

xmdl=lmer (Effectiveness~Fig_complexity*Nationality +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Effectiveness~Fig_complexity*Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )

##4.5 Number of interpretations ~ Nationality
xmdl=lmer (Number_int~Nationality +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Number_int~Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )
aggregate(Number_int~Nationality, data=experiment2, mean) #this is the avergae number of interpretations by subject



### 4.6. Number of interpretations ~ FIG COMPLEXITY * NATIONALITY

xmdl=lmer (Number_int~Fig_complexity*Nationality +
             (1|Participant) + (1|ID_advert), data = experiment2) 
summary(lmer (Number_int~Fig_complexity*Nationality +
                (1|Participant) + (1|ID_advert), data = experiment2)  )

