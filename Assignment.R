# Assignment
#Question 1
surg=read.table("surg.dat",header=TRUE) #read table

surg$Blood=surg$blood #add copy of column blood
surg$blood=surg$survival #swap column survival to blood
colnames(surg)[1]="Survival" #Rename column
surg$blood <- NULL #Remove one of the blood column
newNames=c("Survival","Prognosis","Enzyme","Liver","Age","Gender","Blood")
colnames(surg)=newNames #Change column name

#a) Checking scatterplot matrix

mycols=c("blue","red")[as.factor(surg$gender)] #Put color for gender factor 
levels(as.factor(surg$gender)) #checking level of gender, so we have blue=F and red=M
pairs(surg[,-6],panel=panel.smooth,col=mycols) #scatterplot with smoother, plot data without gender

#Gender variable should be removed because it is a categorical variable. Gender is not a continuous variable. It is an independent variable and has no role to impact dependent variable. As the correlation matrix is numeric summary, the categorical variable should be removed for the correlation matrix computation.
#Scatterplot comment: Survival is correlated with prognosis, enzyme, liver, age and blood but each of the predictors is correlated with the others. No unusual observations. Similar spread

#b. Compute the correlation matrix
cor(surg[,-6]) #Compute correlation matrix without gender variable
#Comment: the diagonals are one (variable has perfect correlation with itself), The off diagonals contain the correlation between different variables

#c. Fit model to explain the relationship between the response Survival and other predictors
#Model for multiple Regression

# Hypothesis for Overall ANOVA 
#H0 : β1 = β2 =β3=β4=β5 = 0, H1 : βi>< 0 for at least one i (not all βi parameters are zero)
surg.lm=lm(Survival ~ Prognosis + Enzyme + Liver + Age + Gender + Blood, data=surg)
anova(surg.lm)

pf(17.8487,6,47,lower.tail=FALSE)

#d) 
qqnorm(surg.lm$residuals, main = "Normal Q-Q plot of residuals", col=mycols)
plot(surg.lm$fitted, surg.lm$residuals, main = "Residuals vs Fitted",
     xlab = "Fitted", ylab = "Residuals", col=mycols)
#e) 
#Question 2
kml=read.table("kml.dat",header=TRUE)
#a) 
with(kml,table(driver,car))
#The design of the study is balanced. Because there are same number of observations in all cells and for all possible pairs of factor levels for factors driver (labelled A, B, C and D) and car (labelled one, two, three, four and five). 

#b) Preliminary investigation
with(kml, interaction.plot(driver, car, kmL, 
                           trace.label = "Specific Car",
                           xlab = "Driver of the car", ylab = "Efficiency of car in km/L", col = 1:3))
with(kml, interaction.plot(car, driver, kmL, 
                           trace.label = "Driver of the car",
                           xlab = "Specific Car", ylab = "Efficiency of car in km/L", col = 1:3))
#Result: The lines are parallel, so there is no interaction between Factor driver and Factor car. Factor car has a constant effect on the efficiency of car in km/L (irrespective  of Factor A)
#Boxplot
boxplot(kmL ~ driver + car, data = kml, main="Efficiency of car in km/L")
#Result: box plots spread with inequal sizes 
#c) Group tests
#Model Yijk = µ + αi + βj + γij + εijk
#There are three types of tests
#1. Interaction: H0 : γij = 0 for all i, j; HA : not all γij = 0
#2. Main effect A: H0 : αi = 0 for all i; HA : not all αi = 0
#3. Main effect B: H0 : βj = 0 for all j; HA : not all βj = 0
#Fit full model with interaction:
kml.1 = lm(kmL ~ car * driver, data = kml)
summary(kml.1)$coefficients

#Fit reduced model without interaction (only main effects)
kml.2 = update(kml.1, . ~ . - car:driver)
## F-test for interaction terms
#ANOVA table for the full model 
anova(kml.1)
n = nrow(kml)
p = kml.1$rank
c(p,n)
#Model Y = µ + αi + βj + γij + ε
#Hypotheses: H0 : γij = 0, H1 : at least one γij non-zero
#P-Value = 0.3715 > 0.05
#The interaction is not significant ⇒ fit reduced model with main effects only

## Fit model with just main effects
anova(kml.2)
#Main Effects: Driver
#Model Y = µ + αi + βj + ε
#Hypotheses: H0 : βj = 0 against H1 : at least one βj non-zero
#P-Value = 2.2e-16 < 0.05
#Driver type is significant

#Main Effects: Car
#Model Y = µ + αi + βj + ε
#Hypotheses: H0 : αi = 0 against H1 : at least one αi non-zero
#P-Value = 2.2e-16 < 0.05
#Car type is significant

## Checking Assumptions
plot(kml.2, which = 1:2)
kml.aov = aov(kmL ~ car * driver, data= kml)
plot(kml.aov, which = 1:2) 
#No curvature in the normal quantile plot of residuals
#The points is scattered evenly above and below the line.

#d) Conclusion: For the fit model with interaction, since the result of p-value is insignificant, so the effect of factor Driver on the efficiency of the car in km/L is independent of factor Car and there is no interaction between the two factors.
#For the fit model with main effects, since the p-value for both factor Driver and Car are significant, so at least one population mean of the efficiency of the car in km/L is different from others for all levels of factor Driver and factor Car.
