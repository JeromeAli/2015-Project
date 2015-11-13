setwd("C:/Users/Jerome Jackson/Google Drive/Certified/R/School/Regressional Analysis/2015 Project")

require(boot) # Alternatively use library("name of package")
require(MASS)
require(nortest) # run install.packages("nortest") if not already install
require (car)  #run install.packages("xlsx") if not already install
require(xlsx) # run install.packages("xlsx") if not already install
source("TM191.r") # Programs used in this script
source("TM181.r")

cred= read.csv("credit2.csv", header= T) # Reads the given data on ourvle
summary(cred) 
plot(cred) # Plot all variables versus

#cred[1,] Shows all columns and first row 

#Assign Variables with simplier names
i  <- cred$ID
y  <- cred$score # a numeric vector giving the credit scores calculated by the bank on a scale from 0 to 100
x1 <- cred$savings # a numeric vector giving the total personal savings of each customer ($'000)
x2 <- cred$income # a numeric vector giving the total net income of each customer ($'000)
t1 <- cred$time.address # a numeric vector giving the number of months each customer has lived at their current address.
t2 <- cred$time.employed # a numeric vector giving the number of months each customer has been with their urrent employer.
d1 <- cred$fte # Full-time employment, and FALSE otherwise
d2 <- cred$status # customer is single, or otherwise

# Modifying Data - increment ever value by 1
# Data is modified in order to remove all points equalling zero apply Box-Cox 
cred2 <- data.frame(ID = cred$ID ,score= cred$score +1 , 
                    savings= cred$savings + 1, 
                    income= cred$income +1, 
                    address= cred$time.address +1,
                    employed= cred$time.employed +1, 
                    fte2= cred$fte, stat= cred$status)

summary(cred2)
plot(cred2) # Notice all point have been shift by 1 but same 
#Points to note :
# Score vs Savings - As savings increase Score tend to increase also
# Score vs Income - As income increase Score tend to increase also
# Score vs Income - As income increase Score tend to increase also
# Score vs time.address  - As Time at current Address increase Score tend to increase also
# Score vs time.employed  - No apparent postive or negative relation between score and time.employed
#        Observe time.employed variable through analyis 


#Assign Variables with simplier names
i  <- cred2$ID
y  <- cred2$score # a numeric vector giving the credit scores calculated by the bank on a scale from 0 to 100
x1 <- cred2$savings # a numeric vector giving the total personal savings of each customer ($'000)
x2 <- cred2$income # a numeric vector giving the total net income of each customer ($'000)
t1 <- cred2$address # a numeric vector giving the number of months each customer has lived at their current address.
t2 <- cred2$employed # a numeric vector giving the number of months each customer has been with their urrent employer.
d1 <- cred2$fte2 # Full-time employment, and FALSE otherwise
d2 <- cred2$stat # customer is single, or otherwise


# Model 1 - First Model
mod.cred2 <- lm(score~ savings+ income + address + employed +fte2 + stat, data= cred2)
summary(mod.cred2) #Adjusted $R^2$ 0.2742  Quite low
# If  Pr(>|t|) < 0.05 then variable is significant in model else it is not.
# For Model 1 All variables are significant to the model except the two factor variables fte2 and stats
anova(mod.cred2)
#

# Residuals
mod.cred2.res <- resid(mod.cred2) # Residuals of Model 1
mod.cred2.stdres = rstandard(mod.cred2) # Standard Residuals of Model 1

# Residual Plots
par(mfrow=c(3,3))
plot(x1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[3] )
plot(x2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[4] )
plot(t1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[5] )
plot(t2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[6] )
plot(d1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[7] )
plot(d2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[8] )
plot(fitted(mod.cred2), mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")
plot(i, mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
#to Test for Normality
qqnorm(mod.cred2.res,cex.main=0.75)
qqline(mod.cred2.res)

#Note
# Savings vs Residuals - funnel in implies non-constant variance
# Address vs Residuals - funnel in implies non-constant variance
# the other plots are relatively constant
# We thus appy transformations to the variables to allow for constance variance assumption

# For normality assumption- The data is fairly large n= 500 thus we expeected the error to be 
#   normall distributed and the qqnorm plot shows this.


# Applying thr Box-Cox Transformation technique
bc=boxcox(mod.cred2, lambda = seq(0,2,.05)) # Note lambda = seq(min,max,incrrement)
bc$x[which.max(bc$y)] # Gives Max of lambda based on graph ******Found to be lambda = 0.5858586

#Thus we Transform the reponse variable  by raising it to 0.5 (squareroot) 
#Transformation of response variable Model # 2
mod.cred22 <- lm(score^0.5~ savings+ income + address + employed +fte2 + stat, data= cred2)
summary(mod.cred22) # Adjusted $R^2$ 0.2471 increased slightly
#Note that the significance of the intercept increased tremendously.

anova(mod.cred22)

# Residuals for New Mod
mod.cred22.res <- resid(mod.cred22)
mod.cred22.stdres = rstandard(mod.cred22)

# Residual Plots
par(mfrow=c(3,3))
plot(x1,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[3] )
plot(x2,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[4] )
plot(t1,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[5] )
plot(t2,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[6] )
plot(d1,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[7] )
plot(d2,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[8] )
plot(fitted(mod.cred22), mod.cred22.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")
plot(i, mod.cred22.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred22.res,cex.main=0.75)
qqline(mod.cred22.res)

# Predictors need to be transform
boxTidwell(score ~  employed+savings+ income + address , other.x=~ + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ savings , other.x=~ + employed + income + address + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ income , other.x=~ + employed +savings  + address + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ address , other.x=~ + employed +savings  +income  + fte2 + stat, data= cred2, max.iter=50)


# Transformation of predictors
incomesq = I((cred2$income)^2.314 ) # Suggested by boxTidwell
#incomeslog = log(cred2$income) 
employedlog = log(cred2$employed)
employedsq = I((cred2$employed)^2.33 ) # Suggested by boxTidwell
savings.sqrt.inv = I((cred2$savings)^-0.5353 ) # Suggested by boxTidwell
address25 = I((cred2$address)^-0.2144 )

# Tranforming data
cred3 <- data.frame(ID = i,score= (cred2$score) , 
                   #incomesq = I((cred2$income)^2.314 ),
                   incomeslog = log(cred2$income),
                    employedlog = log(cred2$employed),
                   # employedsq = (cred2$employed)^2.33, 
                    savings.sqrt.inv = I((cred2$savings)^-0.5353 ),
                    address25 = I((cred2$address)^-0.2144 ), 
                    fte2= d1, 
                    stat=d2)
plot(cred3)

# Code Below uses fuction define the above sources
mod.cred222d <- lm(score ~ savings.sqrt.inv +incomeslog + address25 + employedlog +fte2 + stat  , data= cred2)
summary(mod.cred222d)
Y= cred2$score
b1 = savings.sqrt.inv
b2 = incomeslog
b3 = address25
b4 = employedlog

anova(mod.cred222d)

mod.cred222d.res <- resid(mod.cred222d)
mod.cred222d.stdres = rstandard(mod.cred222d)

par(mfrow=c(3,3))
plot(b1,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[2] )
plot(b2,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[3] )
plot(b3,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[4] )
plot(b4,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[5] )
plot(d1,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[6] )
plot(d2,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[7] )
plot(fitted(mod.cred222d), mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.cred222d.res, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred222d.res,cex.main=0.75)
qqline(mod.cred222d.res)

#At this Point we have a model with constant variances assumption, Normality assumption and 


# Interaction Terms
mod.cred2222d <- step(mod.cred222d)
mod.cred222abd <- step.up(mod.cred222d)
summary(mod.cred222abd) #  Adjusted $R^2$ 0.4973 
plot(mod.cred222abd)

# Not really important
Y= I(cred2$score^.5)
a1 = savings.sqrt.inv
a2 = incomesq
a3 = address25
a4 = employedsq



#int1 = incomeslog*employedlog
#int2 = address.sqrt.inv*stat
#int3 = employedlog*stat

# Final Model - Or least final one for the weekend
final.mod.cred <- lm( score^0.5 ~ savings.sqrt.inv + incomesq + address25 + employedsq + stat + incomesq:employedsq + address25:stat + employedsq:stat , data = cred2)
summary(final.mod.cred) # Adjusted $R^2$ -  0.4947
anova(final.mod.cred)

boxTidwell(score^0.5 ~ savings.sqrt.inv + incomesq + address25 + employedsq, other.x=~ +fte2 + stat, data= cred2, max.iter=50)

final.mod.cred.res <- resid(final.mod.cred)
final.cred222.stdres = rstandard(final.mod.cred)

par(mfrow=c(3,3))
plot(b1,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[2] )
plot(b2,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[3] )
plot(b3,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[4] )
plot(b4,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[5] )
plot(d1,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[6] )
plot(d2,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[7] )
plot(int1,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[8] )
plot(fitted(final.mod.cred), final.cred222.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, final.mod.cred.res, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(final.mod.cred.res,cex.main=0.75)
qqline(final.mod.cred.res)

#********* Creates the  CV Fit ******************
par(mfrow=c(2,2))
plot(final.mod.cred,las=1)
yhat=round(fitted.values(final.mod.cred),3)
rstd=round(rstudent(final.mod.cred),3)
sresd=round(rstandard(final.mod.cred),3)
cooks=round(cooks.distance(final.mod.cred),3)
lev=round(hatvalues(final.mod.cred),3)
dbeta=round(dfbetas(final.mod.cred),3)
dfit=round(dffits(final.mod.cred),3)
cvr=round(covratio(final.mod.cred),3)
Mghbv3 =cbind(cred2,yhat,rstd,sresd, cooks,lev,dbeta,dfit,cvr)

# Write to a excel file because its easier

write.xlsx(Mghbv3, "C:/Users/Jerome Jackson/Google Drive/Certified/R/School/Regressional Analysis/2015 Project/CVFinal256.xlsx")

pred.r.squared <- pred_r_squared(final.mod.cred)
press <- PRESS(final.mod.cred)
pred.r.squared