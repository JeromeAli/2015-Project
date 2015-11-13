setwd("C:/Users/Jerome Jackson/Google Drive/Certified/R/School/Regressional Analysis/2015 Project")
require(boot)
require(MASS)
require(nortest)
require (car)
require(xlsx)
source("TM191.r")
source("TM181.r")

cred= read.csv("credit2.csv", header= T)
summary(cred)
plot(cred) 

# cred[1,]
i  <- cred$ID
y  <- cred$score # a numeric vector giving the credit scores calculated by the bank on a scale from 0 to 100
x1 <- cred$savings # a numeric vector giving the total personal savings of each customer ($'000)
x2 <- cred$income # a numeric vector giving the total net income of each customer ($'000)
t1 <- cred$time.address # a numeric vector giving the number of months each customer has lived at their current address.
t2 <- cred$time.employed # a numeric vector giving the number of months each customer has been with their urrent employer.
d1 <- cred$fte # Full-time employment, and FALSE otherwise
d2 <- cred$status # customer is single, or otherwise

# Modifying Data - increment ever value by 1
cred2 <- data.frame(ID = i,score=y +1 , 
                    savings= x1 + 1, 
                    income= x2 +1, 
                    address= t1 +1,
                    employed= t2 +1, 
                    fte2= d1, stat=d2)

summary(cred2)
plot(cred2)

# Model 1
mod.cred2 <- lm(score~ savings+ income + address + employed +fte2 + stat, data= cred2)

summary(mod.cred2) #Adjusted $R^2$ 0.2742 
anova(mod.cred2)

# Residuals
mod.cred2.res <- resid(mod.cred2)
mod.cred2.stdres = rstandard(mod.cred2)

# Residual Plots
par(mfrow=c(3,3))
plot(cred2$savings,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[3] )
plot(cred2$income,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[4] )
plot(cred2$address,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[5] )
plot(cred2$employed,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[6] )
plot(d1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[7] )
plot(d2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[8] )
plot(fitted(mod.cred2), mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")
plot(i, mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")

qqnorm(mod.cred2.res,cex.main=0.75)
qqline(mod.cred2.res)

bc=boxcox(mod.cred2, lambda = seq(0,2,.05))
bc$x[which.max(bc$y)] # Gives MLE of lambda

#Transformation of response variable Model # 2
mod.cred22 <- lm(score^0.5~ savings+ income + address + employed +fte2 + stat, data= cred2)
summary(mod.cred22) # Adjusted $R^2$ 0.2471
anova(mod.cred22)

mod.cred22.res <- resid(mod.cred22)
mod.cred22.stdres = rstandard(mod.cred22)

par(mfrow=c(3,3))
plot(cred2$savings,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[3] )
plot(cred2$income,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[4] )
plot(cred2$address,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[5] )
plot(cred2$employed,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[6] )
plot(d1,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[7] )
plot(d2,mod.cred22.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[8] )
plot(fitted(mod.cred22), mod.cred22.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")
plot(i, mod.cred22.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred22.res,cex.main=0.75)
qqline(mod.cred22.res)

# Predictors need to be transform
boxTidwell(score^.5 ~  employed+savings+ income + address , other.x=~ + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ savings , other.x=~ + employed + income + address + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ income , other.x=~ + employed +savings  + address + fte2 + stat, data= cred2, max.iter=50)
boxTidwell(score^.5 ~ address , other.x=~ + employed +savings  +income  + fte2 + stat, data= cred2, max.iter=50)


# Transformation of predictors
incomesq = I((cred2$income)^2.314 )
#incomeslog = log(cred2$income)# Why log it - well fuck you thats why
#employedlog = (cred2$employed)^2
employedsq = I((cred2$employed)^2.33 )
savings.sqrt.inv = I((cred2$savings)^-0.5353 )
address25 = I((cred2$address)^-0.2144 )

# Code Below uses fuction define the above sources
mod.cred222d <- lm(I(score^0.5)~ savings.sqrt.inv +incomesq + address25 + employedsq +fte2 + stat  , data= cred2)
summary(mod.cred222d)
Y= cred2$score^.5
a1 = savings.sqrt.inv
a2 = incomesq
a3 = address25
a4 = employedsq

anova(mod.cred222d)

mod.cred222d.res <- resid(mod.cred222d)
mod.cred222d.stdres = rstandard(mod.cred222d)

par(mfrow=c(3,3))
plot(a1,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[2] )
plot(a2,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[3] )
plot(a3,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[4] )
plot(a4,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[5] )
plot(d1,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[6] )
plot(d2,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[7] )
plot(int1,mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222d)[8] )
plot(fitted(mod.cred222d), mod.cred222d.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.cred222d.res, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred222d.res,cex.main=0.75)
qqline(mod.cred222d.res)


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
plot(a1,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[2] )
plot(a2,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[3] )
plot(a3,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[4] )
plot(a4,final.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(final.mod.cred)[5] )
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
Mghbv =cbind(cred2,yhat,rstd,sresd, cooks,lev,dbeta,dfit,cvr)

# Write to a excel file because its easier

write.xlsx(Mghbv, "C:/Users/Jerome Jackson/Google Drive/Certified/R/School/Regressional Analysis/2015 Project/CVFinal256.xlsx")

pred.r.squared <- pred_r_squared(final.mod.cred)
press <- PRESS(final.mod.cred)
pred.r.squared