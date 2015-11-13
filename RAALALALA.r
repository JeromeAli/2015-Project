setwd("C:/Users/Jerome Jackson/Google Drive/Certified/R/School/Regressional Analysis/2015 Project")
require(boot)
require(MASS)
require(nortest)
require (car)
source("TM191.r")
source("TM181.r")

cred= read.csv("credit2.csv", header= T)

# cred[1,]
i  <- cred$ID
y  <- cred$score # a numeric vector giving the credit scores calculated by the bank on a scale from 0 to 100
x1 <- cred$savings # a numeric vector giving the total personal savings of each customer ($'000)
x2 <- cred$income # a numeric vector giving the total net income of each customer ($'000)
t1 <- cred$time.address # a numeric vector giving the number of months each customer has lived at their current address.
t2 <- cred$time.employed # a numeric vector giving the number of months each customer has been with their urrent employer.
d1 <- cred$fte # Full-time employment, and FALSE otherwise
d2 <- cred$status # customer is single, or otherwise

summary(cred)
plot(cred) 
pairs(cred[,-(4:4)], diag.panel=panel.hist) # Does Plot and includes histogram 

# Log the Data
credlog <- data.frame(ID = i,score=y, 
                      log.savings=log(x1 + 1), 
                      log.income=log(x2 +1), 
                      log.address=log(t1 +1),
                      log.employed=log(t2 +1), 
                      fte= d1, single=d2)
pairs(credlog[,2:6],diag.panel=panel.hist) # Plots variables 
mod.credlog = lm (y ~ log.savings + log.income + log.address + log.employed + fte +single , data = credlog)
summary(mod.credlog)
ncvTest(mod.credlog)
plot(mod.credlog)

mod.credlog.res <- resid(mod.credlog)
mod.credlog.stdres = rstandard(mod.credlog)

par(mfrow=c(3,3))
plot(x1,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[3] )
plot(x2,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[4] )
plot(t1,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[5] )
plot(t2,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[6] )
plot(d1,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[7] )
plot(d2,mod.credlog.stdres, ylab = "Standardized Residuals", xlab = names(mod.credlog)[8] )
plot(fitted(mod.credlog), mod.credlog.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.credlog.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.credlog.res,cex.main=0.75)
qqline(mod.credlog.res)


v=boxcox(mod.credlog, lambda = seq(0,2,.05))
range(v$x[v$y > max(v$y)-qchisq(0.95,1)/2])
v$x[which.max(v$y)]
# CV(cred.log)

mod.credlog2 = lm (((y^0.5858586)-1)/0.5858586 ~ log.savings + log.income + log.address + log.employed + fte +single , data = credlog)
summary(mod.credlog2)
plot(mod.credlog2)
pred_r_squared(mod.credlog2)


cred2 <- data.frame(ID = i,score=y +1 , 
                      savings= x1 + 1, 
                      income= x2 +1, 
                      address= t1 +1,
                      employed= t2 +1, 
                      fte= d1, single=d2)

summary(cred2)
plot(cred2)
mod.cred2 <- lm(score~ savings+ income + address + employed +fte + single, data= cred2)
pred_r_squared(mod.cred2)
summary(mod.cred2)

anova(mod.cred2)
mod.cred2.res <- resid(mod.cred2)
mod.cred2.stdres = rstandard(mod.cred2)

par(mfrow=c(3,3))
plot(x1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[3] )
plot(x2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[4] )
plot(t1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[5] )
plot(t2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[6] )
plot(d1,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[7] )
plot(d2,mod.cred2.stdres, ylab = "Standardized Residuals", xlab = names(cred2)[8] )
plot(fitted(mod.cred2), mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.cred2.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred2.res,cex.main=0.75)
qqline(mod.cred2.res)
bc=boxcox(mod.cred2, lambda = seq(0,2,.05))
lambda=bc$x
lik=b$y
bc=cbind(lambda,lik)
bc[order(-lik),]


mod.cred22 <- lm(score^0.5~ savings+ income + address + employed +fte + single, data= cred2)
summary(mod.cred22)
anova(mod.cred22)

mod.cred22.res <- resid(mod.cred22)
mod.cred22.stdres = rstandard(mod.cred22)

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
bc=boxcox(mod.cred22, lambda = seq(0,2,.05))

boxTidwell(score^.5 ~ savings+ income + address + employed, other.x=~ + fte + single, data= cred2)

plot(cred2) 
pairs(cred2[,-(4:4)], diag.panel=panel.hist)

incomesq = I(cred2$income^2.257)
incomeslog = log(cred2$income)
employedlog = log(cred2$employed)
employedsq = I(cred2$employed^2.19)
savings.sqrt.inv = (cred2$savings)^-0.5353
address.sqrt.inv = sqrt(cred2$address)^(-0.2144*2)
int

mod.cred222 <- lm(score^0.5~ savings.sqrt.inv +incomesq + address.sqrt.inv + employedsq +fte + single, data= cred2)
summary(mod.cred222)
anova(mod.cred222)

mod.cred222a <- lm(score^0.5~ savings.sqrt.inv +incomesq + address.sqrt.inv +employedsq , data= cred2) # highest Adjusted R-squared
summary(mod.cred222a)
anova(mod.cred222a)

mod.cred222b <- lm(score^0.5~ savings.sqrt.inv +incomesq + address.sqrt.inv +employedsq + single, data= cred2)
summary(mod.cred222b)
anova(mod.cred222b)

mod.cred222c <- lm(score^0.5~ savings.sqrt.inv +incomesq + address.sqrt.inv +employedsq+ fte , data= cred2)
summary(mod.cred222c)
anova(mod.cred222c)

mod.cred222.res <- resid(mod.cred222)
mod.cred222.stdres = rstandard(mod.cred222)

par(mfrow=c(3,3))
plot(savings.sqrt.inv,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[2] )
plot(incomesq,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[3] )
plot(address.sqrt.inv,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[4] )
plot(employedsq,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[5] )
plot(d1,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[6] )
plot(d2,mod.cred222.stdres, ylab = "Standardized Residuals", xlab = variable.names(mod.cred222)[7] )
plot(fitted(mod.cred222), mod.cred222.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.cred222.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred222.res,cex.main=0.75)
qqline(mod.cred222.res)
bc=boxcox(mod.cred222, lambda = seq(0,2,.05))

mod.cred2222 <- step(mod.cred222)
mod.cred222ab <- step.up(mod.cred222a)
summary(mod.cred222ab)

mod.cred222d <- lm(score^0.5~ savings.sqrt.inv +incomesq + address.sqrt.inv + employedsq +fte + single  , data= cred2)
mod.cred2222d <- step(mod.cred222d)
mod.cred222abd <- step.up(mod.cred222d)
summary(mod.cred222abd)


boxTidwell(score^0.5~  incomesq  +employedsq, other.x=~ + address.sqrt.inv +savings.sqrt.inv+ fte + single, data= cred2)
