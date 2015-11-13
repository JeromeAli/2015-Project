require(MASS)
require(nortest)
require (car)

cred= read.csv("credit2.csv", header= T)

cred[1,]
i  <- cred$ID
y  <- cred2$score # a numeric vector giving the credit scores calculated by the bank on a scale from 0 to 100
x1 <- cred2$savings # a numeric vector giving the total personal savings of each customer ($'000)
x2 <- cred2$income # a numeric vector giving the total net income of each customer ($'000)
t1 <- cred2$address # a numeric vector giving the number of months each customer has lived at their current address.
t2 <- cred2$employed # a numeric vector giving the number of months each customer has been with their current employer.
d1 <- cred2$fte # Full-time employment, and FALSE otherwise
d2 <- cred2$single # customer is single, or otherwise

summary(cred)
plot(cred)
pairs(cred[,-(4:4)], diag.panel=panel.hist)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
#The panel.hist function is defined in help(pairs).
credlog <- data.frame(score=y, 
                        log.savings=log(x1 + 1), 
                        log.income=log(x2 +1), 
                        log.address=log(t1 +1),
                        log.employed=log(t2 +1), 
                        fte= d1, single=d2)
pairs(credlog[,1:5],diag.panel=panel.hist)



# Model 1
mod.cred <- lm(y~ x1+ x2 + t1 + t2 +d1 + d2, data= cred)
boxTidwell(y~ x1+ x2 + t1 + t2, other.x =~ +d1 + d2, data = cred2)
plot(cred)
summary(mod.cred)
mod.credst <- step(mod.cred)
mod.credstu <- step.up(mod.credst)


summary(mod.credstu)

summary(mod.cred)
anova(mod.cred)
mod.cred.res <- resid(mod.cred)
mod.cred.stdres = rstandard(mod.cred)

par(mfrow=c(3,3))
plot(x1,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4] )
plot(t1,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(t2,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[6] )
plot(d1,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[7] )
plot(d2,mod.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[8] )
plot(fitted(mod.cred), mod.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.cred.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.cred.res,cex.main=0.75)
qqline(mod.cred.res)
b=boxcox(mod.cred)
b$x[which.max(b$y)]

moda.cred <- lm(y^ 0.5858586~ x1+ x2 + t1 + t2 +d1 + d2, data= cred)  
summary(moda.cred)
mod.creda.res <- resid(moda.cred)
mod.creda.stdres = rstandard(moda.cred)
plot(x1,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4] )
plot(t1,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(t2,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[6] )
plot(d1,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[7] )
plot(d2,mod.creda.stdres, ylab = "Standardized Residuals", xlab = names(cred)[8] )
plot(fitted(moda.cred), mod.creda.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(i, mod.creda.stdres, ylab = "Standardized Residuals", xlab = "Observation Order")
qqnorm(mod.creda.res,cex.main=0.75)
qqline(mod.creda.res)



ncvTest(moda.cred)
ncvTest(mod.cred)
#Model # 2
mod2.cred <- lm(y~ x1+ x2 + t1 + t2, data= cred)
summary(mod2.cred)
?ncvTest # This test is often called the Breusch-Pagan test

anova(mod2.cred)
mod2.cred.res <- resid(mod2.cred)
mod2.cred.stdres = rstandard(mod2.cred)

par(mfrow=c(2,3))
plot(x1,mod2.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod2.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(t1,mod2.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(t2,mod2.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[6] )
plot(fitted(mod2.cred), mod2.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

#Model # 3 A
mod3a.cred <- lm(y~ x1+ x2 + t2 , data= cred)
summary(mod3a.cred)

anova(mod3a.cred)
mod3a.cred.res <- resid(mod3a.cred)
mod3a.cred.stdres = rstandard(mod3a.cred)

par(mfrow=c(2,3))
plot(x1,mod3a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod3a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(t1,mod3a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(fitted(mod3a.cred), mod3a.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

#Model # 3 B
mod3b.cred <- lm(y~ x1+ x2 + t1 , data= cred2)
summary(mod3b.cred)
boxTidwell(y~x1+ x2 + t1, data= cred2)
plot(mod3b.cred)

anova(mod3b.cred)
mod3b.cred.res <- resid(mod3b.cred)
mod3b.cred.stdres = rstandard(mod3b.cred)

par(mfrow=c(2,3))
plot(x1,mod3b.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod3b.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(t1,mod3b.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(fitted(mod3b.cred), mod3b.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

#Model # 4
mod4.cred <- lm(y~ x1 + x2 , data= cred2)
summary(mod4.cred)
boxTidwell(y~x2+x1, data= cred2)
plot(mod4.cred)

anova(mod4.cred)
mod4.cred.res <- resid(mod4.cred)
mod4.cred.stdres = rstandard(mod4.cred)

par(mfrow=c(2,3))
plot(x1,mod4.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod4.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(t1,mod4.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(fitted(mod4.cred), mod4.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

#Model # 5 a
mod5a.cred <- lm(y~ x1 , data= cred2)
summary(mod5a.cred)
boxTidwell(y~x1, data= cred2)
plot(mod5a.cred)

anova(mod5a.cred)
mod5a.cred.res <- resid(mod5a.cred)
mod5a.cred.stdres = rstandard(mod5a.cred)

par(mfrow=c(2,3))
plot(x1,mod5a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[3] )
plot(x2,mod5a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(t1,mod5a.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[5] )
plot(fitted(mod5a.cred), mod5a.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

#Model # 5 b
mod5b.cred <- lm(y~ x2 , data= cred2)
summary(mod5b.cred)
boxTidwell(y~x2, data= cred2)
plot(mod5b.cred)
anova(mod5b.cred)
mod5b.cred.res <- resid(mod5b.cred)
mod5b.cred.stdres = rstandard(mod5b.cred)

par(mfrow=c(2,3))
plot(x2,mod5b.cred.stdres, ylab = "Standardized Residuals", xlab = names(cred)[4])
plot(fitted(mod5b.cred), mod5b.cred.stdres, ylab = "Standardized Residuals", xlab = "Fitted Values")

ar(mfrow=c(2,2))
plot(mod.cred,las=1)
yhat=round(fitted.values(mod.cred222d),3)
rstd=round(rstudent(mod.cred222d),3)
sresd=round(rstandard(mod.cred222d),3)
cooks=round(cooks.distance(mod.cred222d),3)
lev=round(hatvalues(mod.cred222d),3)
dbeta=round(dfbetas(mod.cred222d),3)
dfit=round(dffits(mod.cred222d),3)
cvr=round(covratio(mod.cred222d),3)
M=cbind(cred2,yhat,rstd,sresd,resid,cooks,lev,cvr,dfit,dbeta)