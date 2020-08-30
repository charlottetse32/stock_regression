DISData <- read.table("stock_regression.csv",sep=',',header=TRUE)

summary(DISData) 

rDIS <- DISData$rDIS
rf <- DISData$rf
rDIS_ex <- DISData$rDIS_ex
rM_ex <- DISData$rM_ex
rSmB <- DISData$rSmB
rHmL <- DISData$rHmL
rRmW <- DISData$rRmW
rCmA <- DISData$rCmA
MoM <- DISData$MoM

# One factor model: LS 
Onefactor <- lm(rDIS_ex ~ rM_ex)
summary(Onefactor)
cor(rDIS_ex,rM_ex,use = "complete.obs")
c(summary(Onefactor)$r.squared, cor(rDIS_ex,rM_ex,use = "complete.obs")^2)

# model diagnostics: LS 
par(mfrow=c(2,1))
plot(Onefactor$residuals,type="l",main="Residual Plot of LS Regression of rDIS_ex Against rM_ex ")

n <- length(rDIS_ex) - 1
plot(Onefactor$residuals[-n],Onefactor$residuals[-1],main="Residual against previous residual ")
cor(Onefactor$residuals[-n],Onefactor$residuals[-1])

par(mfrow=c(1,2))
qqnorm(stdres(Onefactor),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")
qqnorm(studres(Onefactor),main="Q-Q Plot of Studentized Residuals")
abline(0,1,col="red")

# One factor model: LAD 
library(quantreg) 
Onefactorlad <- rq(rDIS_ex ~ rM_ex,0.5)
summary(Onefactorlad)

# model diagnostics: LAD 
par(mfrow=c(2,1))
plot(Onefactorlad$residuals,type="l",main="Residual Plot of LAD Regression of rDIS_ex Against rM_ex ")
plot(Onefactorlad$residuals[-n],Onefactorlad$residuals[-1],main="Residual against previous residual ")
cor(Onefactorlad$residuals[-n],Onefactorlad$residuals[-1])

# 3 factors model: LS 
pairs(cbind(rDIS_ex,rM_ex,rSmB,rHmL))

FF3factor <- lm(rDIS_ex ~ rM_ex + rSmB + rHmL)
summary(FF3factor)

# 4 factors model: LS 
pairs(cbind(rDIS_ex,rM_ex,rSmB,rHmL,MoM))

FF4factor <- lm(rDIS_ex ~ rM_ex + rSmB + rHmL + MoM)
summary(FF4factor)

# 5 factors model: LS 
pairs(cbind(rDIS_ex,rM_ex,rSmB,rHmL,rRmW,rCmA))

FF5factor <- lm(rDIS_ex ~ rM_ex + rSmB + rHmL + rRmW + rCmA)
summary(FF5factor)

# own model: LS
pairs(cbind(rDIS_ex,rM_ex,rRmW))

ownmodel <- lm(rDIS_ex ~ rM_ex + rRmW)
summary(ownmodel)

# model diagnostics: own model 
par(mfrow=c(2,1))
plot(ownmodel$residuals,type="l",main="Residual Plot of LS Regression")

n <- length(rDIS_ex) - 1
plot(ownmodel$residuals[-n],ownmodel$residuals[-1],main="Residual against previous residual ")
cor(ownmodel$residuals[-n],ownmodel$residuals[-1])

par(mfrow=c(1,2))
qqnorm(stdres(ownmodel),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")
qqnorm(studres(ownmodel),main="Q-Q Plot of Studentized Residuals")
abline(0,1,col="red")

# comparison
round(c(summary(Onefactor)$r.squared, summary(FF3factor)$r.squared, summary(FF4factor)$r.squared, summary(FF5factor)$r.squared,  summary(ownmodel)$r.squared),5)






