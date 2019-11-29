### RM2019-solution4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 29 2019 (12:31) 
## Version: 
## Last-Updated: nov 29 2019 (12:31) 
##           By: Brice Ozenne
##     Update #: 1
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## chunk 2
library(reshape2)  # for converting data.frame from wide to long format
library(nlme) # models for repeated measurements (gls, lme)
library(lattice) # graphical display (xyplot)

## * Question 1: Import data
## ** Data management

## chunk 3
dfL.pressure <- read.table("bloodpressure.txt", header = TRUE)
str(dfL.pressure)

## chunk 4
names(dfL.pressure) <- tolower(names(dfL.pressure))

## chunk 5
table(paste0("P",dfL.pressure$period),
      dfL.pressure$treatment,
      paste0("S",dfL.pressure$sequence))

## chunk 6
dfL.pressure$id <- as.factor(dfL.pressure$id)
dfL.pressure$sequence <- factor(dfL.pressure$sequence, 
                                levels = 1:3, labels = c("ABC","BCA","CAB"))
dfL.pressure$treatment <- as.factor(dfL.pressure$treatment)
dfL.pressure$period.num <- dfL.pressure$period
dfL.pressure$period <- as.factor(dfL.pressure$period.num)

## ** Inspection of the dataset

## chunk 7
summary(dfL.pressure, maxsum = 12)

## chunk 8
table(dfL.pressure$id,dfL.pressure$treatment)

## chunk 9
table(dfL.pressure$id, dfL.pressure$sequence)

## chunk 10
colSums(is.na(dfL.pressure))

## ** Descriptive graphs

## chunk 11
plot.spaguettiP <- xyplot(duration ~ period | sequence, group = id, 
                            data = dfL.pressure, type = "b", layout = c(3,1))
plot.spaguettiP

## chunk 13
plot.spaguettiT <- xyplot(duration ~ treatment | sequence, group = id, 
     data = dfL.pressure[order(dfL.pressure$treatment),], 
     type = "b", layout = c(3,1))
plot.spaguettiT

## chunk 15
interaction.plot(x.factor = dfL.pressure$period, 
                 trace.factor = dfL.pressure$sequence, 
                 response = dfL.pressure$duration, 
                 fun = "mean",
                 trace.label = "sequence",
                 xlab = "period",
                 ylab = "duration",
                 col = c("red","blue","green"),
                 lwd = 4)

## * Question 3: Testing a treatment effect controling for period
## ** Fitting the model

## chunk 17
dfL.pressure$treatment <- relevel(dfL.pressure$treatment, ref = "A")
dfL.pressure$period <- relevel(dfL.pressure$period, ref = "1")

## chunk 18
e.glsCS <- gls(duration ~ period + treatment,
              data = dfL.pressure,
              correlation = corCompSymm(form=~ 1 | id))
logLik(e.glsCS)

## chunk 19
e.lmeCS <- lme(duration ~ period + treatment,
               data = dfL.pressure, 
               random =~ 1 | id)
logLik(e.lmeCS)

## chunk 20
coef(e.glsCS) - fixef(e.lmeCS)

## chunk 21
Sigma.glsCS <- getVarCov(e.glsCS, type = "marginal")
Sigma.lmeCS <- getVarCov(e.lmeCS, type = "marginal")[[1]]
unclass(Sigma.glsCS) - unclass(Sigma.lmeCS)

## chunk 22
summary(e.glsCS$modelStruct)

## chunk 23
summary(e.lmeCS$modelStruct)

## chunk 24
sigma.gls <- sigma(e.glsCS)
rho.gls <- intervals(e.glsCS)[["corStruct"]][,"est."]  

sigma.lme <- sigma(e.lmeCS)
tau.lme <- intervals(e.lmeCS)[["reStruct"]]$id[,"est."]  

cbind(variance = c(lme = sigma.lme^2 + tau.lme^2, gls = sigma.gls^2), 
      covariance = c(lme = tau.lme^2, gls = sigma.gls^2 * rho.gls))

## chunk 25
Sigma.glsCS


######################################################################
### RM2019-solution4.R ends here
