### RM2019-solution4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 29 2019 (12:31) 
## Version: 
## Last-Updated: nov 29 2019 (13:16) 
##           By: Brice Ozenne
##     Update #: 2
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

## ** Inference

## chunk 26
summary(e.lmeCS)$tTable

## chunk 27
anova(e.lmeCS, type = "marginal")

## chunk 28
intervals(e.lmeCS)$fixed[c("treatmentB","treatmentC"),]

## chunk 29
newdata <- unique(dfL.pressure[dfL.pressure$treatment %in% c("A","B"),c("treatment","period")])
newdata <- newdata[order(newdata$treatment,newdata$period),]
newdata

## chunk 30
newdata$fit.lmeCS <- predict(e.lmeCS, newdata = newdata, level = 0)

data.frame(period = newdata$period[newdata$treatment=="A"],
           diff = newdata$fit.lmeCS[newdata$treatment=="B"] - newdata$fit.lmeCS[newdata$treatment=="A"]
           )

## chunk 31
X <- model.matrix( ~ treatment + period, data = newdata)
X

## chunk 32
X[newdata$treatment=="B",] - X[newdata$treatment=="A",]

## * Question 4: Heterogeneity among the subjects

## chunk 33
summary(e.lmeCS)

variance.between <- intervals(e.lmeCS)$reStruct$id["est."]^2
variance.within <- intervals(e.lmeCS)$sigma["est."]^2

variance.between/(variance.between+variance.within)

## chunk 35
rho <- intervals(e.glsCS)[["corStruct"]][,"est."]
rho

## chunk 36
variance.between <- sigma(e.glsCS)^2 * rho
variance.within <- sigma(e.glsCS)^2 * (1-rho)

## chunk 39
qt(c(0.025,0.975), df = 20) * sqrt(2 * variance.within)

## chunk 40
qt(c(0.025,0.975), df = 20) * sqrt(2 * (variance.within + variance.between))

## * Question 5: Comparison with 1 or 2 way ANOVA
## ** One way ANOVA

## chunk 41
e.aov1 <- lm(duration ~ treatment, data = dfL.pressure)

## chunk 42
anova(e.aov1)

## chunk 43
ci.aov1 <- cbind(estimate = coef(e.aov1),confint(e.aov1))
ci.aov1[c("treatmentC","treatmentB"),]

## chunk 44
keep.coef <- paste0("treatment",c("B","C"))

M <- cbind(
    summary(e.glsCS)$tTable[keep.coef,c("Value","Std.Error")],
    summary(e.aov1)$coef[keep.coef,c("Estimate","Std. Error")]
)
colnames(M) <- c("estimate.gls","se.gls","estimate.lm","se.lm")
M


## ** Two way ANOVA

## chunk 45
e.aov2 <- lm(duration ~ id + treatment, data = dfL.pressure)

## chunk 46
anova(e.aov2)

## chunk 47
ci.aov2 <- cbind(estimate = coef(e.aov2),confint(e.aov2))
ci.aov2[c("treatmentC","treatmentB"),]

## chunk 48
keep.coef <- paste0("treatment",c("B","C"))

M <- cbind(
    summary(e.glsCS)$tTable[keep.coef,c("Value","Std.Error")],
    summary(e.aov2)$coef[keep.coef,c("Estimate","Std. Error")]
)
colnames(M) <- c("estimate.gls","se.gls","estimate.lm","se.lm")
M


######################################################################
### RM2019-solution4.R ends here
