### RM2019-solution4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 29 2019 (12:31) 
## Version: 
## Last-Updated: nov 29 2019 (14:46) 
##           By: Brice Ozenne
##     Update #: 4
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
dfL.pressure$id <- as.factor(dfL.pressure$id)
e.aov2 <- lm(duration ~ id + treatment + period , data = dfL.pressure)
summary(e.aov2)
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


## ** Comparison

## chunk 49
getTreatmentEstimate <- function(object){

    object.coef <- coef(object)
    object.coef.se <- sqrt(diag(vcov(object)))

    out <- cbind(estimate = object.coef, 
                 se = object.coef.se)
    return(out[c("treatmentC","treatmentB"),])
}

## chunk 50
list(gls.CS = getTreatmentEstimate(e.glsCS),
     ANOVA.1way = getTreatmentEstimate(e.aov1),
     ANOVA.2way = getTreatmentEstimate(e.aov2))

## chunk 51
e.lm <- lm(duration ~ period + id + treatment,
           data = dfL.pressure)
getTreatmentEstimate(e.lm)

## * Question 6: Choice of the covariance structure

## chunk 52
dfL.pressure$sequence <- relevel(dfL.pressure$sequence, ref = "ABC")
dfL.pressure$treatment <- relevel(dfL.pressure$treatment, ref = "A")
dfL.pressure$period <- relevel(dfL.pressure$period, ref = "1")
dfL.pressure$id <- relevel(dfL.pressure$id, ref = "1")

## ** (a) Unstructured covariance matrix with period-specific variance/correlation

## chunk 53
e.glsUNperiod <- gls(duration ~ period + treatment,
          data = dfL.pressure,
          correlation = corSymm(form =~ as.numeric(period) | id),
          weight = varIdent(form =~ 1 | period))
logLik(e.glsUNperiod)
summary(e.glsUNperiod)
## chunk 54
Sigma.UNperiod <- getVarCov(e.glsUNperiod)
Sigma.UNperiod

## chunk 55
label.UNperiod <- attr(e.glsUNperiod$modelStruct$varStruct, 
                       "groupNames")

## chunk 56
rownames(Sigma.UNperiod) <- label.UNperiod
colnames(Sigma.UNperiod) <- label.UNperiod
print(Sigma.UNperiod)

## chunk 57
summary(e.glsUNperiod)$tTable

## ** (b) Unstructured covariance matrix with treatment-specific variance/correlation

## chunk 58
e.glsUNtreat <- gls(duration ~ period + treatment,
                    data = dfL.pressure,
                    correlation = corSymm(form =~ as.numeric(treatment) | id),
                    weight = varIdent(form =~ 1 | treatment))
logLik(e.glsUNtreat)
summary(e.glsUNtreat)
## chunk 59
Sigma.UNtreat <- getVarCov(e.glsUNtreat, type = "conditional")

label.UNtreat <- attr(e.glsUNtreat$modelStruct$varStruct, 
                      "groupNames")

rownames(Sigma.UNtreat) <- label.UNtreat
colnames(Sigma.UNtreat) <- label.UNtreat
print(Sigma.UNtreat)

## chunk 60
summary(e.glsUNtreat)$tTable

## ** Comparison

## chunk 61
anova(e.glsCS, e.glsUNperiod)

## chunk 62
anova(e.glsCS, e.glsUNtreat)

## ** [Extra] Other specification of the covariance matrix

## chunk 63
e.lmeCSseq <- lme(duration ~ period + treatment,
                  data = dfL.pressure,
                  random = list(id = pdDiag(~sequence - 1 )),
                  )
logLik(e.lmeCSseq)

## chunk 64
summary(e.lmeCS$modelStruct)

## chunk 65
summary(e.lmeCSseq$modelStruct)

## chunk 66
index.159 <- which(dfL.pressure$id %in% c(1,5,9))
dfL.pressure[index.159,]

## chunk 67
vec.SigmaCS <- list(
    "ABC" = getVarCov(e.lmeCS, individual = 1, type = "marginal"),
    "BCA" = getVarCov(e.lmeCS, individual = 5, type = "marginal"),
    "CAB" = getVarCov(e.lmeCS, individual = 9, type = "marginal")
)
print(vec.SigmaCS)

## chunk 68
vec.SigmaCSseq <- list(
    "ABC" = getVarCov(e.lmeCSseq, individual = 1, type = "marginal"),
    "BCA" = getVarCov(e.lmeCSseq, individual = 5, type = "marginal"),
    "CAB" = getVarCov(e.lmeCSseq, individual = 9, type = "marginal")
)
print(vec.SigmaCSseq)

## chunk 69
e.lmeUNfull <- lme(duration ~ period + treatment,
            data = dfL.pressure,
            random = list(id = pdDiag(~sequence - 1 )),
            correlation = corSymm(form =~ as.numeric(treatment)|id),
            weights = varIdent(form =~ 1|period)
                   )
logLik(e.lmeUNfull)

## chunk 70
vec.SigmaUNfull <- list(
    "ABC" = getVarCov(e.lmeUNfull, individual = 1, type = "marginal")[[1]],
    "BCA" = getVarCov(e.lmeUNfull, individual = 5, type = "marginal")[[1]],
    "CAB" = getVarCov(e.lmeUNfull, individual = 9, type = "marginal")[[1]]
)
lapply(vec.SigmaUNfull, cov2cor)

## * Question 7: Influence of the sequence on the correlation structure

## chunk 71
getTreatmentEstimate2 <- function(object){
    object.table <- summary(object)$tTable
    out <- object.table[c("treatmentC","treatmentB"),c(1:2,4)]
    return(out)
}

## chunk 72
list(CS = getTreatmentEstimate2(e.glsCS),
     UN.period = getTreatmentEstimate2(e.glsUNperiod),
     UN.treatment = getTreatmentEstimate2(e.glsUNtreat))

## * Question 8: Cross over effect 
## ** Definition of variable indicating the previous treatment

## chunk 73
dfL.pressure <- dfL.pressure[order(dfL.pressure$id,
                                   dfL.pressure$period.num),]
head(dfL.pressure)

## chunk 74
dfL.pressure$previousTreatment <- as.character(NA)

## chunk 75
ls.previousTreatment <- tapply(as.character(dfL.pressure$treatment), 
                               INDEX = dfL.pressure$id, 
                               FUN = function(x){c("none",x[1],x[2])}
                               )

## chunk 76
is.list(ls.previousTreatment)

## chunk 77
dfL.pressure$previousTreatment <- unlist(ls.previousTreatment)

## chunk 78
ls.previousTreatment2 <- tapply(as.character(dfL.pressure$treatment), 
                                INDEX = dfL.pressure$id, 
                                FUN = function(x){c("none","none",x[1])}
                                )
dfL.pressure$previousTreatment2 <- unlist(ls.previousTreatment2)

## chunk 79
dfL.pressure$previousTreatment <- as.factor(dfL.pressure$previousTreatment)
dfL.pressure$previousTreatment2 <- as.factor(dfL.pressure$previousTreatment2)

## chunk 80
list(ABC = table(dfL.pressure[dfL.pressure$sequence=="ABC","period"],
     dfL.pressure[dfL.pressure$sequence=="ABC","previousTreatment"]),
     BCA = table(dfL.pressure[dfL.pressure$sequence=="BCA","period"],
     dfL.pressure[dfL.pressure$sequence=="BCA","previousTreatment"]),
     CAB = table(dfL.pressure[dfL.pressure$sequence=="CAB","period"],
     dfL.pressure[dfL.pressure$sequence=="CAB","previousTreatment"]))

## chunk 81
list(ABC = table(dfL.pressure[dfL.pressure$sequence=="ABC","period"],
     dfL.pressure[dfL.pressure$sequence=="ABC","previousTreatment2"]),
     BCA = table(dfL.pressure[dfL.pressure$sequence=="BCA","period"],
     dfL.pressure[dfL.pressure$sequence=="BCA","previousTreatment2"]),
     CAB = table(dfL.pressure[dfL.pressure$sequence=="CAB","period"],
     dfL.pressure[dfL.pressure$sequence=="CAB","previousTreatment2"]))

## ** Model with carry over effects

## chunk 82
dfL.pressure$previousTreatment <- relevel(dfL.pressure$previousTreatment, "none")
dfL.pressure$previousTreatment2 <- relevel(dfL.pressure$previousTreatment2, "none")

## chunk 83
ff <- duration ~ treatment + previousTreatment + previousTreatment2
gls.UN_CO2 <- gls(ff,
           data = dfL.pressure,
           correlation = corSymm(form =~ as.numeric(treatment) | id),
           weight = varIdent(form =~ 1 | treatment))
logLik(gls.UN_CO2)

## chunk 84
anova(gls.UN_CO2, type = "marginal")

## chunk 85
gls.UN_CO <- gls(duration ~ treatment + previousTreatment,
          data = dfL.pressure,
          correlation = corSymm(form =~ as.numeric(treatment) | id),
          weight = varIdent(form =~1 | treatment))
logLik(gls.UN_CO)

## chunk 86
anova(gls.UN_CO, type = "marginal")

## chunk 87
summary(gls.UN_CO)$tTable

## chunk 88
dfL.pressure$previousC <- (dfL.pressure$previousTreatment == "C")

gls.UN_COc <- gls(duration ~ treatment + previousC,
            data = dfL.pressure,
            correlation = corSymm(form =~ as.numeric(treatment) | id),
            weight = varIdent(form =~ 1 | treatment))
logLik(gls.UN_COc)

## chunk 89
anova(update(gls.UN_COc, method = "ML"),
      update(gls.UN_CO, method = "ML"))

## chunk 90
dfL.pressure$fit <- predict(gls.UN_CO)

xy.fitted <- xyplot(fit ~ treatment, group = sequence, 
             data = dfL.pressure[order(dfL.pressure$treatment),], 
             type = "b",
             auto.key = TRUE)
xy.fitted

## * Using =data.table= 

## chunk 92
library(data.table)

## chunk 93
dtL.pressure <- as.data.table(dfL.pressure)

## ** Question 8

## chunk 94
dtL.pressure[, treatment.char := as.character(treatment)]

## chunk 95
dtL.pressure[,previous.treatment := c("none",treatment.char[1],treatment.char[2]),
             by="id"]
dtL.pressure[,previous.treatment2 := c("none","none",treatment.char[1]),
             by="id"]

## chunk 96
dtL.pressure[,table(previousTreatment, previous.treatment)]

## chunk 97
dtL.pressure[,table(previousTreatment2, previous.treatment2)]

## * Using =ggplot2= 

## chunk 98
library(ggplot2)

## ** Question 1

## chunk 99
gg.spaguettiP <- ggplot(dfL.pressure, aes(x = period, y = duration,
                                          group = id, color = id))
gg.spaguettiP <- gg.spaguettiP + geom_line() + geom_point()
gg.spaguettiP <- gg.spaguettiP + facet_grid(~sequence, 
                                            labeller = label_both)
gg.spaguettiP

## chunk 101
gg.spaguettiT <- ggplot(dfL.pressure, aes(x = treatment, y = duration,
                                          group = id, color = id))
gg.spaguettiT <- gg.spaguettiT + geom_line() + geom_point()
gg.spaguettiT <- gg.spaguettiT + facet_grid(~sequence, 
                                            labeller = label_both)
gg.spaguettiT

## chunk 103
gg.mean <- ggplot(dfL.pressure, aes(x = period, y = duration,
                                    group = sequence, color = sequence))
gg.mean <- gg.mean + stat_summary(geom = "line", fun.y = mean,
                                  size = 3, fun.data = NULL)
gg.mean <- gg.mean + ylab("average duration")
gg.mean

## ** Question 8

## chunk 105
gg.fit <- ggplot(dfL.pressure, aes(x = treatment, y = fit,
                                   group = sequence, color = sequence))
gg.fit <- gg.fit + geom_point(size = 2) + geom_line(size = 1.5)
gg.fit <- gg.fit + ylab("fitted duration")
gg.fit

## * =intervals= vs. =confint=

## chunk 107
vec.prob <- c(lower = 0.025, estimate = 0.5, upper = 0.975)

## chunk 108
beta <- summary(e.glsCS)$tTable["treatmentB","Value"]
beta.se <- summary(e.glsCS)$tTable["treatmentB","Std.Error"]
df.beta <- e.glsCS$dims$N - e.glsCS$dims$p
quantile_student <- qt(vec.prob, 
                       df = df.beta)

beta + quantile_student * beta.se

## chunk 109
confint(e.glsCS)["treatmentB",]

## chunk 110
quantile_gaussian <- qnorm(vec.prob)
beta + quantile_gaussian * beta.se



######################################################################
### RM2019-solution4.R ends here
