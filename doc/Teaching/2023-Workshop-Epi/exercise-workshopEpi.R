### exercise-workshopEpi.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Feb 18 2023 (14:10) 
## Version: 
## Last-Updated: Feb 28 2023 (10:09) 
##           By: Brice Ozenne
##     Update #: 38
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * R package
library(survival)
library(ggplot2)
library(riskRegression)
library(exact2x2)
library(data.table)
library(prodlim)
library(mets)

## * load data
url <- "https://bozenne.github.io/doc/Teaching/bissau.txt"
bissau <- read.table(file = url, header=TRUE)
bissau$ageC <- cut(bissau$age, c(-1,10,120,300))
bissau$status <- bissau$fupstatus=="dead"

## * Graphical representation
bissau.subset <- bissau[15:30,]
gg.traj <- ggplot(bissau.subset)
gg.traj <- gg.traj + geom_segment(aes(x = age/30.5, xend = (age+fuptime)/30.5, y = id, yend = id, color = bcg), linewidth = 1.25)
gg.traj <- gg.traj + geom_point(aes(x = (age + fuptime)/30.5, y = id, shape = fupstatus), size = 3, color = "black")
gg.traj <- gg.traj + geom_point(aes(x = (age + 183)/30.5, y = id, shape = "planned follow-up"), size = 3, color = "purple")
gg.traj <- gg.traj + scale_y_continuous(breaks = bissau.subset$id) + labs(y = "Patient id", x = "age (in months)", shape = "")
gg.traj <- gg.traj + scale_shape_manual(values = c(16,17,3),
                                        guide = guide_legend(override.aes = list(color = c("black","black","purple"))))
gg.traj <- gg.traj + theme(text = element_text(size=15), 
                           axis.line = element_line(linewidth = 1.25),
                           axis.ticks = element_line(linewidth = 2),
                           axis.ticks.length=unit(.25, "cm"),
                           legend.key.size = unit(3,"line"))
gg.traj

## * Ignoring treatment heterogeneity
t22 <- table(bcg = bissau$bcg, 
             status = bissau$status)

## Implementation 1
binomMeld.test(x1 = t22["no","TRUE"],
               x2 = t22["yes","TRUE"],
               n1 = sum(t22["no",]),
               n2 = sum(t22["yes",]),
               conf.int = TRUE,
               parmtype = "difference")

## Implementation 2
e.logit <- glm(status ~ 0+bcg, data = bissau, family = binomial(link="logit"))
1/(1+exp(-coef(e.logit)))
e.id <- glm(status ~ bcg, data = bissau, family = binomial(link="identity"))
coef(e.id)

## Implementation 1
binomMeld.test(x1 = t22["no","TRUE"],
               x2 = t22["yes","TRUE"],
               n1 = sum(t22["no",]),
               n2 = sum(t22["yes",]),
               conf.int = TRUE,
               parmtype = "ratio")

## Implementation 2
e.log <- glm(status ~ bcg, data = bissau, family = binomial(link="log"))
exp(coef(e.log))

## * Accounting for treatment heterogeneity
e.glmI <- glm(status ~ ageC * bcg, data = bissau, 
              family = binomial(link="logit"))

e.glmI.bis <- glm(status ~ 0 + ageC:bcg, data = bissau, 
              family = binomial(link="logit"))
100/(1+exp(-coef(e.glmI.bis)))

## Implementation 1
e.ate <- ate(e.glmI, treatment = "bcg", data = bissau)
summary(e.ate)

## Implementation 2
bissau0 <- bissau 
bissau0$bcg <- "no"
mean(predict(e.glmI, newdata = bissau0, type = "response"))


bissau0 <- bissau 
bissau0$bcg <- "yes"
mean(predict(e.glmI, newdata = bissau0, type = "response"))

## * Accounting for right censoring (rate)

## ** toy example
## orignal data with graphical display
df <- data.frame(id = 1:4, time = (1:4)*2, event = rep(1:0,2))

gg.traj <- ggplot(data = df)
gg.traj <- gg.traj + geom_point(aes(x = time, y = id,
                                    color = as.factor(event), shape = as.factor(event)))
gg.traj <- gg.traj + geom_segment(aes(x = 0, xend = time, y = id, yend = id))
gg.traj

## split data at each event
df.split <- survSplit(Surv(time,event)~1, data = df, cut = df$time,
                      id = "id", episode = "interval")
df.split$last <- rev(!duplicated(rev(df.split$id)))

gg <- ggplot()
gg <- gg + geom_segment(data = df.split,
                        aes(x = tstart, xend = time,
                            y = id - (interval-1)/10, yend = id - (interval-1)/10))
gg <- gg + geom_point(data = df.split[df.split$last,],
                      aes(x = time, y = id - (interval-1)/10,
                          color = as.factor(event), shape = as.factor(event)))
gg

## compute incidence rate by hand
n.event <- tapply(df.split$event,df.split$interval,sum) 
time.atrisk <- tapply(df.split$time-df.split$tstart,df.split$interval,sum)
lambda <- n.event/time.atrisk
dt <- tapply(df.split$time-df.split$tstart,df.split$interval,max)

## survival by hand
cumprod(1-lambda*dt)

## risk by hand
1- cumprod(1-lambda*dt)

## using a software package
e.KM <- prodlim(Hist(time,event) ~ 1, data = df)
plot(e.KM, marktime = TRUE)
1-predict(e.KM, times = 1:4*2)


## ** Bissau study
## Kaplan Meier - implementation 1
e.KM <- prodlim(Hist(fuptime, fupstatus=="dead") ~ bcg, data = bissau)
plot(e.KM)
ePred.KM <- predict(e.KM, newdata = data.frame(bcg = c("no","yes")), times = 183)
1-data.frame(ePred.KM)

1/(1+exp(-coef(e.glm)))

## Kaplan Meier - implementation 2
e.coxph <- coxph(Surv(fuptime, fupstatus=="dead") ~ strata(bcg), data = bissau, x = TRUE)
ePred.coxph <- predictCoxPL(e.coxph, newdata = data.frame(bcg = c("no","yes")), time = 183, se = TRUE)
dtPred.coxph <- as.data.table(ePred.coxph)
dtPred.coxph$risk <- 1-dtPred.coxph$survival
dtPred.coxph

## * Accounting for right censoring (IPCW)
## ** toy example

## by hand
df$survC <- predict(prodlim(Hist(time, event==0) ~ 1, data = df), time = df$time-0.00001)

1 - mean((df$time<=6)*(df$event==1)/df$survC)

## using a software package
e0.ipcw <- wglm(~1, formula.censor = Surv(time,event==0)~1,
               times = 6, data = df, product.limit = TRUE)
1/(1+exp(-coef(e0.ipcw)))

## ** bissau
## update dataset to specify that censored patient at time 183 are alive (no censored)
bissau2 <- bissau
bissau2[(bissau2$fuptime==183) & (bissau2$fupstatus=="censored"),"fuptime"] <- 183.1

e.ipcw <- wglm(~bcg, formula.censor = Surv(fuptime,fupstatus=="censored")~bcg,
               times = 183, data = bissau2, product.limit = TRUE)
summary(e.ipcw)

## small discrepancy with KM probably due to ties
1/(1+exp(-cumsum(coef(e.ipcw))))
1-data.frame(ePred.KM)

## using age in months to account for heterogeneity
e.ipcwI <- wglm(~bcg*as.factor(agem),
                formula.censor = Surv(fuptime,fupstatus=="censored")~bcg*as.factor(agem),
                times = 183, data = bissau2, product.limit = TRUE)
summary(e.ipcwI)

## * Competing risks
data(Melanoma, package = "riskRegression")

## ** No censoring
## make a dataset without censoring
## do not do that in real study!!! This is conditioning on the future and will bias risk estimates (upward)
Melanoma2 <- Melanoma[Melanoma$status != 0, ]
tau <- 5*365.25

## risk of death at 5 years days
mean((Melanoma2$time <= tau))

e.KM <- prodlim(Hist(time, status>0) ~ 1, data = Melanoma2)
1-predict(e.KM, time = tau)

## risk of death due to malignant melanoma at 200 days
mean((Melanoma2$time <= tau) * (Melanoma2$status == 1))

e.wrongKM1 <- prodlim(Hist(time, status==1) ~ 1, data = Melanoma2)
1-predict(e.wrongKM1, time = tau)


## risk of death due to other causes at 200 days
mean((Melanoma2$time <= tau) * (Melanoma2$status == 2))

e.wrongKM2 <- prodlim(Hist(time, status==2) ~ 1, data = Melanoma2)
1-predict(e.wrongKM2, time = tau)

## ** Censoring

## AJ approach
predict(prodlim(Hist(time, status) ~ 1, data = Melanoma2), time = tau)

e.AJ <- prodlim(Hist(time, status) ~ 1, data = Melanoma)
predict(e.AJ, time = tau)

par(mfrow = c(1,2))
plot(e.AJ, cause = 1, title = "Cancer related death")
plot(e.AJ, cause = 2, title = "Death from other causes")

## ICPW approach
e.ipcw.M2 <- wglm(~1,
                  formula.censor = Surv(time,status==0)~1,
                  times = tau, data = Melanoma2,
                  cause = 1, product.limit = TRUE)
1/(1+exp(-coef(e.ipcw.M2)))


e.ipcw.M <- wglm(~1,
                  formula.censor = Surv(time,status==0)~1,
                  times = tau, data = Melanoma,
                  cause = 1, product.limit = TRUE)
1/(1+exp(-coef(e.ipcw.M)))

Melanoma$survC <- predict(prodlim(Hist(time, status==0) ~ 1, data = Melanoma), time = pmin(tau,Melanoma$time)-0.00001)
mean((Melanoma$time <= tau) * (Melanoma$status == 1) / Melanoma$survC)

##----------------------------------------------------------------------
### exercise-workshopEpi.R ends here
