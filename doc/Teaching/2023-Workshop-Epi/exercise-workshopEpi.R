### exercise-workshopEpi.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Feb 18 2023 (14:10) 
## Version: 
## Last-Updated: feb 22 2023 (19:12) 
##           By: Brice Ozenne
##     Update #: 13
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
e.glm <- glm(status ~ 0+bcg, data = bissau, family = binomial(link="logit"))
1/(1+exp(-coef(e.glm)))

## Implementation 1
binomMeld.test(x1 = t22["no","TRUE"],
               x2 = t22["yes","TRUE"],
               n1 = sum(t22["no",]),
               n2 = sum(t22["yes",]),
               conf.int = TRUE,
               parmtype = "ratio")

## Implementation 2
e.glmBis <- glm(status ~ bcg, data = bissau, family = binomial(link="logit"))
exp(coef(e.glmBis))

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

## * Accounting for right censoring

## ** toy example
## orignal data with graphical display
df <- data.frame(id = 1:4, time = (1:4)*2, event = rep(1:0,2))

gg.traj <- ggplot(data = df)
gg.traj <- gg.traj + geom_point(aes(x = time, y = id,
                                    color = as.factor(event), shape = as.factor(event)))
gg.traj <- gg.traj + geom_segment(aes(x = 0, xend = time, y = id, yend = id))

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


## ** IPCW

## by hand

df$survC <- predict(prodlim(Hist(time, event==0) ~ 1, data = df), time = df$time-0.00001)

1 - mean((df$time<=6)*(df$event==1)/df$survC)

## using a software package
e.ipcw <- wglm(~1, formula.censor = Surv(time,event==0)~1,
               times = 6, data = df, product.limit = TRUE)
1/(1+exp(-coef(e.ipcw)))


##----------------------------------------------------------------------
### exercise-workshopEpi.R ends here
