### tempo.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: aug  3 2021 (17:46) 
## Version: 
## Last-Updated: aug  4 2021 (18:08) 
##           By: Brice Ozenne
##     Update #: 9
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * packages
require(survival)
require(riskRegression)

## * sim data
simTrial <- function(n, dmu, dpC){
  require(BuyseTest)
  require(data.table)
  ## simulate data
  dt1  <- simBuyseTest(n.T = n, n.C = n, 
                      argsBin = NULL, argsCont = NULL, 
                      argsTTE = list(scale.T = 1+dmu[1],
                                     scale.C = 1,
                                     scale.Censoring.T = 1+dpC[1],
                                     scale.Censoring.C = 1),
                      latent = TRUE)
  dt2  <- simBuyseTest(n.T = n, n.C = n, 
                       argsBin = NULL, argsCont = NULL, 
                       argsTTE = list(scale.T = 2+dmu[2],
                                      scale.C = 2,
                                      scale.Censoring.T = 2+dpC[2],
                                      scale.Censoring.C = 2),
                       latent = TRUE)
  ## gather into dataset
  dt <- rbind(
    cbind(id = 1:NROW(dt1), group = "G1", dt1),
    cbind(id = NROW(dt1) + 1:NROW(dt2), group = "G2", dt2)
  )
  return(dt)
}
## * Example
set.seed(11)
n <- 10000
dt <- simTrial(n = n, dmu = c(0,1), dpC = c(0,1))

## ** process data
tau <- 1
dt$responseUncensored <- dt$eventtimeUncensored<=tau
dt$response <- ifelse((dt$status==1)+(dt$eventtime>tau),dt$eventtime<=tau,NA)
dt$observed <- ifelse((dt$status==1)+(dt$eventtime>tau),1,0)
print(dt)

## ** oracle
e.oracle <- glm(responseUncensored ~ treatment, data = dt, family = binomial(link="logit"))
summary(e.oracle)$coef
#                 Estimate Std. Error   z value     Pr(>|z|)
# (Intercept)  0.07022885 0.01415085   4.96287 6.945906e-07
# treatmentT  -0.23721580 0.02004104 -11.83650 2.527721e-32

## ** complete case
dt.cc <- dt[dt$observed==1]
e.cc <- glm(response ~ treatment, data = dt.cc, family = binomial(link="logit"))
summary(e.cc)$coef
# Estimate Std. Error   z value      Pr(>|z|)
# (Intercept)  0.4023018 0.01814959  22.16589 7.330653e-109
# treatmentT  -0.3656280 0.02510139 -14.56605  4.618541e-48

## ** IPCW 1
e.IPCmodel <- glm(observed ~ group*treatment, data = dt, family = binomial(link="logit"))
## summary(e.IPCmodel)
dt$IPCweights <- 1/predict(e.IPCmodel, newdata = dt, type = "response")
## sum(dt$IPCweights)

dt.cc <- dt[dt$observed==1]
e.IPCWcc <- glm(response ~ treatment, data = dt.cc, family = binomial(link="logit"), weights = dt.cc$IPCweights)
summary(e.IPCWcc)$coef
# Estimate Std. Error   z value     Pr(>|z|)
## (Intercept)  0.4593548 0.01451679  31.64300 9.465106e-220
## treatmentT  -0.2997806 0.02029810 -14.76889  2.324953e-49

## ** IPCW 2
e.IPCmodel2 <- coxph(Surv(eventtime,status==0) ~ group*treatment, data = dt, x = TRUE, y = TRUE)
## summary(e.IPCmodel)
dt$IPCweights2 <- dt$observed/predictCox(e.IPCmodel2, newdata = dt, time = pmin(dt$eventtime,tau)-(1e-12), diag = TRUE)$survival
## sum(dt$IPCweights2)
## pmin(dt$eventtime,tau) - recoverTime

## dt$eventtime-object.censor$y[,"time"]
## recoverTime-object.censor$y[,"time"]
## recoverTime-dt$eventtime

## range(predictCox(e.IPCmodel2, newdata = dt, time = pmin(dt$eventtime,tau)-(1e-12), diag = TRUE)$survival-iPred)

dt.cc <- dt[dt$observed==1]
e.IPCWcc <- glm(response ~ treatment, data = dt.cc, family = binomial(link="logit"), weights = dt.cc$IPCweights2)
summary(e.IPCWcc)$coef
#               Estimate Std. Error    z value     Pr(>|z|)
## (Intercept)  0.06439848 0.01414818   4.551716 5.321009e-06
## treatmentT  -0.23858152 0.02003591 -11.907693 1.079216e-32

## using riskRegression
library(riskRegression)
e.wglm <- wglm(regressor.event = ~treatment,
               formula.censor = Surv(eventtime,status==0)~group*treatment,
               times = 1,
               data = dt[,.(eventtime,status,group,treatment)])
coef(e.wglm)
## (Intercept)  treatmentT 
##  0.06439847 -0.23858152 

system.time(summary(wglm(regressor.event = ~treatment,
                         formula.censor = Surv(eventtime,status==0)~group*treatment,
                         times = 1,
                         data = dt[,.(eventtime,status,group,treatment)])))

system.time(binreg(formula = Event(eventtime,status) ~ treatment,
                   cens.model = ~group*treatment,
                   time = 1, data = dt, cens.code = 0, cause = 1))

## ** using mets
library(mets)
e.binreg <- binreg(formula = Event(eventtime,status) ~ treatment,
                   cens.model = ~group*treatment,
                   time = 1, data = dt, cens.code = 0, cause = 1)
summary(e.binreg)
##              Estimate   Std.Err      2.5%     97.5% P-value
## (Intercept)  0.064776  0.019716  0.026134  0.103418   0.001
## treatmentT  -0.236638  0.027400 -0.290341 -0.182936   0.000

## * Simulation study

warper <- function(n, dmu, dpC, tau){

    ## simulate data
    dt <- simTrial(n = n, dmu = dmu, dpC = dpC)
    dt$responseUncensored <- dt$eventtimeUncensored<=tau
    dt$response <- ifelse((dt$status==1)+(dt$eventtime>tau),dt$eventtime<=tau,NA)
    dt$observed <- ifelse((dt$status==1)+(dt$eventtime>tau),1,0)

    ## oracle estimator
    e.oracle <- glm(responseUncensored ~ treatment, data = dt, family = binomial(link="logit"))

    ## complete case estimator
    dt.cc <- dt[dt$observed==1]
    e.cc <- glm(response ~ treatment, data = dt.cc, family = binomial(link="logit"))

    ## IPCW version 1
    e.IPCmodel <- glm(observed ~ group*treatment, data = dt, family = binomial(link="logit"))
    dt$IPCweights <- 1/predict(e.IPCmodel, newdata = dt, type = "response")
    dt.cc <- dt[dt$observed==1]
    e.IPCWglm <- suppressWarnings(glm(response ~ treatment, data = dt.cc, family = binomial(link="logit"), weights = dt.cc$IPCweights))

    ## IPCW version 2
    e.IPCWcox <- wglm(regressor.event = ~treatment,
                      formula.censor = Surv(eventtime,status==0)~group*treatment,
                      times = 1,
                      data = dt[,.(eventtime,status,group,treatment)])

    ## IPCW mets
    e.mets <- binreg(formula = Event(eventtime,status) ~ treatment,
                     cens.model = ~group*treatment,
                     time = 1, data = dt, cens.code = 0, cause = 1)

    ## assemble
    res.oracle <- setNames(summary(e.oracle)$coef["treatmentT",c(1:2,4)], c("estimate","sd","p.value"))
    res.cc <- setNames(summary(e.cc)$coef["treatmentT",c(1:2,4)], c("estimate","sd","p.value"))
    res.IPCWglm <- setNames(summary(e.IPCWglm)$coef["treatmentT",c(1:2,4)], c("estimate","sd","p.value"))
    res.IPCWcox <- setNames(summary(e.IPCWcox, print = FALSE)[[1]]["treatmentT",c(1:2,4)], c("estimate","sd","p.value"))
    res.mets <- setNames(summary(e.mets)$coef["treatmentT",c(1:2,5)], c("estimate","sd","p.value"))
    
    out <- rbind(cbind(estimator = "oracle", as.data.frame(as.list(res.oracle))),
                 cbind(estimator = "cc", as.data.frame(as.list(res.cc))),
                 cbind(estimator = "IPCWglm", as.data.frame(as.list(res.IPCWglm))),
                 cbind(estimator = "IPCWcox", as.data.frame(as.list(res.IPCWcox))),
                 cbind(estimator = "mets", as.data.frame(as.list(res.mets))))
    ##
    return(cbind(n=n,tau=tau,out))
}

set.seed(11)
warper(n = 1000, dmu = c(0,1), dpC = c(0,1), tau = 1)

library(pbapply)
n.sim <- 100
ls.res <- pblapply(1:n.sim, function(iSim){
  rbind(warper(n = 100, dmu = c(0,1), dpC = c(0,1), tau = 1),
        warper(n = 500, dmu = c(0,1), dpC = c(0,1), tau = 1),
        warper(n = 1000, dmu = c(0,1), dpC = c(0,1), tau = 1))
})

library(ggplot2)
library(data.table)
dt.res <- as.data.table(do.call(rbind,ls.res))
dt.res[, empirical.sd := sd(estimate), by  = c("n","tau","estimator")]
dt.res[, sample.size := paste0("sample.size: ",n)]
gg.beta <- ggplot(dt.res, aes(y = estimate))
gg.beta <- gg.beta + geom_boxplot(aes(fill=estimator))
gg.beta <- gg.beta + facet_wrap(~sample.size)
gg.beta


gg.sd <- ggplot(dt.res, aes(y = sd))
gg.sd <- gg.sd + geom_boxplot(aes(fill=estimator, x = as.factor(n)))
gg.sd <- gg.sd + geom_point(aes(y=empirical.sd, x = as.factor(n)), shape = 2, size = 2) + geom_line(aes(y=empirical.sd, x = as.factor(n), group=estimator))
gg.sd <- gg.sd + facet_grid(~estimator) + xlab("sample size")
gg.sd



##----------------------------------------------------------------------
### tempo.R ends here
