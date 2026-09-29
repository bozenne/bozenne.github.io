### simulation.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep  2 2026 (13:45) 
## Version: 
## Last-Updated: sep  2 2026 (18:41) 
##           By: Brice Ozenne
##     Update #: 17
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:


## * R packages
library(riskRegression)
library(pbapply)
library(ggplot2)

fitEstimator <- function(data){

    ## ** normalize data
    data$X <- as.factor(data$X)
    data$Z2 <- abs(data$Z)

    ## ** estimators
    e.AIPTW1 <- ate(Y ~ X + Z, treatment = X ~ Z, se = FALSE, data = data, verbose = FALSE)
    e.AIPTW2 <- ate(Y ~ X + Z2, treatment = X ~ Z2, se = FALSE, data = data, verbose = FALSE)
    e.tt <- t.test(Y ~ X, data = data)
    e.lmGaus1 <- lm(Y ~ X + Z, data = data)
    e.lmGaus2 <- lm(Y ~ X + Z2, data = data)
    e.lmBin1 <- glm(Y ~ X + Z, family = binomial(link = "identity"), data = data)
    ci.lmBin1 <- confint(e.lmBin1)

    e.prop1 <- glm(X ~ Z, family = binomial(link = "logit"), data = data)
    data$IPTW1 <- (data$X==1)/predict(e.prop1, type = "response")+(data$X==0)/(1-predict(e.prop1, type = "response"))
    e.proplmGaus1 <- lm(Y ~ X + Z, data = data, weights  = data$IPTW1)
    e.proplmBin1 <- glm(Y ~ X + Z, data = data, family = binomial(link = "identity"), weights  = data$IPTW1)
    ci.proplmBin1 <- confint(e.proplmBin1)

    e.prop2 <- glm(X ~ Z2, family = binomial(link = "logit"), data = data)
    data$IPTW2 <- (data$X==1)/predict(e.prop2, type = "response")+(data$X==0)/(1-predict(e.prop2, type = "response"))
    e.proplmGaus2 <- lm(Y ~ X + Z, data = data, weights  = data$IPTW2)
    e.proplmBin2 <- glm(Y ~ X + Z, data = data, family = binomial(link = "identity"), weights  = data$IPTW2)
    ci.proplmBin2 <- confint(e.proplmBin2)

    ## ** gather results
    out <- rbind(data.frame(name = "AIPTW1", propensity.adj = "Z", outcome.adj = "Z", outcome.model = "glm", outcome.link = "logit", standardisation = TRUE,
                            estimate = coef(e.AIPTW1, type = "diffRisk"), lower = NA, upper = NA),
                 data.frame(name = "AIPTW2", propensity.adj = "abs(Z)", outcome.adj = "abs(Z)", outcome.model = "glm", outcome.link = "logit", standardisation = TRUE,
                            estimate = coef(e.AIPTW2, type = "diffRisk"), lower = NA, upper = NA),
                 data.frame(name = "t-test", propensity.adj = "none", outcome.adj = "none", outcome.model = "t.test", outcome.link = "identity", standardisation = FALSE,
                            estimate = diff(e.tt$estimate), lower = -e.tt$conf.int[2], upper = -e.tt$conf.int[1]),
                 data.frame(name = "lmGaus1", propensity.adj = "none", outcome.adj = "Z", outcome.model = "lm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.lmGaus1)["X1"], lower = confint(e.lmGaus1)["X1",1], upper = confint(e.lmGaus1)["X1",2]),
                 data.frame(name = "lmGaus2", propensity.adj = "none", outcome.adj = "abs(Z)", outcome.model = "lm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.lmGaus2)["X1"], lower = confint(e.lmGaus2)["X1",1], upper = confint(e.lmGaus2)["X1",2]),
                 data.frame(name = "lmBin1", propensity.adj = "none", outcome.adj = "Z", outcome.model = "glm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.lmBin1)["X1"], lower = ci.lmBin1["X1",1], upper = ci.lmBin1["X1",2]),
                 data.frame(name = "proplmGaus1", propensity.adj = "Z", outcome.adj = "Z", outcome.model = "lm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.proplmGaus1)["X1"], lower = confint(e.proplmGaus1)["X1",1], upper = confint(e.proplmGaus1)["X1",2]),
                 data.frame(name = "proplmGaus2", propensity.adj = "abs(Z)", outcome.adj = "Z", outcome.model = "lm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.proplmGaus2)["X1"], lower = confint(e.proplmGaus2)["X1",1], upper = confint(e.proplmGaus2)["X1",2]),
                 data.frame(name = "proplmBin1", propensity.adj = "Z", outcome.adj = "Z", outcome.model = "glm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.proplmBin1)["X1"], lower = ci.proplmBin1["X1",1], upper = ci.proplmBin1["X1",2]),
                 data.frame(name = "proplmBin2", propensity.adj = "abs(Z)", outcome.adj = "Z", outcome.model = "glm", outcome.link = "identity", standardisation = FALSE,
                            estimate = coef(e.proplmBin2)["X1"], lower = ci.proplmBin2["X1",1], upper = ci.proplmBin2["X1",2])
                 )

    rownames(out) <- NULL
    attr(out,"model") <- list(AIPTW1 = e.AIPTW1,
                              AIPTW2 = e.AIPTW2,
                              tt = e.tt,
                              lmGaus1 = e.lmGaus1,
                              lmGaus2 = e.lmGaus2,
                              lmBin1 = e.lmBin1,
                              proplmGaus1 = e.proplmGaus1,
                              proplmBin1 = e.proplmBin1
                              )
    return(out)
}

## * settings
n.obs <- 1e5


## * setting 1: RCT (no confounder)
set.seed(1)

df1 <- data.frame(X = rbinom(n.obs, size = 1, prob = 0.5),
                 Z = rnorm(n.obs))
df1$Y <- rbinom(n.obs, size = 1, prob = plogis(df1$X + 2*df1$Z^2))

estimator1 <- fitEstimator(df1) ## AIPTW2 is the gold standard
##           name propensity.adj outcome.adj outcome.model outcome.link standardisation  estimate     lower     upper
## 1       AIPTW1              Z           Z           glm        logit            TRUE 0.1355572        NA        NA
## 2       AIPTW2         abs(Z)      abs(Z)           glm        logit            TRUE 0.1348582        NA        NA
## 3       t-test           none        none        t.test     identity           FALSE 0.1355572 0.1307070 0.1404075
## 4      lmGaus1           none           Z            lm     identity           FALSE 0.1355573 0.1307068 0.1404078
## 5      lmGaus2           none      abs(Z)            lm     identity           FALSE 0.1349240 0.1303515 0.1394966
## 6       lmBin1           none           Z           glm     identity           FALSE 0.1355580 0.1307086 0.1404093
## 7  proplmGaus1              Z           Z            lm     identity           FALSE 0.1355572 0.1307068 0.1404076
## 8  proplmGaus2         abs(Z)           Z            lm     identity           FALSE 0.1349239 0.1300738 0.1397740
## 9   proplmBin1              Z           Z           glm     identity           FALSE 0.1355583 0.1321289 0.1389890
## 10  proplmBin2         abs(Z)           Z           glm     identity           FALSE 0.1349246 0.1314955 0.1383548

## *** setting 2: confounder
set.seed(1)

df2 <- data.frame(Z = rnorm(n.obs))
df2$X <- rbinom(n.obs, size = 1, prob = plogis(abs(df2$Z)))
df2$Y <- rbinom(n.obs, size = 1, prob = plogis(0.5*df2$X + 3*abs(df2$Z)))

estimator2 <- fitEstimator(df2) ## AIPTW2 is the gold standard
##         name propensity.adj outcome.adj outcome.model outcome.link standardisation   estimate      lower      upper
## 1       AIPTW1              Z           Z           glm        logit            TRUE 0.11342936         NA         NA
## 2       AIPTW2         abs(Z)      abs(Z)           glm        logit            TRUE 0.05051500         NA         NA
## 3       t-test           none        none        t.test     identity           FALSE 0.11342926 0.10847107 0.11838744
## 4      lmGaus1           none           Z            lm     identity           FALSE 0.11342805 0.10898093 0.11787516
## 5      lmGaus2           none      abs(Z)            lm     identity           FALSE 0.06342399 0.05903905 0.06780892
## 6       lmBin1           none           Z           glm     identity           FALSE 0.11343364 0.10849080 0.11840740
## 7  proplmGaus1              Z           Z            lm     identity           FALSE 0.11342943 0.10901658 0.11784227
## 8  proplmGaus2         abs(Z)           Z            lm     identity           FALSE 0.05020197 0.04593430 0.05446965
## 9   proplmBin1              Z           Z           glm     identity           FALSE 0.11343406 0.11031501 0.11655657
## 10  proplmBin2         abs(Z)           Z           glm     identity           FALSE 0.05020663 0.04718970 0.05322421
attr(estimator2,"model")

## *** setting 3: confounder & effect modifier
set.seed(1)

df3 <- data.frame(Z = rnorm(n.obs))
df3$X <- rbinom(n.obs, size = 1, prob = plogis(abs(df3$Z)))
df3$Y <- rbinom(n.obs, size = 1, prob = plogis(0.5*df3$X + 2*abs(df3$Z) + df3$X*abs(df3$Z)))

estimator3 <- fitEstimator(df3)
##           name propensity.adj outcome.adj outcome.model outcome.link standardisation  estimate     lower     upper
## 1       AIPTW1              Z           Z           glm        logit            TRUE 0.1732782        NA        NA
## 2       AIPTW2         abs(Z)      abs(Z)           glm        logit            TRUE 0.1081381        NA        NA
## 3       t-test           none        none        t.test     identity           FALSE 0.1732762 0.1679605 0.1785919
## 4      lmGaus1           none           Z            lm     identity           FALSE 0.1732759 0.1686339 0.1779179
## 5      lmGaus2           none      abs(Z)            lm     identity           FALSE 0.1235399 0.1189440 0.1281358
## 6       lmBin1           none           Z           glm     identity           FALSE 0.1732813 0.1679781 0.1786098
## 7  proplmGaus1              Z           Z            lm     identity           FALSE 0.1732785 0.1686027 0.1779543
## 8  proplmGaus2         abs(Z)           Z            lm     identity           FALSE 0.1078671 0.1032909 0.1124433
## 9   proplmBin1              Z           Z           glm     identity           FALSE 0.1732803 0.1699750 0.1765885
## 10  proplmBin2         abs(Z)           Z           glm     identity           FALSE 0.1078685 0.1046337 0.1111040

attr(estimator3,"model")

## *** test
set.seed(1)

dfT <- data.frame(Z = rnorm(n.obs))
dfT$X <- rbinom(n.obs, size = 1, prob = plogis(abs(dfT$Z)))
dfT$Y <- rbinom(n.obs, size = 1, prob = plogis(-1 + 2*abs(dfT$Z)*dfT$X))
## c(mean(dfT$Y[dfT$X==0]), mean(dfT$Y[dfT$X==1]))
estimatorT <- fitEstimator(dfT) ## AIPTW2 is the gold standard
estimatorT


##----------------------------------------------------------------------
### simulation.R ends here
