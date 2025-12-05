### estimand.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep 29 2025 (15:10) 
## Version: 
## Last-Updated: okt  1 2025 (11:08) 
##           By: Brice Ozenne
##     Update #: 60
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(lava)
library(data.table)

## * Simulate data
## A: treatment
## M: depression
## L: gene, experiencing stroke post-baseline, environmental factors, ...
## Y: cognition  (continuous scale)
## Ybin: cognition (binary: low vs. high)
n <- 1e5

set.seed(1)
dtSim <- data.table(A = rbinom(n, size = 1, p = 0.75),
                    L = rbinom(n, size = 1, p = 0.6))
dtSim[, M := rbinom(n, size = 1, p = 0.1 + 0.2*A + 0.5*L)]
dtSim[, Y := 0.5 + 0.3*A - 0.2*L - 0.25*A*M]
dtSim[, Ybin := rbinom(n, size = 1, p = Y)]

## Marginal
dtSim.col3 <- dtSim[,.(.N, pc = 100*.N/n, E.Y = mean(Y), E.Ybin = mean(Ybin)), by = c("A","L","M")]
setkeyv(dtSim.col3, c("A","L","M"))

## Conditional (P[L,M|A=a],P[L|A=a,M=m],P[L|A=a])
dtSim.col3 <- merge(dtSim.col3,
                    merge(dtSim[,cbind(.SD[,.(count=.N),by=c("L")],n=.N), by = c("A")][, .(L.Aa = 100*(count/n)),by=c("A","L")],
                        merge(dtSim[,cbind(.SD[,.(count=.N),by=c("L","M")],n=.N), by = "A"][, .(L.M.Aa = 100*(count/n)),by=c("A","L","M")],
                              dtSim[,cbind(.SD[,.(count=.N),by=c("L")],n=.N), by = c("A","M")][, .(L.Mm.Aa = 100*(count/n)),by=c("A","L","M")],
                              by = c("A","L","M")),
                        by = c("A","L")),
                    by = c("A","L","M"))
print(dtSim.col3, digits = 4, class = FALSE)
##    A L M     N     pc  E.Y E.Ybin L.M.Aa.x L.Mm.Aa.x  L.Aa L.M.Aa.y L.Mm.Aa.y
## 1: 0 0 0 18117 18.117 0.50 0.4956   36.228     60.17 40.26   36.228     60.17
## 2: 0 0 1  2014  2.014 0.50 0.5258    4.027     10.12 40.26    4.027     10.12
## 3: 0 1 0 11995 11.995 0.30 0.3021   23.986     39.83 59.74   23.986     39.83
## 4: 0 1 1 17882 17.882 0.30 0.3046   35.758     89.88 59.74   35.758     89.88
## 5: 1 0 0 13910 13.910 0.80 0.7983   27.824     69.97 40.00   27.824     69.97
## 6: 1 0 1  6088  6.088 0.55 0.5458   12.178     20.22 40.00   12.178     20.22
## 7: 1 1 0  5970  5.970 0.60 0.6094   11.942     30.03 60.00   11.942     30.03
## 8: 1 1 1 24024 24.024 0.35 0.3465   48.056     79.78 60.00   48.056     79.78

## * Treatment policy strategy (ignoring depression)
## ** continuous endpoint
0.3 - 0.25 * (0.6*(0.1+0.2+0.5)+0.4*(0.1+0.2))
## [1] 0.15
summary(lm(Y ~ A, data = dtSim))
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.3805111  0.0006789   560.4   <2e-16 ***
## A           0.1489096  0.0009603   155.1   <2e-16 ***

dtSim.ITT <- dtSim.col3[, .(estimate = sum(E.Y * L.M.Aa)), by = "A"]
rbind(dtSim.ITT, data.frame(A="1 vs. 0", estimate = diff(dtSim.ITT$estimate)))
##          A estimate
##     <char>    <num>
## 1:       0 38.05111
## 2:       1 52.94207
## 3: 1 vs. 0 14.89096

## ** binary endpoint
summary(glm(Ybin ~ A, data = dtSim, family = binomial(link = "identity")))
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) 0.382099   0.002173  175.85   <2e-16 ***
## A           0.145786   0.003116   46.79   <2e-16 ***

dtSim.ITTbin <- dtSim.col3[, .(estimate = sum(E.Ybin * L.M.Aa)), by = "A"]
rbind(dtSim.ITTbin, data.frame(A="1 vs. 0", estimate = diff(dtSim.ITTbin$estimate)))
##          A estimate
##     <char>    <num>
## 1:       0 38.20989
## 2:       1 52.78845
## 3: 1 vs. 0 14.57856

## * On treatment strategy
## ** Among non-depressed individuals
summary(glm(Ybin ~ A, data = dtSim[dtSim$M==0,], family = binomial(link = "identity")))
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) 0.418504   0.002843  147.21   <2e-16 ***
## A           0.323095   0.004210   76.75   <2e-16 ***

summary(lm(Y ~ A, data = dtSim[dtSim$M==0,]))
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.4203308  0.0005502   763.9   <2e-16 ***
## A           0.3196089  0.0008726   366.3   <2e-16 ***

dtSim.onHealthy <- dtSim.col3[M==0,.(estimate = sum(E.Ybin * L.Mm.Aa)), by = "A"]
rbind(dtSim.onHealthy, data.frame(A="1 vs. 0", estimate = diff(dtSim.onHealthy$estimate)))
##          A estimate
##     <char>    <num>
## 1:       0 41.85043
## 2:       1 74.15996
## 3: 1 vs. 0 32.30953

## ** Among depressed individuals
summary(glm(Ybin ~ A, data = dtSim[dtSim$M==1,], family = binomial(link = "identity")))
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) 0.327000   0.003326   98.32   <2e-16 ***
## A           0.059789   0.004352   13.74   <2e-16 ***

summary(lm(Y ~ A, data = dtSim[dtSim$M==1,]))
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.3202453  0.0005177   618.5   <2e-16 ***
## A           0.0701904  0.0006672   105.2   <2e-16 ***

dtSim.onDepressed <- dtSim.col3[M==1,.(estimate = sum(E.Ybin * L.Mm.Aa)), by = "A"]
rbind(dtSim.onDepressed, data.frame(A="1 vs. 0", estimate = diff(dtSim.onDepressed$estimate)))
##          A  estimate
##     <char>     <num>
## 1:       0 32.700040
## 2:       1 38.678932
## 3: 1 vs. 0  5.978892

## * Hypothetical strategy

## ** Among non-depressed individuals
elm.hypoHealthy2 <- glm(Ybin ~ A*L, data = dtSim[dtSim$M==0,], family = binomial(link = "identity"))
dtSim.hypoHealthy2 <- rbind(data.frame(A="0", estimate = mean(predict(elm.hypoHealthy2, newdata = dtSim[dtSim$A==0,]))),
                            data.frame(A="1", estimate = mean(predict(elm.hypoHealthy2, newdata = dtSim[dtSim$A==1,]))))
rbind(dtSim.hypoHealthy2, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoHealthy2$estimate)))
##         A  estimate
## 1       0 0.3790599
## 2       1 0.6807877
## 3 1 vs. 0 0.3017277

elm.hypoHealthy2c <- lm(Y ~ A*L, data = dtSim[dtSim$M==0,])
dtSim.hypoHealthy2c <- rbind(data.frame(A="0", estimate = mean(predict(elm.hypoHealthy2c, newdata = dtSim[dtSim$A==0,]))),
                            data.frame(A="1", estimate = mean(predict(elm.hypoHealthy2c, newdata = dtSim[dtSim$A==1,]))))
rbind(dtSim.hypoHealthy2c, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoHealthy2c$estimate)))
##         A  estimate
## 1       0 0.3797257
## 2       1 0.6804362
## 3 1 vs. 0 0.3007105

dtSim.hypoHealthy <- dtSim.col3[M==0 ,.(estimate = sum(E.Ybin * L.Aa)), by = "A"]
rbind(dtSim.hypoHealthy, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoHealthy$estimate)))
##         A estimate
##     <char>    <num>
## 1:       0 37.90599
## 2:       1 68.07877
## 3: 1 vs. 0 30.17277


## ** Among depressed individuals
elm.hypoDepressed2 <- glm(Ybin ~ A*L, data = dtSim[dtSim$M==1,], family = binomial(link = "identity"))
dtSim.hypoDepressed2 <- rbind(data.frame(A="0", estimate = mean(predict(elm.hypoDepressed2, newdata = dtSim[dtSim$A==0,]))),
                              data.frame(A="1", estimate = mean(predict(elm.hypoDepressed2, newdata = dtSim[dtSim$A==1,]))))
rbind(dtSim.hypoDepressed2, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoDepressed2$estimate)))
##         A   estimate
## 1       0 0.38326963
## 2       1 0.43230514
## 3 1 vs. 0 0.04903551

elm.hypoDepressed2c <- lm(Y ~ A*L, data = dtSim[dtSim$M==1,])
dtSim.hypoDepressed2c <- rbind(data.frame(A="0", estimate = mean(predict(elm.hypoDepressed2c, newdata = dtSim[dtSim$A==0,]))),
                              data.frame(A="1", estimate = mean(predict(elm.hypoDepressed2c, newdata = dtSim[dtSim$A==1,]))))
rbind(dtSim.hypoDepressed2c, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoDepressed2c$estimate)))
##         A   estimate
## 1       0 0.37972569
## 2       1 0.43043620
## 3 1 vs. 0 0.05071052

dtSim.hypoDepressed <- dtSim.col3[M==1 ,.(estimate = sum(E.Ybin * L.Aa)), by = "A"]
rbind(dtSim.hypoDepressed, data.frame(A="1 vs. 0", estimate = diff(dtSim.hypoDepressed$estimate)))
##          A  estimate
##     <char>     <num>
## 1:       0 38.326963
## 2:       1 43.230514
## 3: 1 vs. 0  4.903551

## * Principal strata strategy

## ** Among non-depressed individuals under treatment
elm.PShealthy <- glm(Ybin ~ A*L, data = dtSim[dtSim$M==0,], family = binomial(link = "identity"))
dtSim.PShealthy <- rbind(data.frame(popA = "1", A="0", estimate = mean(predict(elm.PShealthy, newdata = transform(dtSim[dtSim$A==1 & dtSim$M==0,], A = 0)))),
                           data.frame(popA = "1", A="1", estimate = mean(predict(elm.PShealthy, newdata = transform(dtSim[dtSim$A==1 & dtSim$M==0,], A = 1)))),
                           data.frame(popA = "0", A="0", estimate = mean(predict(elm.PShealthy, newdata = transform(dtSim[dtSim$A==0 & dtSim$M==0,], A = 0)))),
                           data.frame(popA = "0", A="1", estimate = mean(predict(elm.PShealthy, newdata = transform(dtSim[dtSim$A==0 & dtSim$M==0,], A = 1)))))
rbind(dtSim.PShealthy,
      data.frame(popA = "1", A="1 vs. 0", estimate = diff(dtSim.PShealthy[dtSim.PShealthy$popA=="1","estimate"])),
      data.frame(popA = "0", A="1 vs. 0", estimate = diff(dtSim.PShealthy[dtSim.PShealthy$popA=="0","estimate"])))
##   popA       A  estimate
## 1    1       0 0.4412875
## 2    1       1 0.7410866
## 3    0       0 0.4201038
## 4    0       1 0.7203164
## 5    1 1 vs. 0 0.2997991
## 6    0 1 vs. 0 0.3002126

## ** Among depressed individuals
elm.PSdepressed <- glm(Ybin ~ A*L, data = dtSim[dtSim$M==1,], family = binomial(link = "identity"))
dtSim.PSdepressed <- rbind(data.frame(popA = "1", A="0", estimate = mean(predict(elm.PSdepressed, newdata = transform(dtSim[dtSim$A==1 & dtSim$M==1,], A = 0)))),
                           data.frame(popA = "1", A="1", estimate = mean(predict(elm.PSdepressed, newdata = transform(dtSim[dtSim$A==1 & dtSim$M==1,], A = 1)))),
                           data.frame(popA = "0", A="0", estimate = mean(predict(elm.PSdepressed, newdata = transform(dtSim[dtSim$A==0 & dtSim$M==1,], A = 0)))),
                           data.frame(popA = "0", A="1", estimate = mean(predict(elm.PSdepressed, newdata = transform(dtSim[dtSim$A==0 & dtSim$M==1,], A = 1)))))
rbind(dtSim.PSdepressed,
      data.frame(popA = "1", A="1 vs. 0", estimate = diff(dtSim.PSdepressed[dtSim.PSdepressed$popA=="1","estimate"])),
      data.frame(popA = "0", A="1 vs. 0", estimate = diff(dtSim.PSdepressed[dtSim.PSdepressed$popA=="0","estimate"])))
##   popA       A   estimate
## 1    1       0 0.34181956
## 2    1       1 0.39298113
## 3    0       0 0.32013136
## 4    0       1 0.37277051
## 5    1 1 vs. 0 0.05116157
## 6    0 1 vs. 0 0.05263916

## * Mediation strategy
## Total = average hypothetical scenario over treatment
eMed <- estimate(lvm(Y~A+M+L,M~A+L), data = dtSim)
effects(eMed, Y~A)
##          Estimate   Std.Err z value Pr(>|z|)
## Total     0.14957 0.0005123  291.97        0
## Direct    0.17499 0.0003979  439.83        0
## Indirect -0.02542 0.0003483  -72.99        0
## Y~M~A    -0.02542 0.0003483  -72.99        0
##                      Estimate    2.5%   97.5%
## Mediation proportion    -0.17 -0.1753 -0.1646

eMed0 <- estimate(lvm(Y~A+M,M~A), data = dtSim)
effects(eMed0, Y~A)
##          Estimate   Std.Err z value Pr(>|z|)
## Total     0.14891 0.0009602  155.07        0
## Direct    0.19487 0.0006761  288.22        0
## Indirect -0.04596 0.0007094  -64.79        0
## Y~M~A    -0.04596 0.0007094  -64.79        0

## dtSim$Ybin.f <- as.factor(dtSim$Ybin)
## eMed.f <- estimate(lvm(Ybin.f~A+M+L,M~A+L), data = dtSim)
## effects(eMed.f, Ybin.f~A)

##----------------------------------------------------------------------
### estimand.R ends here
