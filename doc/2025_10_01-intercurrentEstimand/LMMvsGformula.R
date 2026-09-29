### LMMvsGformula.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep 25 2026 (15:00) 
## Version: 
## Last-Updated: sep 25 2026 (18:06) 
##           By: Brice Ozenne
##     Update #: 59
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(lava)
library(LMMstar)

## * Simulate data
mSim <- lvm(Y ~ A1 + A0 + L1 + L0,
            A1 ~ A0 + L1 + L0,
            L1 ~ A0 + L0)
distribution(mSim, ~A0+A1) <- binomial.lvm(size = 1, p = 0.5)

set.seed(1)
dfW <- sim(mSim, 1e4)

## * Total effect
e.total <- lm(Y ~ A0 + L0, data = dfW)
summary(e.total)$coef
##              Estimate Std. Error   t value      Pr(>|t|)
## (Intercept) 0.4949312 0.02199994  22.49693 2.240363e-109
## A0          2.2465196 0.03102231  72.41625  0.000000e+00
## L0          2.2505741 0.01539306 146.20711  0.000000e+00

## * Naive strategy for effect among L1=0
e.naive <- lm(Y ~ A0 + L0, data = dfW[dfW$A1==0,])
summary(e.naive)$coef
##              Estimate Std. Error   t value      Pr(>|t|)
## (Intercept) -0.417234 0.03163983 -13.18699  7.815663e-39
## A0           1.719585 0.04942149  34.79428 1.382153e-229
## L0           1.747348 0.02745738  63.63854  0.000000e+00

## * G-formula strategy
dfW.0 <- data.frame(A0 = 0, A1 = 0, dfW[c("L0","L1")])
dfW.1 <- data.frame(A0 = 1, A1 = 0, dfW[c("L0","L1")])

## ** all obs
eG.lm <- lm(Y ~ A0*A1*L0 + A0*A1*L1, data = dfW)
vecG.counterfactual <- c("0" = mean(predict(eG.lm, newdata = dfW[dfW$A0==0,])),
                         "1" = mean(predict(eG.lm, newdata = dfW[dfW$A0==1,])))
c(vecG.counterfactual, diff = unname(diff(vecG.counterfactual)))
##         0         1      diff 
## 0.4483829 2.8211955 2.3728126 

## ** only pre-ICE obs
eG0.lm0 <- lm(Y ~ L0 + L1, data = dfW[dfW$A0==0 & dfW$A1==0,])
eG0.lm1 <- lm(Y ~ L0 + L1, data = dfW[dfW$A0==1 & dfW$A1==0,])
vecG0.counterfactual <- c("0" = mean(predict(eG0.lm0, newdata = dfW[dfW$A0==0,])),
                          "1" = mean(predict(eG0.lm1, newdata = dfW[dfW$A0==1,])))
c(vecG0.counterfactual, diff = unname(diff(vecG0.counterfactual)))
##            0            1         diff 
## -0.008849154  2.058201636  2.067050790 

## * MMRM strategy
dfW$Ystar <- ifelse(dfW$A1==1,NA,dfW$Y)
dfL <- reshape(dfW[,c("A0","L0","L1","Ystar")], direction = "long", timevar = "time",
               varying = c("L0","L1","Ystar"), v.names = "Y")
dfL$visit <- as.character(dfL$time)
dfL <- dfL[order(dfL$id),]

e.MMRM <- lmm(Y ~ 0 + visit + visit:A0, repetition = ~visit|id, structure = UN, data = dfL)
summary(e.MMRM)
##           estimate    se      df  lower upper p.value    
## visit1      -0.021 0.014  9999.4 -0.049 0.007 0.14788    
## visit2      -0.024  0.02 10002.2 -0.063 0.015 0.23256    
## visit3      -0.013  0.04  4815.4 -0.093 0.066 0.73874    
## visit1:A0    0.056  0.02  9999.5  0.017 0.096 0.00536  **
## visit2:A0    1.054 0.028 10001.8  0.999  1.11 < 1e-04 ***
## visit3:A0    2.091 0.059  6423.5  1.976 2.206 < 1e-04 ***

eS.MMRM <- lmm(Y ~ 0 + visit + visit:A0, repetition = ~visit|id, structure = UN(~A0), data = dfL)
summary(eS.MMRM)
##           estimate    se     df  lower upper p.value    
## visit1      -0.021 0.014 4967.7 -0.049 0.008 0.15143    
## visit2      -0.024  0.02 4969.1 -0.063 0.016 0.23535    
## visit3      -0.009 0.041   3006  -0.09 0.072 0.83073    
## visit1:A0    0.056  0.02 9990.4  0.017 0.096 0.00536  **
## visit2:A0    1.054 0.028 9995.8  0.999  1.11 < 1e-04 ***
## visit3:A0    2.067 0.075 1673.1  1.919 2.215 < 1e-04 ***

## ** decomposition of the (stratified) MMRM estimator
dfLW <- reshape(dfW[,c("A0","L0","L1","Ystar")], direction = "long", timevar = "time",
                varying = c("L1","Ystar"), v.names = "Y")
dfLW$visit <- as.character(dfLW$time)
dfLW$A0 <- as.character(dfLW$A0)
dfLW <- dfLW[order(dfLW$id),]

eDetail.MMRM <- list(lmm(Y ~ 0 + visit + visit:L0, repetition = ~visit|id, structure = UN, data = dfLW[dfLW$A0==0,]),
                     lmm(Y ~ 0 + visit + visit:L0, repetition = ~visit|id, structure = UN, data = dfLW[dfLW$A0==1,]))

## *** E[L1 | A0=a0,L0]
beta_10 <- c(coef(eDetail.MMRM[[1]])["visit1"], coef(eDetail.MMRM[[2]])["visit1"])
beta_11 <- c(coef(eDetail.MMRM[[1]])["visit1:L0"], coef(eDetail.MMRM[[2]])["visit1:L0"])

## same as lm
rbind(beta_10,beta_11) - cbind(coef(lm(L1 ~ L0, data = dfW[dfW$A0==0,])), coef(lm(L1 ~ L0, data = dfW[dfW$A0==1,])))
##                visit1        visit1
## beta_10 -6.826137e-16 -1.998401e-15
## beta_11  2.553513e-15 -2.442491e-15

## *** E[Y | A0=a0,L0,L1]
lmm.rho <- c(coef(eDetail.MMRM[[1]], effects = "correlation"), coef(eDetail.MMRM[[2]], effects = "correlation"))
lmm.k <- c(coef(eDetail.MMRM[[1]], effects = "variance")[2], coef(eDetail.MMRM[[2]], effects = "variance")[2])

beta_20 <- c(coef(eDetail.MMRM[[1]])["visit2"], coef(eDetail.MMRM[[2]])["visit2"]) - lmm.rho * lmm.k * beta_10 
beta_21 <- c(coef(eDetail.MMRM[[1]])["visit2:L0"], coef(eDetail.MMRM[[2]])["visit2:L0"]) - lmm.rho * lmm.k * beta_11
beta_22 <- lmm.rho * lmm.k 

## same as lm
rbind(beta_20,beta_21,beta_22) - cbind(coef(lm(Ystar ~ L0 + L1, data = dfW[dfW$A0==0,])), coef(lm(Ystar ~ L0 + L1, data = dfW[dfW$A0==1,])))
##                visit2        visit2
## beta_20 -5.195015e-09  1.098635e-08
## beta_21  8.943633e-09  2.939225e-08
## beta_22 -1.189370e-08 -4.203755e-08

## *** Property of 'ordinary least square'
tapply(dfW$L1, dfW$A0, mean) - (beta_10 + beta_11*tapply(dfW$L0, dfW$A0, mean))
##            0            1 
## 8.291978e-16 2.886580e-15 

## *** estimate average counterfactual
vecMMRM.counterfactual <- beta_20 + beta_21 * tapply(dfW$L0, dfW$A0, mean) + beta_22 * tapply(dfW$L1, dfW$A0, mean)
c(vecMMRM.counterfactual, diff = unname(diff(vecMMRM.counterfactual)))
##            0            1         diff 
## -0.008849159  2.058201605  2.067050764 

model.tables(eS.MMRM)[c("visit3","visit3:A0"),"estimate"] ## 0 and diff
## [1] -0.008849155  2.067050737

c(mean(predict(eDetail.MMRM[[1]], newdata = dfLW[dfLW$A0==0 & dfLW$time==2,])),
  mean(predict(eDetail.MMRM[[2]], newdata = dfLW[dfLW$A0==1 & dfLW$time==2,])))
## [1] -0.008849159  2.058201605


## * With 2 intercurrent events
mSim2 <- lvm(Y ~ A2 + A1 + A0 + L2 + L1 + L0,
             A2 ~ A1 + A0 + L2 + L1 + L0,
             L2 ~ A1 + A0 + L1 + L0,
             A1 ~ A0 + L1 + L0,
             L1 ~ A0 + L0)
distribution(mSim2, ~A0+A1+A2) <- binomial.lvm(size = 1, p = 0.5)

set.seed(1)
dfW2 <- sim(mSim2, 1e4)


## ** G-formula strategy
eGY.lm0 <- lm(Y ~ L0 + L1 + L2, data = dfW2[dfW2$A0==0 & dfW2$A1==0 & dfW2$A2==0,])
eGL2.lm0 <- lm(L2 ~ L0 + L1, data = dfW2[dfW2$A0==0 & dfW2$A1==0,])
## eGL2.lm0 <- lm(L2 ~ L0 + L1, data = dfW2[dfW2$A0==0,])

eGY.lm1 <- lm(Y ~ L0 + L1 + L2, data = dfW2[dfW2$A0==1 & dfW2$A1==0 & dfW2$A2==0,])
eGL2.lm1 <- lm(L2 ~ L0 + L1, data = dfW2[dfW2$A0==1 & dfW2$A1==0,])
## eGL2.lm1 <- lm(L2 ~ L0 + L1, data = dfW2[dfW2$A0==1,])

## *** version 1: prediction
mean(predict(eGY.lm0, newdata = cbind(dfW2[dfW2$A0==0,c("L0","L1")],
                                 data.frame(L2 = predict(eGL2.lm0, newdata = dfW2[dfW2$A0==0,])))
             ))
## [1] 0.03015946

mean(predict(eGY.lm1, newdata = cbind(dfW2[dfW2$A0==1,c("L0","L1")],
                                      data.frame(L2 = predict(eGL2.lm1, newdata = dfW2[dfW2$A0==1,])))
             ))
## [1] 3.899097

## *** version 2: explicit formula
beta_30 <- coef(eGY.lm0)
beta_20 <- coef(eGL2.lm0)
beta_31 <- coef(eGY.lm1)
beta_21 <- coef(eGL2.lm1)

vecG2.counterfactual <- c("0" = beta_30["(Intercept)"] + beta_30["L2"]*beta_20["(Intercept)"] + (beta_30["L0"]+beta_30["L2"]*beta_20["L0"])*mean(dfW2[dfW2$A0==0,"L0"]) + (beta_30["L1"]+beta_30["L2"]*beta_20["L1"])*mean(dfW2[dfW2$A0==0,"L1"]),
                          "1" = beta_31["(Intercept)"] + beta_31["L2"]*beta_21["(Intercept)"] + (beta_31["L0"]+beta_31["L2"]*beta_21["L0"])*mean(dfW2[dfW2$A0==1,"L0"]) + (beta_31["L1"]+beta_31["L2"]*beta_21["L1"])*mean(dfW2[dfW2$A0==1,"L1"])
                          )
c(vecG2.counterfactual, diff = unname(diff(vecG2.counterfactual)))
## 0.(Intercept) 1.(Intercept)          diff 
##    0.03015946    3.89909703    3.86893757 

## ** MMRM strategy
dfW2$Ystar <- ifelse(dfW2$A1==1 | dfW2$A2==1,NA,dfW2$Y)
dfW2$L2star <- ifelse(dfW2$A1==1,NA,dfW2$L2)
dfL2 <- reshape(dfW2[,c("A0","A1","A2","L0","L1","L2star","Ystar")], direction = "long", timevar = "time",
                varying = c("L0","L1","L2star","Ystar"), v.names = "Y")
dfL2$visit <- as.character(dfL2$time)
dfL2 <- dfL2[order(dfL2$id),]

eS.MMRM2 <- lmm(Y ~ 0 + visit + visit:A0, repetition = ~visit|id, structure = UN(~A0), data = dfL2,
                control = list(tol.score = 1e-3))
## mmrm(Y ~ 0 + visit + visit:A0 + us(visit|A0/id), data = transform(dfL2, id = as.factor(id), A0 = as.factor(A0), visit = as.factor(visit)))
model.tables(eS.MMRM2)
##              estimate         se        df       lower      upper    p.value
## visit1     0.01150709 0.01388109 5074.0243 -0.01570584 0.03872002 0.40715716
## visit2     0.01167776 0.01968939 5077.8415 -0.02692193 0.05027746 0.55314116
## visit3     0.03406362 0.04057576 2951.8084 -0.04549604 0.11362328 0.40125310
## visit4     0.03015946 0.07887028 2271.1003 -0.12450587 0.18482479 0.70220547
## visit1:A0 -0.03462594 0.01997869 9974.1611 -0.07378821 0.00453633 0.08310108
## visit2:A0  0.98036691 0.02813897 9991.2390  0.92520885 1.03552496 0.00000000
## visit3:A0  1.90870538 0.07055224 2088.9970  1.77034538 2.04706539 0.00000000
## visit4:A0  3.86893592 0.16061908  608.8102  3.55350121 4.18437062 0.00000000

lm(L0 ~ A0, data = dfW2)
## (Intercept)           A0  
##     0.01151     -0.03463  
lm(L1 ~ A0, data = dfW2)
## (Intercept)           A0  
##     0.01168      0.98037  
lm(L2 ~ A0, data = dfW2)
## (Intercept)           A0  
##       0.528        2.191  

lm(Y ~ A0, data = dfW2) ## not the same as expected due to missing values
## (Intercept)           A0  
##       1.638        4.625  

## ** decomposition of the (stratified) MMRM estimator
dfLW2 <- reshape(dfW2[,c("A0","L0","L1","L2star","Ystar")], direction = "long", timevar = "time",
                varying = c("L1","L2star","Ystar"), v.names = "Y")
dfLW2$visit <- as.character(dfLW2$time)
dfLW2$A0 <- as.character(dfLW2$A0)
dfLW2 <- dfLW2[order(dfLW2$id),]

eDetail.MMRM2 <- list(lmm(Y ~ 0 + visit + visit:L0, repetition = ~visit|id, structure = UN, data = dfLW2[dfLW2$A0==0,]),
                      lmm(Y ~ 0 + visit + visit:L0, repetition = ~visit|id, structure = UN, data = dfLW2[dfLW2$A0==1,]))

c(mean(predict(eDetail.MMRM2[[1]], newdata = dfLW2[dfLW2$A0==0 & dfLW2$time==3,])),
  mean(predict(eDetail.MMRM2[[2]], newdata = dfLW2[dfLW2$A0==1 & dfLW2$time==3,])))
## [1] 0.03015944 3.89909688

## *** E[L1 | A0=a0,L0]
beta2_10 <- c(coef(eDetail.MMRM2[[1]])["visit1"], coef(eDetail.MMRM2[[2]])["visit1"])
beta2_11 <- c(coef(eDetail.MMRM2[[1]])["visit1:L0"], coef(eDetail.MMRM2[[2]])["visit1:L0"])

## same as lm
rbind(beta2_10,beta2_11) - cbind(coef(lm(L1 ~ L0, data = dfW2[dfW2$A0==0,])), coef(lm(L1 ~ L0, data = dfW2[dfW2$A0==1,])))
##                 visit1       visit1
## beta2_10 -1.342513e-15 4.884981e-15
## beta2_11  2.775558e-15 2.442491e-15

## *** E[L1 | A0=a0,L0,L1]
lmm2.rho <- c(coef(eDetail.MMRM2[[1]], effects = "correlation")[1], coef(eDetail.MMRM2[[2]], effects = "correlation")[1])
lmm2.k <- c(coef(eDetail.MMRM2[[1]], effects = "variance")[2], coef(eDetail.MMRM2[[2]], effects = "variance")[2])

beta2_20 <- c(coef(eDetail.MMRM2[[1]])["visit2"], coef(eDetail.MMRM2[[2]])["visit2"]) - lmm2.rho * lmm2.k * beta2_10 
beta2_21 <- c(coef(eDetail.MMRM2[[1]])["visit2:L0"], coef(eDetail.MMRM2[[2]])["visit2:L0"]) - lmm2.rho * lmm2.k * beta2_11
beta2_22 <- lmm2.rho * lmm2.k 

## same as lm
rbind(beta2_20,beta2_21,beta2_22) - cbind(coef(lm(L2star ~ L0 + L1, data = dfW2[dfW2$A0==0,])), coef(lm(L2star ~ L0 + L1, data = dfW2[dfW2$A0==1,])))
##                 visit2        visit2
## beta2_20 -1.878259e-11 -1.288158e-11
## beta2_21  2.939249e-11 -3.007428e-11
## beta2_22 -4.063527e-11  4.284617e-11

## *** E[Y | A0=a0,L0,L1,L2]
rbind(coef(lm(Ystar ~ L0 + L1 + L2star, data = dfW2[dfW2$A0==0,])),
      coef(lm(Ystar ~ L0 + L1 + L2star, data = dfW2[dfW2$A0==1,])))
## +      (Intercept)        L0        L1    L2star
## [1,] -0.02724203 0.9651828 0.9632843 1.0288409
## [2,]  1.10314408 1.1423696 1.0165253 0.9336801

sigma(eDetail.MMRM2[[1]])[3,1:2,drop=FALSE] %*% solve(sigma(eDetail.MMRM2[[1]])[1:2,1:2,drop=FALSE])
sigma(eDetail.MMRM2[[2]])[3,1:2,drop=FALSE] %*% solve(sigma(eDetail.MMRM2[[2]])[1:2,1:2,drop=FALSE])
##           1        2
## 3 0.9632842 1.028841
##          1         2
## 3 1.016525 0.9336801





##----------------------------------------------------------------------
### LMMvsGformula.R ends here
