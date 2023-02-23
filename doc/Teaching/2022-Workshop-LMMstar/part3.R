## * Packages
library(LMMstar)
library(ggplot2)
library(optimx)
library(lava)

## * Load data
data(gastricbypassL, package = "LMMstar")
data(abetaW, package = "LMMstar")
data(abetaL, package = "LMMstar")
data(ckdL, package = "LMMstar")

## * 1. Optimization
## ** 1.a Type of optimizer
eLMM.abeta_gls <- lmm(fast ~ group*year,
                      repetition = ~year|id,
                      structure = UN(~group),
                      data = abetaL, control = list(optimizer = "gls"))
eLMM.abeta_gls
##     Linear Mixed Model with a stratified unstructured covariance matrix 

## outcome/cluster/time: fast/id/year 
## data                : 245 observations and distributed in 131 clusters 
## parameters          : 4 mean ((Intercept):groupBD year1:groupBD (Intercept):groupHC year1:groupHC) 
##                       4 variance (sigma:BD sigma:HC k.1:BD k.1:HC) 
##                       2 correlation (rho(0,1):BD rho(0,1):HC) 
## log-restr.likelihood: -729.615402908917 

eLMM.abeta_fs <- lmm(fast ~ group*year,
                     repetition = ~year|id,
                     structure = UN(~group),
                     data = abetaL, control = list(optimizer = "FS"))
eLMM.abeta_fs
##     Linear Mixed Model with a stratified unstructured covariance matrix 

## outcome/cluster/time: fast/id/year 
## data                : 245 observations and distributed in 131 clusters 
## parameters          : 4 mean ((Intercept) groupHC year1 groupHC:year1) 
##                       4 variance (sigma:BD sigma:HC k.1:BD k.1:HC) 
##                       2 correlation (rho(0,1):BD rho(0,1):HC) 
## log-restr.likelihood: -729.615402908918 
## convergence         : TRUE (3 iterations) 

eLMM.abeta_BFGS <- lmm(fast ~ group*year,
                       repetition = ~year|id,
                       structure = UN(~group),
                       data = abetaL, control = list(optimizer = "BFGS"))
eLMM.abeta_BFGS
##     Linear Mixed Model with a stratified unstructured covariance matrix 

## outcome/cluster/time: fast/id/year 
## data                : 245 observations and distributed in 131 clusters 
## parameters          : 4 mean ((Intercept) groupHC year1 groupHC:year1) 
##                       4 variance (sigma:BD sigma:HC k.1:BD k.1:HC) 
##                       2 correlation (rho(0,1):BD rho(0,1):HC) 
## log-restr.likelihood: -729.615403110658 
## convergence         : TRUE (evaluations: 42 likelihood, 13 score) 

## ** 1.b Following progress
tempo <- lmm(fast ~ group*year,
            repetition = ~year|id,
            structure = UN(~group),
            data = abetaL, control = list(optimizer = "FS", trace = 3))

tempo <- lmm(fast ~ group*year,
             repetition = ~year|id,
             structure = UN(~group),
             data = abetaL, control = list(optimizer = "FS", trace = 5))

## ** 1.c Initialization
solution <- coef(eLMM.abeta_fs, effects = "all")
lmm(fast ~ group*year,
    repetition = ~year|id,
    structure = UN(~group),
    data = abetaL, control = list(optimizer = "FS", init = solution))

## ** 1.d Number of iterations
earlyStop <- lmm(fast ~ group*year,
                 repetition = ~year|id,
                 structure = UN(~group),
                 data = abetaL, control = list(optimizer = "FS", n.iter = 2))
earlyStop

## ** 1.e Profile likelihood (ADVANCED, if time permits)

n.points <- 250
eTheta <- coef(eLMM.abeta_fs, effects = "all")
eTheta.type <- stats::setNames(eLMM.abeta_fs$design$param$type, eLMM.abeta_fs$design$param$name)

ls.profileLik <- lapply(names(eTheta), function(iParam){ ## iParam <- names(eTheta)[1]
    iValue <- eTheta[iParam]
    seqValue <- switch(eTheta.type[iParam],
                       "mu" = seq(iValue-5,iValue+5,length.out=n.points),
                       "sigma" = exp(seq(log(iValue)-1,log(iValue)+1,length.out=n.points)),
                       "k" = exp(seq(log(iValue)-1,log(iValue)+1,length.out=n.points)),
                       "rho" = tanh(seq(atanh(iValue)-1,atanh(iValue)+1,length.out=n.points)))
    data.frame(param = iParam,
               value = seqValue,
               logLik = sapply(seqValue, function(iiValue){p <- eTheta; p[iParam] <- iiValue; logLik(eLMM.abeta_fs, p = p)}),
               score = sapply(seqValue, function(iiValue){p <- eTheta; p[iParam] <- iiValue; score(eLMM.abeta_fs, effects = "all", p = p)[iParam]}))
})
df.profileLik <- do.call(rbind,ls.profileLik)
df.profileLik$param <- factor(df.profileLik$param, levels = unique(df.profileLik$param))
ggplot(df.profileLik, aes(x=value,y=logLik)) + geom_line() + facet_wrap(~param, scales="free", nrow = 2)

## ** Exercise 1
eLMM.debug <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id, structure = "UN",
              control = list(optimizer = "FS"))
## [Q] (strange) model for which the optimization seems to fail: can you fix it?

## *** attempt 1: change optimizer
eLMM.sol1 <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id, structure = "UN",
              control = list(optimizer = "???"))

## *** attempt 2: increase number of iterations
eLMM.sol2 <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id,
              control = list(optimizer = "FS", ?????))

logLik(eLMM.sol1)-logLik(eLMM.sol2)
coef(eLMM.sol1, effects = "all") - coef(eLMM.sol2, effects = "all")

## *** attempt 3: change initialization value
## dput(coef(eLMM.debug, effects = "all"))

betterInit <- c(`(Intercept)` = ??,
                glucagonAUC = ??, 
                sigma = ??,
                k.2 = ??, 
                k.3 = ??,
                k.4 = ??, 
                `rho(1,2)` = ??,
                `rho(1,3)` = ??, 
                `rho(1,4)` = ??,
                `rho(2,3)` = ??, 
                `rho(2,4)` = ??,
                `rho(3,4)` = ??
                )
eLMM.fail <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id,
                 control = list(optimizer = "FS", ?????))

## *** attempt 4: simplify model
eLMM.sol4 <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id,
                 structure = ??, control = list(optimizer = "FS"))

## [Q] what parametrization are we achieving with the model? Compared to "usual" CS or UN structure?
gastricbypassL$baseline <- gastricbypassL$visit %in% 1:2
eLMM.sol4.bis <- lmm(weight ~ glucagonAUC, data = gastricbypassL, repetition = ~visit|id,
                     structure = CS(~baseline), control = list(optimizer = "FS", n.iter = 1000))
eLMM.sol4.bis 
cov2cor(sigma(eLMM.sol4.bis))

## * 2. Statistical inference
eLMM.abeta <- lmm(fast ~ group*year,
                  repetition = ~year|id,
                  structure = UN(~group),
                  data = abetaL, control = list(optimizer = "FS"))

## extract model parameters
eTHETA.abeta <- coef(eLMM.abeta, effects = "all")
## extract uncertainty about the parameters
eSIGMA.abeta <- vcov(eLMM.abeta, effects = "all")

## ** 2.a test linear hypotheses (mean structure - formula interface)
eANOVA.default <- anova(eLMM.abeta)
eSANOVA.default <- summary(eANOVA.default)

## user-specific via formula
eANOVA.group <- anova(eLMM.abeta, effects = c("baseline" = "groupHC=0",
                                              "follow-up" = "groupHC+groupHC:year1=0"))
eSANOVA.group <- summary(eANOVA.group)
## extract contrast matrix
attr(eANOVA.group$all,"glht")[[1]]$linfct

## ** 2.b test linear hypotheses (mean structure - constrast matrix)
## build contrast matrix
C.group <- matrix(0, nrow = 2, ncol = length(eTHETA.abeta), dimnames = list(c("baseline","follow-up"), names(eTHETA.abeta)) )
C.group["baseline","groupHC"] <- 1
C.group["follow-up",c("groupHC","groupHC:year1")] <- 1

## via ANOVA
eANOVA.group2 <- anova(eLMM.abeta, effects = C.group)
summary(eANOVA.group2, method = "none")

## manually
F.group <- t(C.group %*% eTHETA.abeta) %*% solve(C.group %*% eSIGMA.abeta %*% t(C.group)) %*% (C.group %*% eTHETA.abeta) / 2
c(F.group,1 - pchisq(F.group, df = 2))

## ** 2.c test linear hypotheses (variance/covariance structure)
## inference on the unconstrained scale
eANOVA.transall <- anova(eLMM.abeta, effects = c("variance","correlation"))
summary(eANOVA.transall, method = "none")

## without transformation
eANOVA.all <- anova(eLMM.abeta, effects = c("variance","correlation"),
                    transform.sigma = "none", transform.rho = "none")
summary(eANOVA.all, method = "none")


## ** Exercise 2
ckdL$treat <- ckdL$allocation
ckdL$treat[ckdL$time==0] <- "A"
ckdL$time.treat <- interaction(ckdL$time, ckdL$treat, drop=TRUE)

eLMM.ckd <- lmm(aix ~ time + time:treat,
                repetition = ~time:treat|id,
                structure = UN,
                data = ckdL, control = list(optimizer = "FS"))
plot(eLMM.ckd, color = "allocation", time.var = "time")

summary(anova(eLMM.ckd))

## [Q] Can you compute the time effect in group B
##     i.e. how in average patients differ from baseline at 12 and 24 weeks

## *** formula interface
eANOVA.timeB <- anova(eLMM.ckd, effects = c(?????,?????))
summary(eANOVA.timeB)

## check by change of reference level
ckdL.bis <- ckdL
ckdL.bis$treat <- relevel(ckdL.bis$treat,"B")
ckdL.bis$treat[ckdL.bis$time==0] <- "B"
eLMM.ckd.bis <- lmm(aix ~ time + time:treat,
                    repetition = ~time:treat|id,
                    structure = UN,
                    data = ckdL.bis, control = list(optimizer = "FS"))
summary(eLMM.ckd.bis)

## *** matrix calculation for time effect in B group
eTHETA.ckd <- coef(eLMM.ckd, effects = "all")
eSIGMA.ckd <- vcov(eLMM.ckd, effects = "all")
C.timeB <- matrix(0, nrow = 2, ncol = length(eTHETA.ckd), dimnames = list(c("baseline","follow-up"), names(eTHETA.ckd)) )
C.timeB["baseline",?????] <- ???
C.timeB["follow-up",?????] <- ???

eANOVA.timeB2 <- anova(eLMM.abeta, effects = C.group)
summary(eANOVA.timeB2, method = "none")

## ** Exercise 2 bis (ADVANCED, if time permits)
eLMM.abeta <- lmm(fast ~ group*year,
                  repetition = ~year|id,
                  structure = UN(~group),
                  data = abetaL, control = list(optimizer = "FS"))

## extract model parameters
eTHETA.abeta <- coef(eLMM.abeta, effects = "all")
## extract uncertainty about the parameters
eSIGMA.abeta <- vcov(eLMM.abeta, effects = "all")

## [Q] Can you test for a difference in baseline variance between groups?
## [Q] Can you test for a difference in correlation between groups?
##     - without transformation
##     - on the log/atanh scale

## *** manually compute contrasts

c(eTHETA.abeta["???"] - eTHETA.abeta["???"], log ????)
## 9.719647 1.865112 
c(eTHETA.abeta["???"] - eTHETA.abeta["???"], atanh ????)
## 0.4496886  0.6668606 

## *** formula interface
eANOVA.logvar <- anova(eLMM.abeta, effects = c("???","???","???"))
summary(eANOVA.logvar, method = "none")
##                      estimate        se        df     lower  upper p.value    
## sigma:BD             2.442425  0.076249 79.899976  2.290681 2.5942 < 1e-05 ***
## sigma:HC             0.577312  0.107833 42.433327  0.359763 0.7949 < 1e-05 ***
## sigma:BD - sigma:HC  1.865112  0.132068 84.284520  1.602494 2.1277 < 1e-05 ***
eANOVA.var <- anova(eLMM.abeta, effects = c("???","???","???"), transform.sigma = "none")
eSANOVA.var <- summary(eANOVA.var, method = "none")
##                     estimate       se       df    lower   upper   p.value    
## sigma:BD            11.50089  0.87693 12.95539  9.60573 13.3961   < 1e-05 ***
## sigma:HC             1.78124  0.19208  6.58239  1.32115  2.2413 5.126e-05 ***
## sigma:BD - sigma:HC  9.71965  0.89772 14.16412  7.79631 11.6430   < 1e-05 ***

## DOES NOT WORK FOR THE CORRELATION BECAUSE OF SPECIAL CHARACTERS (here parentheses but white spaces are also problematic)
## anova(eLMM.abeta, effects = c("rho(0,1):BD=0","rho(0,1):HC=0","rho(0,1):BD-rho(0,1):HC=0"))

## *** contrast matrix
C.vcov <- matrix(0, nrow = 2, ncol = length(eTHETA.abeta), dimnames = list(c("log ratio sd","diff atanh(cor)"), names(eTHETA.abeta)) )
C.vcov["log ratio sd",c("???","???")] <- c(???,???)
C.vcov["diff atanh(cor)",c("???","???")] <- c(???,???)

eANOVA.transvcov <- anova(eLMM.abeta, effects = C.vcov)
summary(eANOVA.transvcov, method = "none")

## * 3. Dynamic predictions
eLMM.abeta <- lmm(fast ~ 0+group+group:year,
                  repetition = ~year|id,
                  structure = UN(~group),
                  data = abetaL, control = list(optimizer = "FS"))

eTHETA.abeta <- coef(eLMM.abeta, effects = "all")

## ** 3.a Dynamic predictions
df.incomplete <- abetaL[abetaL$id==2,c("id","group","year","fast")]
##   id group year fast
## 3  2    BD    0   32
## 4  2    BD    1   NA

## static prediction
predict(eLMM.abeta, newdata = df.incomplete[2,,drop=FALSE])
muY1 <- sum(eTHETA.abeta[c("groupBD", "groupBD:year1")])

## dynamic prediction
predict(eLMM.abeta, newdata = df.incomplete, type = "dynamic", keep.newdata = TRUE)
##   id group year fast estimate       se  df    lower    upper
## 1  2    BD    0   32       NA       NA  NA       NA       NA
## 2  2    BD    1   NA 22.79893 2.215132 Inf 18.45735 27.14051

## see slide 26 for a formula
muY1 + eTHETA.abeta["rho(0,1):BD"]*eTHETA.abeta["k.1:BD"]*(df.incomplete[1,"fast",drop=FALSE] - eTHETA.abeta["groupBD"])

Omega <- sigma(eLMM.abeta, cluster = df.incomplete)
muY1 + (Omega["0","1"]/Omega["0","0"])*(df.incomplete[1,"fast",drop=FALSE] - eTHETA.abeta["groupBD"])


## ** 3.b Mixed model as an imputation model
coef(eLMM.abeta)
##    groupBD       groupHC groupBD:year1 groupHC:year1 
## 15.5747126     1.6136364    -4.4891511    -0.8464617 

dfaug.abeta <- merge(fitted(eLMM.abeta, format = "wide", impute = TRUE),
                     abetaW, by = "id", sort = FALSE)
head(dfaug.abeta)
##   id fast.0 imputed.0   fast.1 imputed.1 sex age group episode fast0 qol0 pss0 fast1 pss1 qol1 educationyears alcohol      missingreason
## 1  1      1     FALSE  0.00000     FALSE   M  30    BD       0     1   88    9     0   NA   NA             13       0 test not performed
## 2  2     32     FALSE 22.79893      TRUE   F  55    BD       1    32   87   21    NA   NA   NA             15       0                   
## 3  3     29     FALSE 31.00000     FALSE   M  51    BD       0    29   86   23    31   27   79             21       1                   
## 4  4      1     FALSE  6.00000     FALSE   M  38    BD       0     1   96    7     6    6  101             21       1                   
## 5  5      3     FALSE  1.00000     FALSE   M  21    BD       0     3   97    1     1    5  105             12       1                   
## 6  6     22     FALSE 40.00000     FALSE   M  42    BD       1    22   70   17    40   18   68             13       0                   

tapply(dfaug.abeta$fast.1-dfaug.abeta$fast.0, dfaug.abeta$group, mean)
##         BD         HC 
## -4.4891511 -0.8464617 

## Multiple imputation
fitted(eLMM.abeta, format = "wide", impute = TRUE, se.impute = "total")[1:2,]
fitted(eLMM.abeta, format = "wide", impute = TRUE, se.impute = "total")[1:2,]

## ** Exercise 3
eLMM.ckd <- lmm(aix ~ 0 + time + time:treat,
                repetition = ~time.treat|id,
                structure = UN,
                data = ckdL, control = list(optimizer = "FS"))
plot(eLMM.ckd, color = "allocation", time.var = "time")

df.incomplete2 <- ckdL[ckdL$id==29,]
##     id allocation  sex age visit time  pwv  aix dropout treat time.treat
## 1   29          B male  51     1    0 6.05 15.5       1     A        0.A
## 52  29          B male  51     2   12 5.65 18.0       1     B       12.B
## 103 29          B male  51     3   24   NA   NA       1     B       24.B

## [Q] Can you estimate the most likely aix value for individual 29 at time 24 based on:
##     - its group membership (and time)?
##     - its group membership (and time) as weel as its observed aix values at time 0 and 12?

## *** using the predict function
## type can be "static" (default) or "dynamic" 
predict(eLMM.ckd, newdata = df.incomplete2[?,,drop=FALSE], type = ????, keep.newdata = TRUE)
predict(eLMM.ckd, newdata = df.incomplete2, type = ????, keep.newdata = TRUE)

## *** by hand
mu <- coef(eLMM.ckd)[???] + c(0,coef(eLMM.ckd)[???])
Omega <- sigma(eLMM.ckd, cluster = df.incomplete2)

mu[?] + Omega[?,???] %*% solve(Omega[???,???]) %*% (df.incomplete2[???,???] - mu[???])


