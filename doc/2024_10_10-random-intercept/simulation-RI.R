### simulation-RI.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: okt 22 2024 (11:03) 
## Version: 
## Last-Updated: okt 22 2024 (21:06) 
##           By: Brice Ozenne
##     Update #: 62
##----------------------------------------------------------------------
## 
### Commentary: 
## cd /projects/biostat01/people/hpl802/WorkTempo/denseSampling
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(mvtnorm)
library(lme4)
library(lmerTest)
library(pbapply)
library(ggplot2)
library(data.table)

if(FALSE){
    setwd("c:/Users/hpl802/Documents/Presentations/KU-Biostat-Coffee_Talk/2024-10-23-randomIntercept/")
}
export <- TRUE
cpus <- 100
n.sim1 <- 20000
n.sim2 <- 20000

grid1 <- expand.grid(n.obs = c(6,60),
                    n.rep = c(2,3,5,10))

grid2 <- expand.grid(n.obs = c(6,60),
                    n.rep = c(2,3,5,10))

## * Simulation functions

## ** correlation pattern
## *** compound symmetry
Sigma.CS <- function(n.rep, rho = 0.8){

    out <- rho + diag(1-rho,2*n.rep)
    attr(out, "rhoW") <- rho
    attr(out, "rhoB") <- rho
    attr(out, "rhoB.lag") <- rho
    return(out)
}
Sigma.CS(2)
##      [,1] [,2] [,3] [,4]
## [1,]  1.0  0.8  0.8  0.8
## [2,]  0.8  1.0  0.8  0.8
## [3,]  0.8  0.8  1.0  0.8
## [4,]  0.8  0.8  0.8  1.0

## *** repeated cross over
Sigma.CO <- function(n.rep, rhoW = 0.5, rhoB = 0.2, rhoB.lag = 0.1){
    block.1 <- matrix(c(1,rhoW,rhoW,1),2,2)
    block.2 <- matrix(c(rhoB,rhoB.lag,rhoB.lag,rhoB),2,2)
    ls.block2 <- lapply(1:n.rep, function(i){block.2})
    out <- do.call(rbind,lapply(1:n.rep, function(iRep){
        iLS <- ls.block2
        iLS[[iRep]] <- block.1
        do.call(cbind,iLS)
    }))
    attr(out, "rhoW") <- rhoW
    attr(out, "rhoB") <- rhoB
    attr(out, "rhoB.lag") <- rhoB.lag
    return(out)
}

Sigma.CO(2)
##      [,1] [,2] [,3] [,4]
## [1,]  1.0  0.5  0.2  0.1
## [2,]  0.5  1.0  0.1  0.2
## [3,]  0.2  0.1  1.0  0.5
## [4,]  0.1  0.2  0.5  1.0
Sigma.CO(3)
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  1.0  0.5  0.2  0.1  0.2  0.1
## [2,]  0.5  1.0  0.1  0.2  0.1  0.2
## [3,]  0.2  0.1  1.0  0.5  0.2  0.1
## [4,]  0.1  0.2  0.5  1.0  0.1  0.2
## [5,]  0.2  0.1  0.2  0.1  1.0  0.5
## [6,]  0.1  0.2  0.1  0.2  0.5  1.0

## *** dense sampling
Sigma.DS <- function(n.rep, rhoW = 0.5, rhoB = 0.2){
    block.1 <- rhoW + diag(1-rhoW,n.rep,n.rep)
    block.2 <- matrix(rhoB,n.rep,n.rep)
    out <- rbind(cbind(block.1,block.2),
                 cbind(block.2,block.1))
    attr(out, "rhoW") <- rhoW
    attr(out, "rhoB") <- rhoB
    attr(out, "rhoB.lag") <- rhoB
    return(out)
}

Sigma.DS(3)  
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  1.0  0.5  0.5  0.2  0.2  0.2
## [2,]  0.5  1.0  0.5  0.2  0.2  0.2
## [3,]  0.5  0.5  1.0  0.2  0.2  0.2
## [4,]  0.2  0.2  0.2  1.0  0.5  0.5
## [5,]  0.2  0.2  0.2  0.5  1.0  0.5
## [6,]  0.2  0.2  0.2  0.5  0.5  1.0

## ** warper
simRI <- function(seed, n.obs, n.rep, type.Sigma, ...){ ## iSim <- 1
  
    set.seed(seed)
    require(mvtnorm)
    require(lme4)
    require(lmerTest)
    
    ## *** generate correlation matrix
    if(!is.null(names(type.Sigma))){
        name <- names(type.Sigma)
    }else{
        name <- type.Sigma
    }
    type.Sigma <- match.arg(type.Sigma, c("CS","CO","DS")) ## compound symmetry, cross-over, dense sample
    Sigma <- switch(type.Sigma,
                    "CS" = Sigma.CS(n.rep, ...),
                    "CO" = Sigma.CO(n.rep, ...),
                    "DS" = Sigma.DS(n.rep, ...))
    ## deltaC <- cbind(diag(1,n.rep,n.rep),diag(-1,n.rep,n.rep))
    ## deltaC %*% Sigma %*% t(deltaC)
    
    ## *** simulate data
    M.sim <- rmvnorm(n.obs, mean = rep(0,2*n.rep), sigma = Sigma)
    dfL <- reshape(as.data.frame(M.sim), direction = "long", varying = paste0("V",1:NCOL(M.sim)), v.names = "V")
    dfL <- dfL[order(dfL$id),,drop=FALSE]
    rownames(dfL) <- NULL
    if(type.Sigma == "CO"){
        dfL$treatment <- as.numeric(dfL$time %in% (2*(1:n.rep)))
    }else{
        dfL$treatment <- as.numeric(dfL$time > n.rep)
    }
    dfL$visit <- as.factor(dfL$time)

    ## *** statistical test
    suppressMessages(e.lmer <- lmer(V ~ treatment + (1|id), data = dfL))
    if(!is.null(e.lmer@optinfo$conv$lme4$messages) && e.lmer@optinfo$conv$lme4$messages == "boundary (singular) fit: see help('isSingular')"){
        boundary.lmer <- TRUE
    }else{
        boundary.lmer <- FALSE
    }

    if(type.Sigma == "CO"){
        e.tt <- t.test(as.double(M.sim[,2*(1:n.rep)]-M.sim[,2*(1:n.rep)-1]))
        e.ttmean <- t.test(rowMeans(M.sim[,2*(1:n.rep)]) - rowMeans(M.sim[,2*(1:n.rep)-1]))
    }else{
        e.tt <- t.test(as.double(M.sim[,1:n.rep]-M.sim[,n.rep+(1:n.rep)]))
        e.ttmean <- t.test(rowMeans(M.sim[,1:n.rep]) - rowMeans(M.sim[,n.rep+(1:n.rep)]))
    }

    ## *** export
    out <- data.frame(seed = seed, n.obs = n.obs, n.rep = n.rep,
                      scenario = name,
                      structure = type.Sigma,
                      model = c("tt","ttmean","lmer"),
                      boundary = c(FALSE,FALSE,boundary.lmer),
                      estimate = c(e.tt$estimate, e.ttmean$estimate,fixef(e.lmer)["treatment"]),
                      se = c(e.tt$stderr, e.ttmean$stderr, summary(e.lmer)$coef["treatment","Std. Error"]),
                      p.value = c(e.tt$p.value, e.ttmean$p.value, summary(e.lmer)$coef["treatment","Pr(>|t|)"]),
                      rhoW = attr(Sigma,"rhoW"), rhoB = attr(Sigma,"rhoB"), rhoB.lag = attr(Sigma,"rhoB.lag")
                      )
    return(out)
}


## * Simulation study
## ** simulation 1

ls.sim1 <- pblapply(1:n.sim1, function(iSim){ ## iSim <- 1
    iOut <- NULL
    for(iG in 1:NROW(grid1)){ ## iG <- 1
        iOut <- rbind(iOut,
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("DS:0"="DS"), rhoW = 0.5, rhoB = 0),
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("DS:0.1"="DS"), rhoW = 0.5, rhoB = 0.1),
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("DS:0.2"="DS"), rhoW = 0.5, rhoB = 0.2),
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("DS:0.3"="DS"), rhoW = 0.5, rhoB = 0.3),
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("DS:0.4"="DS"), rhoW = 0.5, rhoB = 0.4),
                      simRI(iSim, n.obs = grid1[iG,"n.obs"], n.rep = grid1[iG,"n.rep"], type.Sigma = c("CS"="CS"), rho = 0.5)
                      )
    }
    return(iOut)
}, cl = cpus)

dt.sim1 <- as.data.table(do.call(rbind,ls.sim1))
dt.sim1[model == "lmer", .(boundary = mean(boundary)), by = c("scenario","n.obs","n.rep")]
dtS.sim1 <- dt.sim1[,.(n.sim = .N, rhoB = unique(rhoB), bias = mean(estimate), rejection.rate = mean(p.value <= 0.05)), by= c("scenario","model","n.obs","n.rep")]
dtS.sim1[, n := paste0("(n=",n.obs,", rep=",n.rep,")")]
dtSW.sim1 <- reshape(dtS.sim1[order(dtS.sim1$n.obs,dtS.sim1$n.rep),
                            c("scenario","rhoB","model","n","n.sim","rejection.rate")],
                    direction = "wide", timevar = "n", idvar = c("scenario","rhoB","model","n.sim"), v.names = "rejection.rate")
names(dtSW.sim1) <- gsub("rejection.rate.", " ", names(dtSW.sim1))
dtSW.sim1
##     scenario  model n.sim  (n=6, rep=2)  (n=6, rep=3)  (n=6, rep=10)  (n=60, rep=2)  (n=60, rep=3)  (n=60, rep=10)
##       <char> <char> <int>         <num>         <num>          <num>          <num>          <num>           <num>
##  1:     DS:0     tt  2000        0.1110        0.1675         0.4310         0.1085         0.1605          0.4115
##  2:     DS:0 ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
##  3:     DS:0   lmer  2000        0.1540        0.2150         0.4795         0.1415         0.2120          0.4705
##  4:   DS:0.1     tt  2000        0.1015        0.1570         0.4065         0.0995         0.1515          0.3935
##  5:   DS:0.1 ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
##  6:   DS:0.1   lmer  2000        0.1340        0.1920         0.4505         0.1275         0.1910          0.4445
##  7:   DS:0.2     tt  2000        0.0930        0.1380         0.3730         0.0930         0.1335          0.3645
##  8:   DS:0.2 ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
##  9:   DS:0.2   lmer  2000        0.1170        0.1705         0.4105         0.1135         0.1665          0.4050
## 10:   DS:0.3     tt  2000        0.0790        0.1185         0.3225         0.0845         0.1150          0.3095
## 11:   DS:0.3 ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
## 12:   DS:0.3   lmer  2000        0.0970        0.1370         0.3500         0.0985         0.1395          0.3455
## 13:   DS:0.4     tt  2000        0.0655        0.0905         0.2275         0.0675         0.0910          0.2250
## 14:   DS:0.4 ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
## 15:   DS:0.4   lmer  2000        0.0715        0.1015         0.2465         0.0800         0.1020          0.2445
## 16:       CS     tt  2000        0.0465        0.0510         0.0530         0.0465         0.0540          0.0525
## 17:       CS ttmean  2000        0.0540        0.0510         0.0525         0.0480         0.0550          0.0495
## 18:       CS   lmer  2000        0.0445        0.0510         0.0530         0.0480         0.0545          0.0520

if(export){
    saveRDS(dt.sim1, file = "RIsim1-denseSampling.rds")
    saveRDS(dtS.sim1, file = "RIsim1-summary-denseSampling.rds")
    saveRDS(dtSW.sim1, file = "RIsim1-summaryWide-denseSampling.rds")
}

## ** figure 1
## dtS.sim1 <- readRDS(file = "x:/WorkTempo/denseSampling/RIsim1-summary-denseSampling.rds")
dtS.sim1[, n.obs_label := paste0("n=",n.obs," patients")]
dtS.sim1[, n.rep_label := factor(n.rep, levels=unique(n.rep))]
dtS.sim1[, model_label := factor(model, c("ttmean","tt","lmer"), labels = c("t-test (mean)","t-test (change)","random intercept model"))]

ggRejection1 <- ggplot(dtS.sim1[n.obs==6], aes(x = rhoB, y = rejection.rate, group = n.rep_label, colour = n.rep_label))
ggRejection1 <- ggRejection1 + geom_line(linewidth=1.15) + geom_point(size = 2.5)
ggRejection1 <- ggRejection1 + facet_grid(n.obs_label~model_label)
ggRejection1 <- ggRejection1 + labs(x=expression(rho[B]), y = "rejection rate", color = expression(p[1]))
ggRejection1 <- ggRejection1 + scale_color_manual(values = unname(palette.colors()[-1]))
ggRejection1 <- ggRejection1 + theme(text = element_text(size=15), 
                           axis.line = element_line(linewidth = 1.25),
                           axis.ticks = element_line(linewidth = 2),
                           axis.ticks.length=unit(.25, "cm"),
                           legend.key.size = unit(3,"line"))
ggRejection1

if(export){
    ggsave(ggRejection1, filename = "figures/gg-Rejection1-denseSampling.pdf", width = 10, height = 5)
}

## ** simulation 2

ls.sim2 <- pblapply(1:n.sim2, function(iSim){ ## iSim <- 1
    iOut <- NULL
    for(iG in 1:NROW(grid2)){ ## iG <- 1
        iOut <- rbind(iOut,
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CO:0"="CO"), rhoW = 0.5, rhoB = 0, rhoB.lag = 0),
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CO:0.1"="CO"), rhoW = 0.5, rhoB = 0.1, rhoB.lag = 0),
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CO:0.2"="CO"), rhoW = 0.5, rhoB = 0.2, rhoB.lag = 0.1),
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CO:0.3"="CO"), rhoW = 0.5, rhoB = 0.3, rhoB.lag = 0.2),
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CO:0.4"="CO"), rhoW = 0.5, rhoB = 0.4, rhoB.lag = 0.3),
                      simRI(iSim, n.obs = grid2[iG,"n.obs"], n.rep = grid2[iG,"n.rep"], type.Sigma = c("CS"="CS"), rho = 0.5)
                      )
    }
    return(iOut)
}, cl = cpus)

dt.sim2 <- as.data.table(do.call(rbind,ls.sim2))
dt.sim2[model == "lmer", .(boundary = mean(boundary)), by = c("scenario","n.obs","n.rep")]
dtS.sim2 <- dt.sim2[,.(n.sim = .N, rhoB = unique(rhoB), bias = mean(estimate), rejection.rate = mean(p.value <= 0.05)), by= c("scenario","model","n.obs","n.rep")]
dtS.sim2[, n := paste0("(n=",n.obs,", rep=",n.rep,")")]
dtSW.sim2 <- reshape(dtS.sim2[order(dtS.sim2$n.obs,dtS.sim2$n.rep),
                            c("scenario","rhoB","model","n","n.sim","rejection.rate")],
                    direction = "wide", timevar = "n", idvar = c("scenario","rhoB","model","n.sim"), v.names = "rejection.rate")
names(dtSW.sim2) <- gsub("rejection.rate.", " ", names(dtSW.sim2))
dtSW.sim2
##     scenario  rhoB  model n.sim  (n=6, rep=2)  (n=6, rep=3)  (n=6, rep=10)  (n=60, rep=2)  (n=60, rep=3)  (n=60, rep=10)
##       <char> <num> <char> <int>         <num>         <num>          <num>          <num>          <num>           <num>
##  1:     CO:0   0.0     tt 10000        0.0462        0.0512         0.0474         0.0497         0.0491          0.0490
##  2:     CO:0   0.0 ttmean 10000        0.0513        0.0527         0.0485         0.0510         0.0509          0.0485
##  3:     CO:0   0.0   lmer 10000        0.1508        0.0862         0.1191         0.1462         0.0857          0.1133
##  4:   CO:0.1   0.1     tt 10000        0.0720        0.0942         0.2465         0.0715         0.0946          0.2403
##  5:   CO:0.1   0.1 ttmean 10000        0.0513        0.0527         0.0485         0.0510         0.0509          0.0485
##  6:   CO:0.1   0.1   lmer 10000        0.1437        0.0835         0.1155         0.1421         0.0843          0.1093
##  7:   CO:0.2   0.2     tt 10000        0.0720        0.0942         0.2465         0.0715         0.0946          0.2403
##  8:   CO:0.2   0.2 ttmean 10000        0.0513        0.0527         0.0485         0.0510         0.0509          0.0485
##  9:   CO:0.2   0.2   lmer 10000        0.1277        0.0781         0.1081         0.1284         0.0789          0.1020
## 10:   CO:0.3   0.3     tt 10000        0.0720        0.0942         0.2465         0.0715         0.0946          0.2403
## 11:   CO:0.3   0.3 ttmean 10000        0.0513        0.0527         0.0485         0.0510         0.0509          0.0485
## 12:   CO:0.3   0.3   lmer 10000        0.1103        0.0722         0.0987         0.1103         0.0739          0.0907
## 13:   CO:0.4   0.4     tt 10000        0.0720        0.0942         0.2465         0.0715         0.0946          0.2403
## 14:   CO:0.4   0.4 ttmean 10000        0.0513        0.0527         0.0485         0.0510         0.0509          0.0485
## 15:   CO:0.4   0.4   lmer 10000        0.0890        0.0650         0.0855         0.0898         0.0665          0.0764
## 16:       CS   0.5     tt 10000        0.0461        0.0520         0.0523         0.0495         0.0518          0.0494
## 17:       CS   0.5 ttmean 10000        0.0503        0.0502         0.0525         0.0508         0.0519          0.0499
## 18:       CS   0.5   lmer 10000        0.0482        0.0496         0.0534         0.0489         0.0507          0.0491
if(export){
    saveRDS(dt.sim2, file = "RIsim2-denseSampling.rds")
    saveRDS(dtS.sim2, file = "RIsim2-summary-denseSampling.rds")
    saveRDS(dtSW.sim2, file = "RIsim2-summaryWide-denseSampling.rds")
}

## ** figure 2
## dtS.sim2 <- readRDS(file = "x:/WorkTempo/denseSampling/RIsim2-summary-denseSampling.rds")
dtS.sim2[, n.obs_label := paste0("n=",n.obs," patients")]
dtS.sim2[, n.rep_label := factor(n.rep, levels=unique(n.rep))]
dtS.sim2[, model_label := factor(model, c("ttmean","tt","lmer"), labels = c("t-test (mean)","t-test (change)","random intercept model"))]

ggRejection2 <- ggplot(dtS.sim2[n.obs==6], aes(x = rhoB, y = rejection.rate, group = n.rep_label, colour = n.rep_label))
ggRejection2 <- ggRejection2 + geom_line(linewidth=1.15) + geom_point(size = 2.5)
ggRejection2 <- ggRejection2 + facet_grid(n.obs_label~model_label)
ggRejection2 <- ggRejection2 + labs(x=expression(rho[B]), y = "rejection rate", color = expression(p[1]))
ggRejection2 <- ggRejection2 + scale_color_manual(values = unname(palette.colors()[-1]))
ggRejection2 <- ggRejection2 + theme(text = element_text(size=15), 
                           axis.line = element_line(linewidth = 1.25),
                           axis.ticks = element_line(linewidth = 2),
                           axis.ticks.length=unit(.25, "cm"),
                           legend.key.size = unit(3,"line"))
ggRejection2

if(export){
    ggsave(ggRejection2, filename = "figures/gg-Rejection2-denseSampling.pdf", width = 10, height = 5)
}

## ** extra
Sigma.CO(n.rep = 2, rhoW = 0.8, rhoB = 0.5, rhoB.lag = 0.4)

ls.simE <- pblapply(1:1000, function(iSim){simRI(iSim, n.obs = 6, n.rep = 2, type.Sigma = c("CO"), rhoW = 0.5, rhoB = 0.1, rhoB.lag = 0)})
dt.simE <- as.data.table(do.call(rbind,ls.simE))
dtS.simE <- dt.simE[,.(n.sim = .N, bias = mean(estimate), rejection.rate = mean(p.value <= 0.05)), by= c("scenario","model","n.obs","n.rep")]
dtS.simE
##      <char> <char> <num> <num> <int>        <num>          <num>
## 1:       CO     tt     6     2  1000 -0.003647353          0.123
## 2:       CO ttmean     6     2  1000 -0.003647353          0.063
## 3:       CO   lmer     6     2  1000 -0.003647353          0.029

##----------------------------------------------------------------------
### simulation-RI.R ends here
