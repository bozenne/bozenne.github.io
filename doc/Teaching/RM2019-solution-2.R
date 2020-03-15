### RM2019-solution-2.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 19 2019 (15:46) 
## Version: 
## Last-Updated: nov 22 2019 (16:56) 
##           By: Brice Ozenne
##     Update #: 32
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Load packages
library(lattice) ## graphical display of repeated measurements
library(latticeExtra) ## combine 2 lattice plots
library(psych) ## scatterplot with histograms and correlations
library(nlme) ## mixed models
library(qqtest) ## more informative qqplots
library(data.table) ## convenient for data management
library(multcomp) ## adjustment for multiple comparisons 

## * Set working directories
path <- "c:/Users/hpl802/Documents/Teaching/repeated measurements 2019/2-Longitudinal/"
setwd(path)

## * Exercise 1
## ** Question 1: Import the dataset and describe it 
list.files()
file.exists("ncgs.txt")
readLines("ncgs.txt")[1:3]

wide <- read.table("ncgs.txt", header=TRUE, na.strings=".")

str(wide)

wide$group <- factor(wide$group, levels = 1:2, labels = c("highdose","placebo"))
wide$id <- factor(wide$id)

str(wide)

table(wide$id)
table(wide$group)

is.na(NA)
is.na(1)
sum(is.na(wide))

summary(wide)

## ** Question 2: Transform to long format
## *** reshape
long <- reshape(wide,
                direction='long',
                idvar=c('group','id'),
                varying=c('cholest1','cholest2','cholest3','cholest4','cholest5'),
                v.names=c('cholest'),
                timevar='visit')
rownames(long) <- NULL

long2 <- reshape(wide,
                 direction='long',
                 idvar=c('group','id'),
                 varying=paste0("cholest",1:5),
                 v.names=c('cholest'),
                 timevar='visit')
rownames(long2) <- NULL
identical(long2,long)         

long$time <- factor(long$visit, levels = 1:5, labels = c(0,6,12,20,24))
summary(long)

## *** visualize
xyplot(cholest ~ time, group = id, type = "b", data = long)
a <- xyplot(cholest ~ time|group, group = id, type = "b", data = long, col = "grey")

## ** Question 3: summary statistics
## *** values

## wide format
c("1" = mean(wide$cholest1),
  "2" = mean(wide$cholest2),
  "3" = mean(wide$cholest3),
  "4" = mean(wide$cholest4),
  "5" = mean(wide$cholest5))

c("1" = mean(wide$cholest1, na.rm = TRUE),
  "2" = mean(wide$cholest2, na.rm = TRUE),
  "3" = mean(wide$cholest3, na.rm = TRUE),
  "4" = mean(wide$cholest4, na.rm = TRUE),
  "5" = mean(wide$cholest5, na.rm = TRUE))

## long format
aggregate(cholest ~ time, long, mean)
aggregate(cholest ~ time, long, mean, na.action = na.pass)

mymean <- function(x){
    mean(x, na.rm = TRUE)
}

aggregate(cholest ~ time, long, mymean, na.action = na.pass)

mysum <- function(x){
    c("observed" = sum(!is.na(x)),
      "missing" = sum(is.na(x)),
      "mean" = mean(x, na.rm = TRUE),
      "sd" = sd(x, na.rm = TRUE),
      "min" = min(x, na.rm = TRUE),
      "median" = median(x, na.rm = TRUE),
      "max" = max(x, na.rm = TRUE)
      )
}
aggregate(cholest ~ time, long, mysum)
aggregate(cholest ~ group + time, long, mysum)

## using data.table
dt.long <- as.data.table(long)
dt.long[, mean(cholest,na.rm=TRUE), by = "group"]
dt.long[, list("mean" = mean(cholest,na.rm=TRUE),
               "sd" = sd(cholest,na.rm=TRUE),
               "missing" = sum(is.na(cholest)),
               "min" = min(cholest,na.rm=TRUE),
               "max" = max(cholest,na.rm=TRUE)
               ),
        by = "group"]

dt.long[, list("mean" = mean(cholest,na.rm=TRUE),
               "sd" = sd(cholest,na.rm=TRUE),
               "missing" = sum(is.na(cholest)),
               "min" = min(cholest,na.rm=TRUE),
               "max" = max(cholest,na.rm=TRUE)
               ),
        by = c("group","time")]

## *** display
long.mean <- aggregate(cholest ~ time+group, data = long, mean)
long.sd <- aggregate(cholest ~ time+group, data = long, sd)

xyplot(cholest ~ time, type = "b", group = group, data = long.mean)
xyplot(cholest ~ time, type = "b", group = group, data = long.sd)

b <- xyplot(cholest ~ time|group, type = "b", data = long.mean, col = "red", lwd = 2, size = 2)
a + as.layer(b)

highdose.reps <- subset(wide, group == "highdose")[,paste0("cholest",1:5)]
pairs.panels(highdose.reps)

lowdose.reps <- subset(wide, group == "placebo")[,paste0("cholest",1:5)]
pairs.panels(lowdose.reps)

## ** Question 4: time-varing covariate for the treatment effect

long$highdose.time <- long$time
long$highdose.time[long$group=="placebo"] <- "0"
subset(long, id == 1)
subset(long, id == 63)

## ** Question 5: Constrained linear mixed model
## *** fit model
long$time <- relevel(long$time,"0")
long$highdose.time <- relevel(long$highdose.time,"0")
model.matrix(cholest ~ time, data = long)
fit.clmm <- gls(cholest ~ time + highdose.time,
                data = long,
                correlation = corSymm(form =~ visit|id),
                weight = varIdent(form =~ 1|time),
                na.action = na.exclude,
                control = glsControl(opt = 'optim'))
logLik(fit.clmm)
summary(fit.clmm)
summary(fit.clmm)$tTable

## ## wrong!!!!
## fit.clmm0 <- gls(cholest ~ time + highdose.time,
##                 data = long,
##                 correlation = corSymm(form =~ 1|id),
##                 weight = varIdent(form =~ 1|time),
##                 na.action = na.exclude,
##                 control = glsControl(opt = 'optim'))
## logLik(fit.clmm0)

## ok
long.NNA <- na.omit(long) ## remove lines with NA
fit.clmm2 <- gls(cholest ~ time + highdose.time,
                data = long.NNA,
                correlation = corSymm(form =~ visit|id),
                weight = varIdent(form =~ 1|time),
                control = glsControl(opt = 'optim'))
logLik(fit.clmm2)
anova(fit.clmm)
## *** diagnostics
res.clmm <- residuals(fit.clmm, type = "response")
resNorm.clmm <- residuals(fit.clmm, type = "pearson") ## help(residuals.gls)
qqtest(na.omit(res.clmm))
qqtest(na.omit(resNorm.clmm))

boxplot(res.clmm ~ long$time)
boxplot(resNorm.clmm ~ long$time)

boxplot(resNorm.clmm ~ long$group)

plot(resNorm.clmm ~ fitted(fit.clmm))

## *** expected values
df.new <- expand.grid(group = factor(1:2, labels = c("highdose","placebo")),
                       time = factor(1:5, labels = c(0,6,12,20,24)))
df.new$highdose.time <- df.new$time
df.new$highdose.time[df.new$group=="placebo"] <- "0"

df.new2 <- unique(long[,c("group","time","highdose.time")])
df.new2$fit <- predict(fit.clmm, newdata = df.new2)


c <- xyplot(fit ~ time|group, data = df.new2, type = "b")
a + as.layer(c)

## *** statistical inference
summary(fit.clmm)
intervals(fit.clmm)
anova(fit.clmm)

e.glht <- glht(fit.clmm,
               linfct = c("highdose.time6 = 0","highdose.time12 = 0","highdose.time20 = 0","highdose.time24 = 0"))
summary(e.glht, test = adjusted("none"))
summary(e.glht, test = adjusted("bonferroni"))
summary(e.glht, test = adjusted("single-step"))

## *** change of reference level
long$group <- relevel(long$group,"placebo")
long$time <- relevel(long$time,"6")

fit.clmm2 <- update(fit.clmm, data = long)
logLik(fit.clmm)
logLik(fit.clmm2)
summary(fit.clmm2)$tTable

## yet another reference level
long$group <- relevel(long$group,"highdose")
long$time <- relevel(long$time,"0")

fit.clmm3 <- update(fit.clmm, data = long)
logLik(fit.clmm)
logLik(fit.clmm3)
summary(fit.clmm3)$tTable ## does not work

## solution 1
e.glht <- glht(fit.clmm,
               linfct = c("highdose.time6 + time6 = 0",
                          "highdose.time12 + time12 = 0",
                          "highdose.time20 + time20 = 0",
                          "highdose.time24 + time24 = 0"))
summary(e.glht, test = adjusted("none"))

## solution 2
long$placebo.time <- long$time
long$placebo.time[long$group=="highdose"] <- "0"
subset(long, id == 1)
subset(long, id == 63)

fit.clmm4 <- gls(cholest ~ time + placebo.time,
                 data = long,
                 correlation = corSymm(form =~ visit|id),
                 weight = varIdent(form =~ 1|time),
                 na.action = na.exclude,
                 control = glsControl(opt = 'optim'))
logLik(fit.clmm)
logLik(fit.clmm4)
summary(fit.clmm4)



## * Exercise 2
## ** Question 1: Import the dataset and reshape it
## *** Import
wide <- read.table("vitamin2019.txt", header = TRUE)
str(wide)

## *** Data management
wide$group <- factor(wide$group, 
                     levels = 1:2, labels = c("C","T"))
wide$animal <- as.factor(wide$animal)

str(wide)
summary(wide, maxsum = 10)
colSums(is.na(wide))

long <- reshape(wide,
                direction = "long",
                idvar = c("animal","group"),
                varying = paste0("weight",c(1,3:7)),
                v.name = "weight",
                timevar = "visit")

long$time <- factor(long$visit, levels = 1:6, labels = c(1,3:7))
table(long$group,long$animal)
table(long$time,long$animal)

## ** Question 2: Descriptive statistics

xyplot(weight ~ time | group, group = animal, data = long, type = "b")
a <- xyplot(weight ~ time | group, group = animal, data = long, type = "b", col = "gray")


dt.long <- as.data.table(long)
dt.sufstat <- dt.long[,.(observed = sum(!is.na(weight)),
                         missing = sum(is.na(weight)),
                         mean = mean(weight),
                         sd = sd(weight)),
                      by = c("group","time")]

b <- xyplot(mean ~ time, data = dt.sufstat, group = group, type = "b")
a + as.layer(b)

## ** Question 3: baseline adjustment

long$vita.time <- long$time
long$vita.time[long$group=="C" | long$visit<4] <- "1"
subset(long, animal == 1)
subset(long, animal == 6)

## ** Question 4: linear mixed model

## *** fit model
long$time <- relevel(long$time, ref = "1")
long$vita.time <- relevel(long$vita.time, ref = "1")

fit.clmm <- gls(weight ~ time + vita.time,
                data = long,
                correlation = corSymm(form =~ as.numeric(time)|animal),
                weight = varIdent(form =~ 1|time),
                na.action = na.exclude,
                control = glsControl(opt = 'optim'))

## *** diagnostic
## not done
plot(fit.clmm)
qqtest(residuals(fit.clmm, type = "pearson"))
qqtest(residuals(fit.clmm, type = "normalized"))

Omega <- getVarCov(fit.clmm)
OmegaHalf <- eigen(Omega)$vectors %*% diag(sqrt(eigen(Omega)$values)) %*% t(eigen(Omega)$vectors)
OmegaM1Half <- solve(OmegaHalf)

long$res <- residuals(fit.clmm, type = "response")
wide2 <- dcast(long, value.var = "res",
               animal + group ~ time)

M.rawRes <- as.matrix(wide2[,as.character(c(1,3:7))])
apply(M.rawRes,2,mean)

resNorm <- M.rawRes %*% OmegaM1Half
qqtest(as.double(resNorm))

## *** expected values
df.new <- unique(long[,c("group","time","vita.time")])
df.new$fit <- predict(fit.clmm, newdata = df.new)

xyplot(fit ~ time, group = group, data = df.new, type = "b")

c <- xyplot(fit ~ time| group, data = df.new, type = "b")
a + as.layer(c)

## *** inference
summary(fit.clmm)

## ** Question 5: small sample inference
library(lavaSearch2)
sCorrect(fit.clmm) <- FALSE 
summary2(fit.clmm)
summary(fit.clmm)
compare2(fit.clmm, par = c("vita.time5","vita.time6","vita.time7"))

######################################################################
### RM2019-solution-2.R ends here
