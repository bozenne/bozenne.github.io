### RM2019-solution-1.R ---
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 11 2019 (09:49) 
## Version: 
## Last-Updated: nov 19 2019 (17:00) 
##           By: Brice Ozenne
##     Update #: 30
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

path <- "c:/Users/hpl802/Documents/Teaching/repeated measurements 2019/1-Introduction/"
setwd(path)

## * Load packages
library(nlme) # to do mixed model analyses
library(lattice) # to make spaghettiplots and trend plots
library(ggplot2) # OPTIONAL: nice graphics
library(psych) # OPTIONAL: scatterplots with effects

## * Import data
list.files()
file.exists("gastricbypass.txt")
readLines("gastricbypass.txt")[1:3]

wide <- read.table("gastricbypass.txt", header=TRUE, na.strings=".")

## * Data management

## ** Check data
summary(wide)

## ** Reformat data
wide$id <- factor(wide$id)
summary(wide)

## Exercise 1, question 1: move to long format
long <- reshape(wide,
                direction='long',
                idvar='id',
                varying=list(
                    c('weight1','weight2','weight3','weight4'),
                    c('glucagonAUC1', 'glucagonAUC2', 'glucagonAUC3', 'glucagonAUC4')
                ),
                v.names=c('weight','glucagonAUC'),
                timevar='visit')
                                        # Make a categorical version of the time variable:
time.names <- c('-3 month','-1 week','+1 week','+3 month')
long$time <- factor(long$visit, labels=time.names)

summary(long)

wide1 <- cbind(wide[,c("id","weight1")],time = 1)
names(wide1) <- c("id","weight","time")

wide2 <- cbind(wide[,c("id","weight2")],time = 2)
names(wide2) <- c("id","weight","time")

rbind(wide1,wide2)

## * Question 2: Descriptive statistics/Data display

## ** display data
xyplot(weight~time, data=long, group=id, type='b')

# Simplify code by extracting outcomes from wide data:
weight.replicates <- wide[,c('weight1','weight2','weight3','weight4')]
# Scatterplot matrices with effects (psych-package):
pairs.panels(weight.replicates, smooth=FALSE, ellipses=TRUE)

par(mfrow = c(2,2))
hist(wide$weight1)
hist(wide$weight2)
hist(wide$weight3)
hist(wide$weight4, nbreak = 20)


ggplot(long, aes(x = weight)) + geom_histogram(bins = 8) + facet_wrap(~time)
ggplot(long, aes(x = time, y = weight)) + geom_boxplot()
ggplot(long, aes(x = time, y = weight)) + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center")
ggplot(long, aes(x = weight, fill = time)) + geom_dotplot()

## ** trends
aggregate(weight~time, data=long, mean)
## tapply(X= long$weight, INDEX = long$time, FUN = function(iVec){sum(!is.na(iVec))})

mymean <- function(x){
    mean(x)
}
mymean(1:3)

aggregate(weight~time, data=long, mymean)

a <- 5
b <- 5
a+b

mean(wide$weight1)
sufstat <- function(iWeight){
    c("observed" = sum(!is.na(iWeight)),
      "missing" = sum(is.na(iWeight)),
      "mean" = mean(iWeight, na.rm = TRUE),
      "sd" = sd(iWeight, na.rm = TRUE),
      "min" = min(iWeight, na.rm = TRUE),
      "median" = median(iWeight, na.rm = TRUE),
      "max" = max(iWeight, na.rm = TRUE)
      )
}


w.summaries2 <- aggregate(weight~time, data=long, sufstat, na.action = na.pass)
print(w.summaries2, digits = 4)
w.summaries <- data.frame(w.summaries2[1],w.summaries2[[2]])
print(w.summaries, digit = 4)



xyplot(mean~time, data=w.summaries, type='b')

cor(weight.replicates, use='pairwise.complete.obs')

## * Question 3: oneway ANOVA mixed model
# Set reference point (intercept) for time factor:
long$time <- relevel(long$time, ref="-3 month")

## Fit the linear mixed model:
fit.main <- gls(weight~time,
                data=long,
                correlation=corSymm(form=~visit|id),
                weights=varIdent(form=~1|time),
                na.action=na.exclude,
                control=glsControl(opt="optim"))
logLik(fit.main)
summary(fit.main)

cis.main <- intervals(fit.main, which="coef")
res.main <- cbind(cis.main$coef, summary(fit.main)$tTable[,"p-value"])
colnames(res.main) <- c("Lower95%CL","Estimate","Upper95%CL","P-value")
                                        # Rearrange to have 'Estimate' first:
print(res.main[,c(2,1,3,4)], digits=3)
## print(res.main[,c("Estimate","Lower95%CL","Upper95%CL")], digit = 4)

cor.main <- cov2cor(getVarCov(fit.main))
print(cor.main, digits=3)


fit.main$modelStruct$varStruct

cis.var <- intervals(fit.main, which="var")
sd.main <- cis.var$sigma["est."]*c(1,cis.var$varStruct[,"est."])
print(sd.main, digits=3)
setNames(sd.main, attr(fit.main$modelStruct$varStruct,"groupNames"))

## * Question 4: model validation

# Make a dataframe with predicted population means:
pred <- data.frame(visit=1:4, time=factor(1:4, labels=time.names))
pred <- cbind(pred, predicted.mean=predict(fit.main, newdata=pred))
# Plot with xyplot (lattice package) or just plot:
a <- xyplot(predicted.mean~time, data=pred, type='b', col = "red", lwd = 2)
b <- xyplot(weight~time, data=long, group = id, type='b', col = "grey")
b + latticeExtra::as.layer(a)

par(mfrow=c(1,2)) # splits graphics window in two
plot(fitted(fit.main), residuals(fit.main, type="normalized"), main="Normalized residuals")
abline(h=0)
qqnorm(residuals(fit.main, type="pearson"))
abline(0,1)

qqtest::qqtest(residuals(fit.main, type="pearson"))

plot(fit.main)

## * Question 5: change of reference point
## set reference point (intercept) for time factor:
long$time2 <- relevel(long$time, ref="-1 week")
## re-fit the model
fit.time2 <- gls(weight~time2,
                 data=long,
                 correlation=corSymm(form=~visit|id),
                 weights=varIdent(form=~1|time),
                 na.action=na.exclude,
                 control=glsControl(opt="optim"))
logLik(fit.time2)
summary(fit.time2)
cis.main2 <- intervals(fit.time2, which="coef")
res.main2 <- cbind(cis.main2$coef, summary(fit.time2)$tTable[,"p-value"])
colnames(res.main2) <- c("Lower95%CL","Estimate","Upper95%CL","P-value")
print(res.main2[,c(2,1,3,4)], digits=3)

## * Question 6: Paired t-test
t14 <- t.test(wide$weight4, wide$weight1, paired=TRUE)
res14 <- c(t14$estimate, t14$conf.int, t14$p.value)
names(res14) <- c("Estimate","Lower95%CL", "Upper95%CL","P-value")
print(res14, digits=4)

res.main[4,c(2,1,3)]/res14[1:3]


t23 <- t.test(wide$weight3, wide$weight2, paired=TRUE)
res23 <- c(t23$estimate, t23$conf.int, t23$p.value)
names(res23) <- c('Estimate','Lower95%CL', 'Upper95%CL','P-value')
print(res23, digits=4)

## ** df
t14
fit.main$dim

long$time3 <- as.factor(as.numeric(long$time))
fit.extra <- gls(weight~time3,
                 data=long,
                 correlation=corSymm(form=~visit|id),
                 weights=varIdent(form=~1|time),
                 na.action=na.exclude,
                 control=glsControl(opt="optim"))
library(lavaSearch2)
summary2(fit.extra)

## * Exercise 2: same with glucagonAUC

## ** Descriptive statistics/Data display

xyplot(glucagonAUC~time, data=long, group=id, type='b')

# Simplify code by extracting outcomes from wide data:
glucagonAUC.replicates <- wide[,c('glucagonAUC1','glucagonAUC2','glucagonAUC3','glucagonAUC4')]
# Scatterplot matrices with effects (psych-package):
pairs.panels(glucagonAUC.replicates, smooth=FALSE, ellipses=TRUE)

g.summaries2 <- aggregate(glucagonAUC~time, data=long, function(iWeight){
    c("observed" = sum(!is.na(iWeight)),
      "missing" = sum(is.na(iWeight)),
      "mean" = mean(iWeight, na.rm = TRUE),
      "sd" = sd(iWeight, na.rm = TRUE),
      "min" = min(iWeight, na.rm = TRUE),
      "median" = median(iWeight, na.rm = TRUE),
      "max" = max(iWeight, na.rm = TRUE)
      )
})
print(g.summaries2, digits = 4)
g.summaries <- data.frame(g.summaries2[1],g.summaries2[[2]])
           
xyplot(mean~time, data=g.summaries, type='b')
cor(glucagonAUC.replicates, use='pairwise.complete.obs')


## ** Linear mixed model

long$time <- relevel(long$time, ref="-3 month")

## Fit the linear mixed model:
MM.glucagon <- gls(glucagonAUC~time,
                   data=long,
                   correlation=corSymm(form=~visit|id),
                   weights=varIdent(form=~1|time),
                   na.action=na.exclude,
                   control=glsControl(opt="optim"))


## *** model validation
logLik(MM.glucagon)

# Make a dataframe with predicted population means:
pred <- data.frame(visit=1:4, time=factor(1:4, labels=time.names))
pred <- cbind(pred, predicted.mean=predict(fit.main, newdata=pred))

## Plot with xyplot (lattice package) or just plot:
xyplot(predicted.mean~time, data=pred, type='b')

corMM.glucagon <- cov2cor(getVarCov(MM.glucagon))
print(corMM.glucagon, digits=3)

par(mfrow=c(1,2)) # splits graphics window in two
plot(fitted(fit.main), residuals(fit.main, type="pearson"), main="Pearson residuals")
abline(h=0)
qqnorm(residuals(fit.main, type="pearson"))
abline(0,1)

## *** estimates
summary(MM.glucagon)
iMM.glucagon <- intervals(MM.glucagon, which="coef")
tableMM.glucagon <- cbind(iMM.glucagon$coef, summary(MM.glucagon)$tTable[,"p-value"])
colnames(tableMM.glucagon) <- c("Lower95%CL","Estimate","Upper95%CL","P-value")
tableMM.glucagon


## ** change reference
long$time2 <- relevel(long$time, ref="-1 week")
## re-fit the model
MM.glucagon2 <- gls(glucagonAUC~time2,
                    data=long,
                    correlation=corSymm(form=~visit|id),
                    weights=varIdent(form=~1|time),
                    na.action=na.exclude,
                    control=glsControl(opt="optim"))
logLik(MM.glucagon2)

## *** estimates
iMM.glucagon2 <- intervals(MM.glucagon2, which="coef")
tableMM.glucagon2 <- cbind(iMM.glucagon2$coef, summary(MM.glucagon2)$tTable[,"p-value"])
colnames(tableMM.glucagon2) <- c("Lower95%CL","Estimate","Upper95%CL","P-value")
print(tableMM.glucagon2[,c("Estimate","Lower95%CL","Upper95%CL")], digit = 4)

## ** paired t-test
ttest.glucagon14 <- t.test(wide$glucagonAUC4, wide$glucagonAUC1, paired=TRUE)
ttest.glucagon14

ttest.glucagon23 <- t.test(wide$glucagonAUC3, wide$glucagonAUC2, paired=TRUE)
ttest.glucagon23
######################################################################
### RM2019-solution-1.R ends here
