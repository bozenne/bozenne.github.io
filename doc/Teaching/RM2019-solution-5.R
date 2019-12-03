### RM2019-solution-5.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  3 2019 (11:14) 
## Version: 
## Last-Updated: dec  3 2019 (12:31) 
##           By: Brice Ozenne
##     Update #: 4
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## chunk 2
library(lme4) # subject specific model
library(geepack) # population average models
library(doBy) # esticon function
library(broom) # confint for GEE

library(lattice) # graphical display (xyplot)
library(latticeExtra) # graphical display (as.layer)

## * Question 1: Import data
## ** Data management

## chunk 3
dfW.ony <- read.table("onycholysis.txt", 
                      header = TRUE, na.strings = ".")
str(dfW.ony)

## chunk 4
dfL.ony <- reshape(dfW.ony, 
                   direction="long", 
                   idvar=c("id","treatment"), 
                   varying=list(paste0("response",1:7), 
                                paste0("time",1:7)),
                   v.names=c("response","time"),
                   timevar="visit")
str(dfL.ony)

## chunk 5
dfL.ony$id <- as.factor(dfL.ony$id)
dfL.ony$visit <- as.factor(dfL.ony$visit)
dfL.ony$treatment <- factor(dfL.ony$treatment, levels = 0:1, 
                            labels = c("200mg","250mg"))

## chunk 6
dfL.ony$expected.timeweek <- as.numeric(NA)
dfL.ony[dfL.ony$visit=="1", "expected.timeweek"] <- 0
dfL.ony[dfL.ony$visit=="2", "expected.timeweek"] <- 4
dfL.ony[dfL.ony$visit=="3", "expected.timeweek"] <- 8
dfL.ony[dfL.ony$visit=="4", "expected.timeweek"] <- 12
dfL.ony[dfL.ony$visit=="5", "expected.timeweek"] <- 24
dfL.ony[dfL.ony$visit=="6", "expected.timeweek"] <- 36
dfL.ony[dfL.ony$visit=="7", "expected.timeweek"] <- 48

table(dfL.ony$visit,dfL.ony$expected.timeweek)

## chunk 7
dfL.ony$visit.num <- as.numeric(dfL.ony$visit)

## ** Inspection of the dataset

## chunk 8
summary(dfL.ony)

## chunk 9
aggregate(treatment ~ visit, FUN = "table", data = dfL.ony)

## chunk 10
dfL.ony$group <- factor(dfL.ony$treatment, 
                        levels = c("200mg","250mg"), 
                        labels = c("control","experimental"))

## chunk 11
dfL.ony$treatment <- as.character(dfL.ony$treatment)
dfL.ony[dfL.ony$visit=="1","treatment"] <- "none"
dfL.ony$treatment <- as.factor(dfL.ony$treatment)

## chunk 12
table(dfL.ony$visit, dfL.ony$treatment)

## chunk 13
table(dfL.ony$visit, dfL.ony$group)

## chunk 14
colSums(is.na(dfL.ony))

## ** Computation of the prevalence

## chunk 15
df.prev <- aggregate(response ~ group + visit, FUN = mean, 
                     data = dfL.ony)
df.prev

## chunk 16
mySummary <- function(x){
    return(c(n.obs = sum(!is.na(x)), 
             n.response = sum(x, na.rm = TRUE),
             prevalence = mean(x, na.rm = TRUE)))
}
aggregate(response ~ group + visit, FUN = mySummary, 
          data = dfL.ony, na.action = na.pass)

## chunk 17
prevplot <- xyplot(response ~ visit, group = group, data = df.prev,  
                   type = "b", auto.key = list(corner = c(0.8,0.8)),
                   ylab = "prevalence")
prevplot

## * Question 2: Scheduled vs. observed visit times

## chunk 19
dfL.ony$timeweek <- dfL.ony$time * 52 / 12

## chunk 20
aggregate(timeweek ~ visit + group + expected.timeweek, FUN = quantile, 
          data = dfL.ony, probs = c(0,0.5,1))

## chunk 21
boxplot(timeweek ~ visit, data = dfL.ony)
points(expected.timeweek ~ visit, data = dfL.ony, 
       col = "red", pch = 21, cex = 2)

## * Question 3: Discrete time population average model
dfL.ony <- dfL.ony[order(dfL.ony$id,dfL.ony$visit),]
dfL.ony[1:10,c("id","treatment","visit","time","response")]

head(dfL.ony)

## chunk 23
simplified.gee <- geeglm(response ~ visit + group, 
                         id = id,
                         data = dfL.ony, 
                         family = binomial(link = "logit"),
                         corstr = "unstructured")
summary(simplified.gee)


try(capture.output(geeglm(response ~ treatment*visit, 
                          id = id,
                          data = dfL.ony, 
                          family = binomial(link = "logit"),
                          corstr = "unstructured")))


######################################################################
### RM2019-solution-5.R ends here
