### RM2019-solution-5.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  3 2019 (11:14) 
## Version: 
## Last-Updated: dec  3 2019 (13:20) 
##           By: Brice Ozenne
##     Update #: 6
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

## constant group difference on the logit scale
simplified.gee <- geeglm(response ~ visit + group, 
                         id = id,
                         data = dfL.ony, 
                         family = binomial(link = "logit"),
                         corstr = "unstructured")
summary(simplified.gee)

## unrestricted group effect over visit 
simplified.gee2 <- geeglm(response ~ visit*group, 
                          id = id,
                          data = dfL.ony, 
                          family = binomial(link = "logit"),
                          corstr = "unstructured")
summary(simplified.gee2)

## unrestricted treatment effect over visit
## but the groups are constrained to have the same prevalence at baseline
try(capture.output(geeglm(response ~ treatment*visit, 
                          id = id,
                          data = dfL.ony, 
                          family = binomial(link = "logit"),
                          corstr = "unstructured")))

## chunk 24
X <- model.matrix(response ~ visit*treatment, data = dfL.ony)
colnames(X)

## chunk 25
dfL.ony$treatment.visit <- interaction(dfL.ony$treatment,dfL.ony$visit)
table(dfL.ony$treatment.visit)

## chunk 26
dfL.ony$treatment.visit <- droplevels(dfL.ony$treatment.visit)
table(dfL.ony$treatment.visit)

summary(geeglm(response ~ treatment.visit, 
               id = id,
               data = dfL.ony, 
               family = binomial(link = "logit"),
               corstr = "unstructured"))

## ** Definition of the interaction

## chunk 27
dfL.ony$treatmentIvisit <- as.character(dfL.ony$visit)

## chunk 28
dfL.ony[dfL.ony$visit == "1", "treatmentIvisit"] <- "none"

## chunk 29
dfL.ony[dfL.ony$treatment == "200mg", "treatmentIvisit"] <- "none"

## chunk 30
dfL.ony$treatmentIvisit <- as.factor(dfL.ony$treatmentIvisit)

## chunk 31
table(dfL.ony$treatmentIvisit, dfL.ony$visit, dfL.ony$group)

## ** Model fitting

## chunk 32
dfL.ony <- dfL.ony[order(dfL.ony$id,dfL.ony$visit),]
dfL.ony[1:10,c("id","treatment","visit","time","response")]

## chunk 33
dfL.ony$treatment <- relevel(dfL.ony$treatment, "none")
dfL.ony$visit <- relevel(dfL.ony$visit, "1")
dfL.ony$treatmentIvisit <- relevel(dfL.ony$treatmentIvisit, "none")

## chunk 34
e.geeUN <- geeglm(response ~ visit + treatmentIvisit, 
                  id = id,
                  data = dfL.ony, 
                  family = binomial(link = "logit"),
                  corstr = "unstructured")
logLik(e.geeUN)

## chunk 35
summary(e.geeUN)$coefficients

## chunk 36
corr.coef <- summary(e.geeUN)$corr[,"Estimate"]

n.corr.coef <- length(corr.coef)
Mcorr <- matrix(NA, 7, 7)
Mcorr[lower.tri(Mcorr)] <- corr.coef

Mcorr

## chunk 37
name.corr.coef <- rownames(summary(e.geeUN)$corr)
ls.position.corr.coef <- strsplit(gsub("alpha.","",name.corr.coef),":")
position.corr.coef <- do.call(rbind,lapply(ls.position.corr.coef, 
                                           as.numeric))
rownames(position.corr.coef) <- name.corr.coef
colnames(position.corr.coef) <- c("row","column")
head(position.corr.coef)

## ** Inference

## chunk 38
ORvisit7 <- exp(coef(e.geeUN)["visit7"])
ORvisit7

## chunk 39
try(confint(e.geeUN))

## chunk 40
exp(broom:::confint.geeglm(e.geeUN)["visit7",])

## chunk 41
beta.visit7 <- summary(e.geeUN)$coef["visit7","Estimate"]
betaSE.visit7 <- summary(e.geeUN)$coef["visit7","Std.err"]

quantile.norm <- qnorm(c(lower = 0.025, estimate = 0.5, upper = 0.975))

CI.visit7 <- exp(beta.visit7 + quantile.norm * betaSE.visit7)
CI.visit7

## chunk 42
e.geeUN0 <- geeglm(response ~ visit, 
                   id = id,
                   data = dfL.ony, 
                   family = binomial(link = "logit"),
                   corstr = "unstructured")

## chunk 43
anova(e.geeUN0, e.geeUN)

## ** Display of the fitted prevalence

## chunk 44
fitted.values <- fitted(e.geeUN)
length(fitted.values)

## chunk 45
sum(!is.na(dfL.ony$response))

## chunk 46
dfL.ony$fittedGEE <- NA
dfL.ony[!is.na(dfL.ony$response), "fittedGEE"] <- fitted.values

## chunk 47
df.tempo <- na.omit(dfL.ony[order(dfL.ony$expected.timeweek),])

## chunk 48
plot.fittedGEE <- xyplot(fittedGEE ~ expected.timeweek, group = group, 
                         type = "l", ylab = "fitted prevalence (GEE)",
                         xlab = "expected visit time (weeks)",
                         data = df.tempo,
                         auto.key = TRUE
                         )

## chunk 49
df.prev$expected.timeweek <- factor(df.prev$visit, levels = 1:7, labels = c(0,4,8,12,24,36,48))
df.prev$expected.timeweek <- as.numeric(as.character(df.prev$expected.timeweek))

grayprevplot <- xyplot(response ~ expected.timeweek, 
                       group = group, data = df.prev,  
                       type = "b",
                       ylab = "prevalence", col = "gray")
plot.fittedGEE + as.layer(grayprevplot)

## chunk 51
ilogit <- function(x){log(x/(1-x))}
plot.fittedGEE_logit <- xyplot(ilogit(fittedGEE) ~ expected.timeweek, 
                               group = group, 
                               type = "l", 
                               ylab = "log odds  (GEE)",
                               xlab = "expected visit time (weeks)",
                               data = df.tempo,
                               auto.key = TRUE
                               )

grayprevplot_logit <- xyplot(ilogit(response) ~ expected.timeweek, 
                             group = group, data = df.prev,  
                             type = "b",
                             ylab = "prevalence", col = "gray")
plot.fittedGEE_logit + as.layer(grayprevplot_logit)



######################################################################
### RM2019-solution-5.R ends here
