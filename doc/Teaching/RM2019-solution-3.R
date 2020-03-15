## chunk 2
## data management
library(reshape2)  # for converting data.frame from wide to long format

## graphical display
library(fields) # graphical display (image.plot)
library(gridExtra) # graphical display (grid.arrange)
library(lattice) # graphical display (xyplot)
library(latticeExtra) # graphical display (grid.arrange)

## statistical modeling/inference
library(nlme) # models for repeated measurements (gls, lme)
library(lavaSearch2) # inference in small samples with gls,lme

library(lme4) # another library for models for repeated measurements (lmer)
library(lmerTest) # inference in small samples with lmer

## diagnostics
library(qqtest) # qqplot
library(car) # leveneTest

## * Question 1: Import data

## chunk 3
dfL.swabs <- read.table("swabs.txt", header = TRUE)
str(dfL.swabs)

## chunk 4
dfL.swabs$family <- as.factor(dfL.swabs$family)

## chunk 5
dfL.swabs$crowding <- factor(dfL.swabs$crowding, 
                             levels = c("uncrow","crow","overcrow"))
dfL.swabs$name <- factor(dfL.swabs$name,
                         levels = c("mother","father",
                                    "child1","child2","child3"))

## chunk 6
dfL.swabs <- dfL.swabs[order(dfL.swabs$crowding,dfL.swabs$family,dfL.swabs$name),]
head(dfL.swabs)

## ** Inspection of the dataset

## chunk 7
summary(dfL.swabs, maxsum = 10)

## chunk 8
table(dfL.swabs$family, dfL.swabs$name)

## chunk 9
table(dfL.swabs$family, dfL.swabs$crowding)

## chunk 10
colSums(is.na(dfL.swabs))

## ** Descriptive graphs

## chunk 11
xy.spaguetti <- xyplot(swabs ~ name, group = family, type = "b", 
                       data = dfL.swabs)
xy.spaguetti

## chunk 13
interaction.plot(x.factor = dfL.swabs$name, 
                 trace.factor = dfL.swabs$crowding,
                 response = dfL.swabs$swabs, 
                 fun = "mean",
                 type = "b",
                 trace.label = "crowding level",
                 xlab = "family member", ylab = "swabs",
                 col = c("red","blue","green"))

## chunk 15
a <- xyplot(swabs ~ name, group = family, type = "b", 
            data = dfL.swabs,
            col = "gray")

b <- xyplot(swabs ~ name, group = crowding, type = "b", 
            data = aggregate(swabs ~ name + crowding, data = dfL.swabs, FUN = mean),
            ylim = range(dfL.swabs$swabs), lwd = 3, lty = 2,
            auto.key = list(corner = c(0.2,0.8)))
b + as.layer(a)

## * Question 2: Two-level model
## ** Fitting the model

## chunk 17
dfL.swabs$crowding <- relevel(dfL.swabs$crowding, ref = "uncrow")
dfL.swabs$name <- relevel(dfL.swabs$name, ref = "mother")

## chunk 18
e.lmeCS <- lme(swabs ~ crowding + name, 
              data = dfL.swabs,
              random =~ 1 | family)
logLik(e.lmeCS)

## chunk 19
e.glsCS <- gls(swabs ~ crowding + name, 
              data = dfL.swabs,
              correlation = corCompSymm(form=~ 1 | family))
logLik(e.glsCS)

## chunk 20
coef(e.glsCS) - fixef(e.lmeCS)

## chunk 21
Sigma.glsCS <- getVarCov(e.glsCS)
Sigma.glsCS

## chunk 22
Sigma.lmeCS <- getVarCov(e.lmeCS, type = "marginal")[[1]]
unclass(Sigma.glsCS) - unclass(Sigma.lmeCS)

## ** Inference

## chunk 23
intervals(lme.CS)$fixed[c("crowdingovercrow","namechild3"),]

## chunk 24
newdata <- unique(dfL.swabs[dfL.swabs$crowding %in% c("uncrow","overcrow"),c("name","crowding")])
newdata

## chunk 25
newdata$fit.glsCS <- predict(e.glsCS, newdata = newdata)

## chunk 26
data.frame(name = newdata$name[newdata$crowding=="overcrow"],
           diff = newdata$fit.glsCS[newdata$crowding=="overcrow"] - newdata$fit.glsCS[newdata$crowding=="uncrow"]
           )

## chunk 27
X <- model.matrix( ~ crowding + name, data = newdata)
X

## chunk 28
X[newdata$crowding=="overcrow",] - X[newdata$crowding=="uncrow",]

## ** Differential effect of crowding by family member

## chunk 29
e.glsCSinteract <- gls(swabs ~ crowding * name, 
                       data = dfL.swabs,
                       correlation = corCompSymm(form=~ 1 | family))
logLik(e.glsCSinteract)

## chunk 30
anova(e.glsCSinteract)

## chunk 31
anova(update(e.glsCSinteract, method = "ML"), 
      update(e.glsCS, method = "ML"))

## ** Graphical display of the fitted values

## chunk 32
dfL.swabs$fitted.CS <- fitted(e.glsCS)

## chunk 33
xy.fittedCS <- xyplot(fitted.CS ~ name, group = crowding, 
                      data = dfL.swabs[order(dfL.swabs$name),], 
                      type = "b", lwd = 3, auto.key = list(corner = c(0.2,0.8)))
xy.fittedCS

## chunk 35
b + as.layer(xy.fittedCS)

## * Question 3: Ignoring the correlation within families

## chunk 36
e.aovWrong <- aov(swabs ~ crowding + name , 
             data = dfL.swabs)
logLik(e.aovWrong)

## chunk 37
e.lmWrong <- lm(swabs ~ crowding + name , 
           data = dfL.swabs)
logLik(e.lmWrong)

## chunk 38
summary(e.lmWrong)$coef

## chunk 39
anova(e.lmWrong)

## chunk 40
keep.coef <- c("crowdingcrow","crowdingovercrow",
               "namefather",
               "namechild1","namechild2","namechild3")

M <- cbind(
    summary(e.glsCS)$tTable[keep.coef,c("Value","Std.Error")],
    summary(e.lmWrong)$coef[keep.coef,c("Estimate","Std. Error")]
)
colnames(M) <- c("estimate.gls","se.gls","estimate.lm","se.lm")
M

## * Question 4: Marginal analysis (over family)
## ** Compute the average swabs within families:

## chunk 41
df.average <- aggregate(swabs ~ crowding + family, 
                        FUN = "mean", data = dfL.swabs)
head(df.average)

## ** Test the effect of crowding using a 1 way anova

## chunk 42
e.lmAverage <- lm(swabs ~ crowding, data = df.average)
summary(e.lmAverage)$coef

## chunk 43
summary(e.glsCS)$tTable

## chunk 44
summary(e.lmeCS)$tTable

## chunk 45
anova(e.lmAverage)

## chunk 46
summary(aov(swabs ~ crowding, data = df.average))

## chunk 47
anova(e.lmeCS)

## chunk 48
cbind(estimate = coef(e.lmAverage), confint(e.lmAverage))

## ** Graphical display of the model

## chunk 49
df.average$fitted <- fitted(e.lmAverage)

## chunk 50
boxplot(swabs ~ crowding, data = df.average)
points(fitted ~ crowding, data = df.average, col = "red")

## * Question 5: Adjusting on families using fixed effects
## ** Fitting the model

## chunk 52
e.lmFamily <- lm(swabs ~ family + name, 
                data = dfL.swabs)
logLik(e.lmFamily)

## ** Inference

## chunk 53
anova(e.lmFamily)

## chunk 54
keep.coef <- c("namefather", "namechild1", 
               "namechild2", "namechild3")
cbind(
    estimate = coef(e.lmFamily),
    confint(e.lmFamily)
)[keep.coef,]

## chunk 55
anova(e.glsCS, type = "marginal")

## chunk 56
M <- cbind(
    summary(e.glsCS)$tTable[keep.coef,c("Value","Std.Error")],
    summary(e.lmFamily)$coef[keep.coef,c("Estimate","Std. Error")]
)
colnames(M) <- c("estimate.gls","se.gls","estimate.lm","se.lm")
M

## ** Graphical display of the model

## chunk 57
dfL.swabs$fittedFamily <- fitted(e.lmFamily)

## chunk 58
par(mfrow = c(1,2))
xy.fittedFamily <- xyplot(fittedFamily ~ name, group = family,
                          data = dfL.swabs[order(dfL.swabs$name),],
                          type = "l", ylab = "fitted swabs")
grid.arrange(xy.spaguetti, xy.fittedFamily, ncol = 2)

## chunk 59
pdf("figures/fittedFamily.pdf", width = 12)
grid.arrange(xy.spaguetti, xy.fittedFamily, ncol = 2)
graphics.off()

## * Question 6: A more flexible two-level model
## ** Marginal correlation

## chunk 60
dfW.swabs <- dcast(dfL.swabs, value.var = "swabs",
                   formula = family+crowding ~ name)
head(dfW.swabs)

## chunk 61
rho <- cor(dfW.swabs[,c("mother","father","child1","child2","child3")])
rho

## chunk 62
rho.plot <- rho
rho.plot[lower.tri(rho)] <- NA
rho.plot

## chunk 63
Uname <- colnames(rho.plot)
image.plot(x = 1:length(Uname), y = 1:length(Uname), z = rho.plot, 
           zlim = c(-1,1), xlab = "", ylab = "", axes = FALSE)
axis(1, at = 1:length(Uname), labels = Uname)
axis(2, at = 1:length(Uname), labels = Uname)

## ** [Extra] Diagnostic plots for the CS model

## chunk 65
dfL.swabs$residuals <- residuals(e.glsCS, type = "normalized")

## chunk 66
boxplot(residuals ~ name, data = dfL.swabs)

## chunk 68
leveneTest(residuals ~ name, data = dfL.swabs)

## chunk 69
dfW.residuals <- dcast(dfL.swabs, 
                       value.var = "residuals",
                       formula = family~name)
head(dfW.residuals)

## chunk 70
cor(dfW.residuals[,c("mother","father","child1","child2","child3")])

## chunk 71
qqtest(dfL.swabs$residuals)

## chunk 73
shapiro.test(dfL.swabs$residuals)

## ** Relaxing assumptions using an unstructured covariance matrix

## chunk 74
e.glsUN <- gls(swabs ~ crowding + name, 
          data = dfL.swabs,
          correlation = corSymm(form = ~ as.numeric(name) | family),
          weights = varIdent(form = ~ 1 | name)
               )
logLik(e.glsUN)

## chunk 75
Sigma <- getVarCov(e.glsUN)
rownames(Sigma) <-  attr(e.glsUN$modelStruct$varStruct, "groupNames")
colnames(Sigma) <-  attr(e.glsUN$modelStruct$varStruct, "groupNames")

## chunk 76
sqrt(diag(Sigma))

## chunk 77
Corr <- cov2cor(Sigma)
Corr

## chunk 78
anova(e.glsCS, e.glsUN)

## chunk 79
cbind(CS.estimate = summary(e.glsCS)$tTable[,"Value"], 
      CS.se = summary(e.glsCS)$tTable[,"Std.Error"], 
      UN.estimate = summary(e.glsUN)$tTable[,"Value"],
      UN.se = summary(e.glsUN)$tTable[,"Std.Error"])

## * Using =data.table= 

## chunk 80
library(data.table)

## chunk 81
dtL.swabs <- as.data.table(dfL.swabs)

## ** Question 4

## chunk 82
dt.average <- dtL.swabs[,.(swabs = mean(swabs)),
                        by = c("family","crowding")]
head(dt.average)

## * Using =ggplot2= 

## chunk 83
library(ggplot2)

## ** Question 1

## chunk 84
gg.spaguetti <- ggplot(dfL.swabs, aes(x = name, y = swabs, 
                                      group = family, color = family))
gg.spaguetti <- gg.spaguetti + geom_line() + geom_point()
gg.spaguetti <- gg.spaguetti + facet_grid(~crowding, 
                                          labeller = label_both)
gg.spaguetti <- gg.spaguetti + xlab("")
gg.spaguetti

## chunk 86
gg.mean <- ggplot(dfL.swabs, aes(x = name, y = swabs, 
                                 group = crowding, color = crowding))
gg.mean <- gg.mean + stat_summary(geom = "line", fun.y = mean, 
                                  size = 3, fun.data = NULL)
gg.mean

## ** Question 3

## chunk 88
gg.fittedCS <- ggplot(dfL.swabs, aes(x = name, y = fitted.CS, 
                          group = crowding, color = crowding))
gg.fittedCS <- gg.fittedCS + geom_point() + geom_line()
gg.fittedCS <- gg.fittedCS + xlab("") + ylab("fitted value")
gg.fittedCS

## ** Question 4

## chunk 90
gg.fittedA <- ggplot(df.average, aes(x = crowding))
gg.fittedA <- gg.fittedA + geom_boxplot(aes(y = swabs, color = "data", shape = "data"))
gg.fittedA <- gg.fittedA + geom_point(aes(y = fitted, color = "fitted", shape = "fitted"), 
                                      size = 5)
gg.fittedA

## ** Question 6

## chunk 92
dfL.cor <- melt(rho.plot,
                value.name = "correlation")
head(dfL.cor)

## chunk 93
gg.cor <- ggplot(dfL.cor, aes(x = Var1, y = Var2, fill = correlation))
gg.cor <- gg.cor + geom_tile()
gg.cor <- gg.cor + scale_fill_gradient2(limits = c(-1,1), 
                                        low = "blue", high = "red")
gg.cor <- gg.cor + xlab("") + ylab("")
gg.cor

## * Marginal and conditional predictions with the =lme= function

## chunk 95
newdata$fit.lmeCS <- predict(e.lmeCS, newdata = newdata, level = 0)

## chunk 96
range(newdata$fit.lmeCS - newdata$fit.glsCS)

## chunk 97
try( predict(e.lmeCS, newdata = newdata, level = 1) )

## chunk 98
pf1 <- predict(e.lmeCS, newdata = cbind(newdata, family = 1), level = 1)
pf1

## chunk 99
pf2 <- predict(e.lmeCS, newdata = cbind(newdata, family = 2), level = 1)
pf2

## chunk 100
pf2-pf1

## chunk 101
ranef(e.lmeCS)[2,1] - ranef(e.lmeCS)[1,1]

