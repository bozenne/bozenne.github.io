## chunk 2
library(reshape2)  # for converting data.frame from wide to long format
library(nlme) # models for repeated measurements (gls, lme)
library(fields) # graphical display (image.plot)
library(gridExtra) # graphical display (grid.arrange)
library(latticeExtra) # graphical display (grid.arrange)
library(lattice) # graphical display (xyplot)
library(qqtest) # qqplot
library(car) # leveneTest
library(lme4) # mixed models
library(lmerTest) # mixed models

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

## ** Inspection of the dataset

## chunk 6
summary(dfL.swabs, maxsum = 10)

## chunk 7
table(dfL.swabs$family, dfL.swabs$name)

## chunk 8
table(dfL.swabs$family, dfL.swabs$crowding)

## chunk 9
colSums(is.na(dfL.swabs))

## ** Descriptive graphs

## chunk 10
xyplot(swabs ~ name, group = family, type = "b", 
       data = dfL.swabs[order(dfL.swabs$name),])
a <- xyplot(swabs ~ name, group = family, type = "b", 
       data = dfL.swabs[order(dfL.swabs$name),], col = "gray")

dfL.mean <- aggregate(swabs ~ name + crowding, data = dfL.swabs, FUN = mean)
b <- xyplot(swabs ~ name, group = crowding, type = "b",
            data = dfL.mean, lwd = 2, auto.key = TRUE, ylim = range(dfL.swabs$swabs))
b + as.layer(a)
## chunk 12
interaction.plot(x.factor = dfL.swabs$name, 
                 trace.factor = dfL.swabs$crowding,
                 response = dfL.swabs$swabs, 
                 fun = "mean",
                 type = "b",
                 trace.label = "crowding level",
                 xlab = "family member", ylab = "swabs",
                 col = c("red","blue","green"))

## * Question 2: Two-level model
## ** Fitting the model

## chunk 14
dfL.swabs$crowding <- relevel(dfL.swabs$crowding, ref = "uncrow")
dfL.swabs$name <- relevel(dfL.swabs$name, ref = "mother")

## chunk 16
e.glsCS <- gls(swabs ~ crowding + name, 
              data = dfL.swabs,
              correlation = corCompSymm(form=~ 1 | family))
logLik(e.glsCS)

dfL.swabs[1,]
X <- model.matrix( ~ crowding + name, data = dfL.swabs[1,])
X
fitted(e.glsCS)[1]
sum(coef(e.glsCS) * X[1,])


## chunk 15
lme.CS <- lme(swabs ~ crowding + name, 
              data = dfL.swabs,
              random =~ 1 | family)
logLik(lme.CS)

dfL.swabs[1,]
X <- model.matrix( ~ crowding + name, data = dfL.swabs[1,])
X

fitted(lme.CS)[1]
sum(fixef(lme.CS) * X[1,])
sum(fixef(lme.CS) * X[1,]) + ranef(lme.CS)[1,1]

predict(lme.CS, level = 0)[1]
predict(lme.CS, level = 1)[1]


e.lmer <- lmer(swabs ~ crowding + name + (1|family),
               data = dfL.swabs)
logLik(e.lmer)

## chunk 17
coef(e.glsCS) - fixef(lme.CS)

## chunk 18
getVarCov(e.glsCS) - getVarCov(lme.CS, type = "marginal")[[1]]

## chunk 19
Sigma.CS <- getVarCov(e.glsCS, individuals = 1)
Sigma.CS

## ** Inference

## chunk 20
intervals(lme.CS)$fixed[c("crowdingovercrow","namechild3"),]

## ** Differential effect of crowding by family member

## chunk 21
e.glsCSinteract <- gls(swabs ~ crowding * name, 
                      data = dfL.swabs,
                      correlation = corCompSymm(form=~ 1 | family))
logLik(e.glsCSinteract)
summary(e.glsCSinteract)$tTable

## chunk 22
anova(e.glsCSinteract)

## chunk 23
anova(update(e.glsCSinteract, method = "ML"), 
      update(e.glsCS, method = "ML"))

## ** Graphical display of the fitted values

## chunk 24
dfL.swabs$fitted.CS <- fitted(e.glsCS)

## chunk 25
xy.fittedCS <- xyplot(fitted.CS ~ name, group = crowding, 
                      data = dfL.swabs[order(dfL.swabs$name),], 
                      type = "b", auto.key = TRUE)
xy.fittedCS
#xy.fittedCS + as.layer(a)
xy.fittedCS + as.layer(b)

## * Question 3: Ignoring the correlation within families

## chunk 27
e.aovWrong <- aov(swabs ~ crowding + name , 
             data = dfL.swabs)
logLik(e.aovWrong)

## chunk 28
e.lmWrong <- lm(swabs ~ crowding + name , 
           data = dfL.swabs)
logLik(e.lmWrong)

## chunk 29
summary(e.lmWrong)$coef

## chunk 30
anova(e.lmWrong)

## chunk 31
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

## chunk 32
df.average <- aggregate(swabs ~ crowding + family, 
                        FUN = "mean", data = dfL.swabs)
head(df.average)

## ** Test the effect of crowding using a 1 way anova

## chunk 33
e.lmAverage <- lm(swabs ~ crowding, data = df.average)
anova(e.lmAverage)

## chunk 34
cbind(estimate = coef(e.lmAverage), confint(e.lmAverage))

## NOTE: inference in small samples
## chunk 35
anova(e.glsCS, type = "marginal")

## chunk 36
anova(e.lmer)

## compare2(e.glsCS, par = c("crowdingcrow","crowdingovercrow"))

## ** Graphical display of the model

## chunk 38
df.average$fitted <- fitted(e.lmAverage)

## chunk 39
boxplot(swabs ~ crowding, data = df.average)
points(fitted ~ crowding, data = df.average, col = "red")

## * Question 5: Adjusting on families using fixed effects
## ** Fitting the model

## chunk 41
e.lmFamily <- lm(swabs ~ family + name, 
                data = dfL.swabs)
logLik(e.lmFamily)

## ** Inference

## chunk 42
anova(e.lmFamily)

## chunk 43
keep.coef <- c("namefather", "namechild1", 
               "namechild2", "namechild3")
cbind(
    estimate = coef(e.lmFamily),
    confint(e.lmFamily)
)[keep.coef,]

## chunk 44
anova(e.lmFamily)
anova(e.glsCS, type = "marginal")
anova(e.lmer, type = "marginal")

## chunk 45
M <- cbind(
    summary(e.glsCS)$tTable[keep.coef,c("Value","Std.Error")],
    summary(e.lmFamily)$coef[keep.coef,c("Estimate","Std. Error")]
)
colnames(M) <- c("estimate.gls","se.gls","estimate.lm","se.lm")
M


## ** Graphical display of the model

## chunk 46
dfL.swabs$fittedFamily <- fitted(e.lmFamily)

## chunk 47
par(mfrow = c(1,2))
xy.fittedFamily <- xyplot(fittedFamily ~ name, group = family,
                          data = dfL.swabs[order(dfL.swabs$name),],
                          type = "l", ylab = "fitted swabs")
grid.arrange(a, xy.fittedFamily, ncol = 2)

## chunk 48
pdf("figures/fittedFamily.pdf", width = 12)
grid.arrange(xy.spaguetti, xy.fittedFamily, ncol = 2)
graphics.off()

## * Question 6: A more flexible two-level model
## ** Marginal correlation

## chunk 49
dfW.swabs <- dcast(dfL.swabs, value.var = "swabs",
                   formula = family+crowding ~ name)
head(dfW.swabs)

## chunk 50
rho <- cor(dfW.swabs[,c("mother","father","child1","child2","child3")])
rho

## chunk 51
rho.plot <- rho
rho.plot[lower.tri(rho)] <- NA
rho.plot

## chunk 52
Uname <- colnames(rho.plot)
image.plot(x = 1:length(Uname), y = 1:length(Uname), z = rho.plot, 
           zlim = c(-1,1), xlab = "", ylab = "", axes = FALSE)
axis(1, at = 1:length(Uname), labels = Uname)
axis(2, at = 1:length(Uname), labels = Uname)

## ** [Extra] Diagnostic plots for the CS model

## chunk 54
dfL.swabs$residuals <- residuals(e.glsCS, type = "normalized")

## chunk 55
boxplot(residuals ~ name, data = dfL.swabs)

## chunk 57
leveneTest(residuals ~ name, data = dfL.swabs)

## chunk 58
dfW.residuals <- dcast(dfL.swabs, 
                       value.var = "residuals",
                       formula = family~name)
head(dfW.residuals)

## chunk 59
cor(dfW.residuals[,c("mother","father","child1","child2","child3")])

## chunk 60
qqtest(dfL.swabs$residuals)

## chunk 62
shapiro.test(dfL.swabs$residuals)

## ** Relaxing assumptions using an unstructured covariance matrix

## chunk 63
e.glsUN <- gls(swabs ~ crowding + name, 
               data = dfL.swabs,
               correlation = corSymm(form = ~ as.numeric(name) | family),
               weights = varIdent(form = ~ 1 | name)
               )
logLik(e.glsUN)

## lme.1 <- lme(swabs ~ crowding + name, 
##               data = dfL.swabs,
##               random =~ factor(name) | family)
## logLik(lme.1)

## lme.2 <- lme(swabs ~ crowding + name, 
##              data = dfL.swabs,
##              random =~ 1 | family,
##              correlation = corSymm(form = ~ as.numeric(name)| family),
##              weight = varIdent(form = ~1|name))
## logLik(lme.2)
## summary(lme.2)

## e.lmerUN <- lmer(swabs ~ crowding + name + (name-1|family), data = dfL.swabs)
## e.lmerUN <- lmer(swabs ~ crowding + name + (name|family), data = dfL.swabs,
##                  control = lmerControl(check.nobs.vs.nRE = "ignore"))
## logLik(e.lmerUN)


## chunk 64
Sigma <- getVarCov(e.glsUN)
rownames(Sigma) <-  attr(e.glsUN$modelStruct$varStruct, "groupNames")
colnames(Sigma) <-  attr(e.glsUN$modelStruct$varStruct, "groupNames")

## chunk 65
sqrt(diag(Sigma))

## chunk 66
Corr <- cov2cor(Sigma)
Corr

## chunk 67
anova(e.glsCS, e.glsUN)

## chunk 68
cbind(CS.estimate = summary(e.glsCS)$tTable[,"Value"], 
      CS.se = summary(e.glsCS)$tTable[,"Std.Error"], 
      UN.estimate = summary(e.glsUN)$tTable[,"Value"],
      UN.se = summary(e.glsUN)$tTable[,"Std.Error"])

## * Using =data.table= 

## chunk 69
library(data.table)

## chunk 70
dtL.swabs <- as.data.table(dfL.swabs)

## ** Question 4

## chunk 71
dt.average <- dtL.swabs[,.(swabs = mean(swabs)),
                        by = c("family","crowding")]
head(dt.average)

## * Using =ggplot2= 

## chunk 72
library(ggplot2)

## ** Question 1

## chunk 73
gg.spaguetti <- ggplot(dfL.swabs, aes(x = name, y = swabs, 
                                      group = family, color = family))
gg.spaguetti <- gg.spaguetti + geom_line() + geom_point()
gg.spaguetti <- gg.spaguetti + facet_grid(~crowding, 
                                          labeller = label_both)
gg.spaguetti <- gg.spaguetti + xlab("")
gg.spaguetti

## chunk 75
gg.mean <- ggplot(dfL.swabs, aes(x = name, y = swabs, 
                                 group = crowding, color = crowding))
gg.mean <- gg.mean + stat_summary(geom = "line", fun.y = mean, 
                                  size = 3, fun.data = NULL)
gg.mean

## ** Question 3

## chunk 77
gg.fittedCS <- ggplot(dfL.swabs, aes(x = name, y = fitted.CS, 
                          group = crowding, color = crowding))
gg.fittedCS <- gg.fittedCS + geom_point() + geom_line()
gg.fittedCS <- gg.fittedCS + xlab("") + ylab("fitted value")
gg.fittedCS

## ** Question 4

## chunk 79
gg.fittedA <- ggplot(df.average, aes(x = crowding))
gg.fittedA <- gg.fittedA + geom_boxplot(aes(y = swabs, color = "data", shape = "data"))
gg.fittedA <- gg.fittedA + geom_point(aes(y = fitted, color = "fitted", shape = "fitted"), 
                                      size = 5)
gg.fittedA

## ** Question 6

## chunk 81
dfL.cor <- melt(rho.plot,
                value.name = "correlation")
head(dfL.cor)

## chunk 82
gg.cor <- ggplot(dfL.cor, aes(x = Var1, y = Var2, fill = correlation))
gg.cor <- gg.cor + geom_tile()
gg.cor <- gg.cor + scale_fill_gradient2(limits = c(-1,1), 
                                        low = "blue", high = "red")
gg.cor <- gg.cor + xlab("") + ylab("")
gg.cor
