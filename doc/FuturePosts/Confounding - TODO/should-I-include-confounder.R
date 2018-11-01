# setwd("C:/Users/hpl802/Documents/Consult/Questions/Miscelaneous/Confounders")
library(lava)
library(ggplot2)
library(data.table)

betaC <- c(0,5,-3.5)
betaI <- 0:2
betaII <- c(0,-2,2)

#### no interaction
m <- lvm(Y ~ age + CONTRAST)
categorical(m, labels = c("A","B","C")) <- ~CONTRAST
regression(m,additive=FALSE,beta=betaC) <- Y~CONTRAST

set.seed(10)
df <- sim(m, 3e2)
head(df)

lm1a <- lm(Y~age, data=df)
summary(lm1a)
lm1b <- lm(Y~age+CONTRAST, data=df)
summary(lm1b)

gg <- ggplot(df, aes(x = age, y = Y, color = CONTRAST)) + geom_point()
seqAge <- sort(unique(df$age))
ls.newDf <- lapply(factor(c("A","B","C")), function(x){data.frame(age = seqAge, CONTRAST = x)})
newDf <- do.call(rbind,ls.newDf)
newDf$Y <- predict(lm1b, newdata = newDf)

gg <- gg + geom_line(data = newDf, aes(x = age, y = Y, color = CONTRAST))
ggsave("noInteraction.png")

####  interaction 1
dt2 <- as.data.table(df)
dt2[, Y := age + betaC[.GRP] + betaI[.GRP]*age + rnorm(nrow(.SD)), by = CONTRAST]

lm2a <- lm(Y~age*CONTRAST, data=dt2)
summary(lm2a)

gg <- ggplot(dt2, aes(x = age, y = Y, color = CONTRAST)) + geom_point()
seqAge <- sort(unique(dt2$age))
newDf$Y2 <- predict(lm2a, newdata = newDf)

gg <- gg + geom_line(data = newDf, aes(x = age, y = Y2, color = CONTRAST))
ggsave("interactionI.png")

####  interaction 2
dt3 <- as.data.table(df)
dt3[, Y := age + betaC[.GRP] + betaII[.GRP]*age + rnorm(nrow(.SD)), by = CONTRAST]

lm3a <- lm(Y~age*CONTRAST, data=dt3)
summary(lm3a)

gg <- ggplot(dt3, aes(x = age, y = Y, color = CONTRAST)) + geom_point()
seqAge <- sort(unique(dt3$age))
newDf$Y3 <- predict(lm3a, newdata = newDf)

gg <- gg + geom_line(data = newDf, aes(x = age, y = Y3, color = CONTRAST))
ggsave("interactionII.png")
