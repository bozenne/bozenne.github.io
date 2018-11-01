library(lava)
library(data.table)
library(AER) # for ivreg

## * linear regression - attenuation
set.seed(10)
n <- 1e4
vec.param <- c(sigma2 = 2, beta = 1.5)
mSim <- lvm(Y ~ beta * X, Xobs[0:sigma2] ~ X)
dt.data <- as.data.table(sim(mSim, n, p = vec.param))

e.lm <- lm(Y~Xobs, data = dt.data)
betaObs <- coef(e.lm)["Xobs"]
sigmaX <- var(dt.data$X)
sigmaU <- sigma(lm(Xobs~X, data = dt.data))^2
lambda <- sigmaX/(sigmaX+sigmaU)

betaObs/lambda


## * linear regression - attenuation with a covariate
set.seed(10)
n <- 1e4
vec.param <- c(sigma2 = 2)
mSim <- lvm(c(Y,Xobs) ~ X, X ~ Age)
dt.data <- as.data.table(sim(mSim, n, p = vec.param))

plot(mSim)

## ** check attenuation bias
e.lm <- lm(Y~Xobs, data = dt.data)
betaObs <- coef(e.lm)["Xobs"]
sigmaX <- var(dt.data$X)
sigmaU <- sigma(lm(Xobs~X, data = dt.data))^2
lambda <- sigmaX/(sigmaX+sigmaU)
c(naive = betaObs, corrected = betaObs/lambda)

## ** lvm
m <- lvm(Y ~ eta, Xobs ~ eta, eta ~ Age)
e <- estimate(m, data = dt.data)
summary(e)
coef(e)["Xobs~eta"] * (coef(e)["eta~~eta"]+coef(e)["Xobs~~Xobs"])/coef(e)["Xobs~~Xobs"]

## ** IV estimator
olsreg1 <- lm(Xobs ~ Age, data = dt.data)
dt.data$pred1s <- fitted(olsreg1)
olsreg2 <- lm(Y ~ pred1s, data = dt.data)

summary(lm(Y~Xobs, data = dt.data))
summary(olsreg2)
summary(ivreg(Y~Xobs|Age, data = dt.data))


## * linear regression - real situation
set.seed(10)
n <- 1e4
vec.param <- c(sigma2 = 2, beta = 1.5)
mSim <- lvm(Y ~ beta * X + Gender, Xobs[0:sigma2] ~ X + IM, X ~ Age)
categorical(mSim, labels = c("Male","Female")) <- ~Gender
dt.data <- as.data.table(sim(mSim, n, p = vec.param))

## ** check attenuation bias
e.lm <- lm(Y~Xobs, data = dt.data)
betaObs <- coef(e.lm)["Xobs"]
sigmaX <- var(dt.data$X)
sigmaU <- sigma(lm(Xobs~X, data = dt.data))^2
lambda <- sigmaX/(sigmaX+sigmaU)
c(naive = betaObs, corrected = betaObs/lambda)

## ** lvm
m <- lvm(Y ~ eta + Gender, Xobs ~ eta + IM, eta ~ Age)
#m <- lvm(Y ~ eta, Xobs ~ eta, eta ~ Age)
e <- estimate(m, data = dt.data)
summary(e)$coef

vcov(e)


## ** IV estimator
olsreg1 <- lm(Xobs ~ Age, data = dt.data)
dt.data$pred1s <- fitted(olsreg1)
olsreg2 <- lm(Y ~ pred1s + Gender, data = dt.data)

summary(lm(Y~Xobs, data = dt.data))
summary(olsreg2)
summary(ivreg(Y~Xobs+Gender|Age, data = dt.data))

