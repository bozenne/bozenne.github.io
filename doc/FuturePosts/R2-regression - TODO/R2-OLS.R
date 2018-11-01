n <- 1000
require(lava)
require(ggplot2)
require(data.table)

#### simulate X and Y unrelated
m1 <- lvm(Y ~ X)
regression(m1, Y ~ X) <- 0.6
df.m1 <- sim(m1)

#### X and Y with 0 mean
lm.m0 <- lm(Y ~ -1 + X, data = df.m1)
lm.m1 <- lm(Y ~ X, data = df.m1)
summary(lm.m0)$r.squared
1-sum(lm.m0$residuals^2)/sum(df.m1$Y^2) # explicit formula

summary(lm.m1)$r.squared
1-var(lm.m1$residuals)/var(df.m1$Y)  # explicit formula
# nul R2 in both cases

#### non nul mean for Y
df.m1$Y2 <- df.m1$Y + 3

lm2.m0 <- lm(Y2 ~ -1 + X, data = df.m1)
lm2.m1 <- lm(Y2 ~ X, data = df.m1)
summary(lm2.m0)$r.squared
1-sum(lm2.m0$residuals^2)/sum(df.m1$Y2^2)

summary(lm2.m1)$r.squared
1-var(lm2.m1$residuals)/var(df.m1$Y2)
# nul R2 in both cases

#### non nul mean for Y and X
df.m1$X2 <- df.m1$X + 5

lm3.m0 <- lm(Y2 ~ -1 + X2, data = df.m1)
lm3.m1 <- lm(Y2 ~ X2, data = df.m1)
summary(lm3.m0)$r.squared
1-sum(lm3.m0$residuals^2)/sum(df.m1$Y2^2)
## non nul R2 because X is catching the mean of Y
coef(lm3.m0)*mean(df.m1$X2) - mean(df.m1$Y2)

summary(lm3.m1)$r.squared
1-var(lm3.m1$residuals)/var(df.m1$Y2)

plot(df.m1$X2, df.m1$Y2)
cor(df.m1$X2, df.m1$Y2)

lm3.m1 <- lm(Y2 ~ X2, data = df.m1)
summary(lm3.m1)$r.squared

lm3.m0 <- lm(Y2 ~ -1 + X2, data = df.m1)


Ycentre <- df.m1$Y2-mean(df.m1$Y2)
sum( (lm3.m0$fitted.values-mean(df.m1$Y2))^2 )/sum(Ycentre^2)


#### Interval dependent R2
df.m1$X2 <- df.m1$X + 5


n <- 25
sigma_Y <- 4
X <- rnorm(n = n, mean = 0, sd = 3)
Y <- 3 + 2*X + rnorm(n = n, mean = 0, sd = sigma_Y)
# X <- runif(n = n, min = 0, max = 3)
# Y <- 3 + 2*X + runif(n = n, min = 0, max = 4)
df.m2 <- data.frame(Y = Y, X = X)

index.I1 <- which(df.m2$X<=median(df.m2$X))
index.I2 <- which(df.m2$X>median(df.m2$X))
index.resample <- sample.int(n,n/2, replace = FALSE)

summary(lm0.I <- lm(Y ~ X, data = df.m2))$r.squared
summary(lm0bis.I <- lm(Y ~ X, data = df.m2[index.resample,]))$r.squared
summary(lm1.I <- lm(Y ~ X, data = df.m2[index.I1,]))$r.squared
summary(lm2.I <- lm(Y ~ X, data = df.m2[index.I2,]))$r.squared

df.res <- data.table(rbind(data.frame("Y" = df.m2$Y, "X" = df.m2$X, "fitted" = lm0.I$fitted.values, method = "lm0.I"),
                           data.frame("Y" = df.m2$Y[index.resample], "X" = df.m2$X[index.resample], "fitted" = lm0bis.I$fitted.values, method = "lm0bis.I"),
                           data.frame("Y" = df.m2$Y[index.I1], "X" = df.m2$X[index.I1], "fitted" = lm1.I$fitted.values, method = "lm1.I"),
                           data.frame("Y" = df.m2$Y[index.I2], "X" = df.m2$X[index.I2], "fitted" = lm2.I$fitted.values, method = "lm2.I")
))
setkey(df.res, method, Y)
df.res[,residuals := fitted - Y]
df.res[, .(varY = var(Y), varResidual = var(residuals), R2 = 1 - var(residuals)/var(Y)), by = method]
ggbase <- ggplot(df.res, aes(x = X)) 
ggbase + geom_point(aes(y = Y), alpha = 1) + geom_line(aes(col = method, group = method, y = fitted))


#### lack of fit ####
set.seed(10)
n <- 100
X <- rnorm(n)
fctX  <- function(x){x + 0.3 * x^2}
Y <- fctX(X) + rnorm(n, sd = 0.1)

plot(X,Y)
curve(fctX, -5,5, add = TRUE, lty = 2)
points(X, lm(Y ~ X)$fitted, col = "red", type = "l")

plot(X,Y-lm(Y ~ X)$fitted)

summary(lm(Y ~ X))$r.squared

summary(lm(Y[X < 0] ~ X[X < 0]))$r.squared
summary(lm(Y[X > 0] ~ X[X > 0]))$r.squared
