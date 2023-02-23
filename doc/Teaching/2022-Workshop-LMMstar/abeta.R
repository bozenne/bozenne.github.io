#### Load R-packages to be used in the analysis #####
library(LMMstar)  # to do mixed model analyses
library(lattice)  # to make spaghettiplots and trend plots

# Load data
data("abetaW")
data('abetaL')

str(abetaW)
str(abetaL)


# Spaghettiplots
xyplot(fast~year|group, data=abetaL, group=id, type='b')

# Summary statistics:
summarize(fast~year+group|id, data=abetaL, na.rm=TRUE)


# Set reference levels (intercept):
abetaL$group <- relevel(abetaL$group, ref="HC")
abetaL$year <- relevel(abetaL$year, ref="0")


# Model with heterogeneous covariances:
fit2 <- lmm(fast~group*year,
            repetition=~year|id,
            structure=UN(~group),
            data=abetaL, 
            control = list(optimizer = "FS"))

summary(fit2)
sigma(fit2)
confint(fit2, columns=c('estimate','lower','upper', 'p.value'))
confint(fit2, columns=c('estimate','se'))

plot(fit2)

# Residual diagnostics
d2 <- residuals(fit2, type='all', keep.data=TRUE)

plot(d2$fitted, d2$r.studentized, main='Studentized residuals')
abline(h=0)
qqnorm(d2$r.studentized)
abline(0,1)
plot(d2$fitted, d2$r.scaled, main='Scaled residuals')
abline(h=0)
qqnorm(d2$r.scaled)
abline(0,1)
# Residual plots indicate some skewness.

# Outlier: Healthy control with high FAST-score at baseline.
subset(d2, r.studentized>4.5)

plot(d2$r.studentized~d2$group)
plot(d2$r.scaled~d2$group)
plot(d2$r.studentized~d2$year)
plot(d2$r.scaled~d2$year)
# Skewness is more prominent in the healthy control
# group due to the lower bound at zero.

# For comparison: Model with homogeneous covariances:
fit1 <- lmm(fast~group*year,
            repetition=~year|id,
            structure="UN",
            data=abetaL)

summary(fit1)
sigma(fit1)
confint(fit1, columns=c('estimate','lower','upper', 'p.value'))
confint(fit1, columns=c('estimate','se'))

plot(fit1)

# Residual diagnostics
d1 <- residuals(fit1, type='all', keep.data=TRUE)

plot(d1$fitted, d1$r.studentized, main='Studentized residuals')
abline(h=0)
qqnorm(d1$r.studentized)
abline(0,1)
plot(d1$fitted, d1$r.scaled, main='Scaled residuals')
abline(h=0)
qqnorm(d1$r.scaled)
abline(0,1)
# Residual plots indicate heterogeneity...

plot(d1$r.studentized~d1$group)
plot(d1$r.scaled~d1$group)
plot(d1$r.studentized~d1$year)
plot(d1$r.scaled~d1$year)
# Heterogeneity between groups, not over time.