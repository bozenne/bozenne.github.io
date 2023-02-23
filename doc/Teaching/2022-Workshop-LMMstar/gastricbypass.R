### INTRODUCTION TO LINEAR MIXED MODELS FOR REPEATED MEASUREMENTS DATA #######
###### ANALYSIS OF A SINGLE GROUP FOLLOW-UP STUDY WITH R-PACKAGE LMMstar ###########

#### Load R-packages to be used in the analysis #####
library(LMMstar)  # to do mixed model analyses
library(lattice)  # to make spaghettiplots and trend plots
library(psych)    # OPTIONAL: scatterplots with effects

## Load the case study data (wide format)
data("gastricbypassW")
wide <- gastricbypassW
str(wide)
data("gastricbypassL")
long <- gastricbypassL
str(long)

#### Descriptive statistics #####

# Spaghettiplot
xyplot(weight~time, data=long, group=id, type='b')

# Scatterplot matrices
weight.replicates <- wide[,c('weight1','weight2','weight3','weight4')]
plot(weight.replicates)

# Scatterplot matrices with effects (psych-package):
pairs.panels(weight.replicates, smooth=FALSE, ellipses=TRUE)

# Summary statistics:
w.summaries <- summarize(weight~time, data=long, na.rm=TRUE)
print(w.summaries, digits=4)

# Visualize trends in summary statistics
xyplot(mean~time, data=w.summaries, type='b')
xyplot(sd~time, data=w.summaries, type='b')

# Summary statistics including correlations:
summarize(weight~time|id, data=long, na.rm=TRUE)


#### Linear mixed model analysis of a single group study #####

# Set reference point (intercept) for time factor:
long$time <- relevel(long$time, ref="3monthsBefore")

fit <- lmm(weight~time,
           repetition=~time|id,
           structure="UN",
           data=long)

# Model summary, loads of information:
summary(fit)

# Visualize model fit (plot predicted means):
plot(fit)

# Extract estimates and confidence intervals:
confint(fit)

# Same including the p-values:
confint(fit, columns=c('estimate','lower','upper','p.value'))


# Extract covariance matrix:
sigma(fit)

# F-test for time effect:
anova(fit)

# Make a dataset with covariate values for prediction:
pred <- long[,c('visit','time')]
# Reduce to one of each value:
pred <- unique(pred)
# Add predicted means to the dataframe:
pred <- cbind(pred, predict(fit, newdata=pred))
print(pred, digits=4)

# Residual diagnostics
rdiag <- residuals(fit, type='all', keep.data=TRUE)

plot(rdiag$fitted, rdiag$r.studentized, main='Studentized residuals')
abline(h=0)
qqnorm(rdiag$r.studentized)
abline(0,1)
# Note: Studentized residuals shows some skewness in the marginal distribution. 
plot(rdiag$fitted, rdiag$r.scaled, main='Scaled residuals')
abline(h=0)
qqnorm(rdiag$r.scaled)
abline(0,1)
# Note: Scaled residuals look good.

##### Alternative syntax: Means over time ##########
# Fit the model without an intercept using -1 in the model formula:
fit.means <- lmm(weight~-1+time,
                 repetition=~time|id,
                 structure="UN",
                 data=long)

# Extract estimated means and confidence intervals:
confint(fit.means)


#### Small sample adjustment to degrees of freedom ######
# Note that lmm approximates the degrees of freedom.
# In this case the approximation is close to the exact
# degrees of freedom we get from the paired t-test.
t.test(wide$weight2, wide$weight1, paired=TRUE)$conf.int
t.test(wide$weight3, wide$weight1, paired=TRUE)$conf.int
t.test(wide$weight4, wide$weight1, paired=TRUE)$conf.int
t.test(wide$weight3, wide$weight2, paired=TRUE)$conf.int
t.test(wide$weight4, wide$weight2, paired=TRUE)$conf.int
t.test(wide$weight4, wide$weight3, paired=TRUE)$conf.int
# However, linear mixed models implicitly imputes missing 
# data under a missing at random assumption, whereas
# the paired t-test makes complete case analysis.
