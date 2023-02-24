#### Load R-packages to be used in the analysis #####
library(LMMstar)  # to do mixed model analyses
library(lattice)  # to make spaghettiplots and trend plots

# Load data
data('ckdW')
data('ckdL')

wide <- ckdW
long <- ckdL

str(wide)
str(long)


# Spaghettiplots
xyplot(aix~time|allocation, data=long, group=id, type='b')

# Summary statistics:
summarize(aix~time+allocation|id, data=long, na.rm=TRUE)


# Define time-varying treatment variable:
long$treat <- long$allocation
long$treat[long$time==0] <- "A"
table(long$time, long$treat)

# Set reference levels (intercept):
long$treat <- relevel(long$treat, ref="A")
long$time <- relevel(long$time, ref="0")

# Fit the constrained linear mixed model:
fitc <- lmm(aix~time+time:treat,
            repetition=~time|id,
            structure="UN",
            data=long, 
            control = list(optimizer = "FS"))

summary(fitc)


# Visualize model fit:
plot(fitc)
plot(fitc, color = "allocation")

# Similar model with a constrained heterogeneous variance:
long$time.treat <- interaction(long$time, long$treat)

fitcc <- lmm(aix~time+time:treat,
            repetition=~time:treat|id,
            structure="UN",
            data=long, 
            control = list(optimizer = "FS"))

summary(fitcc)

# DOESN'T WORK CURRENTLY:
plot(fitcc, color = "allocation")
