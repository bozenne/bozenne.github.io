## chunk 2
library(lava)
library(mice)
library(data.table)
library(ggplot2)

## * Simulate data (just to have an example to work with)

## chunk 3
mSim <- lvm(Y~group+season+bmi+gender+age)
categorical(mSim, labels = c("winter","summer")) <- ~season
categorical(mSim, labels = c("SAD","HC")) <- ~group
categorical(mSim, labels = c("Male","Female")) <- ~gender
distribution(mSim,~bmi) <- lava::gaussian.lvm(mean = 22, sd = 3)
distribution(mSim,~age) <- lava::uniform.lvm(20,80)

## chunk 4
n <- 1e2
set.seed(10)
dt.data <- as.data.table(sim(mSim,n))

## chunk 5
dt.data[1:10, bmi:=NA]

## * Working with mice
## ** Step 1: Inspect the missing data pattern

## chunk 6
colSums(is.na(dt.data))

## chunk 7
md.pattern(dt.data)

## ** Step 2: Define imputation model

## chunk 8
all.variables <- c("Y","group","season","bmi","gender","age")
n.variables <- length(all.variables)
Mlink <- matrix(0, n.variables, n.variables,
                dimnames = list(all.variables,all.variables))
Mlink[c("group","season","gender","age"),"bmi"] <- 1
Mlink

## ** Step 3: Generate imputed datasets

## chunk 9
n.imputed <- 3 ## number of imputed datasets
dt.mice <- mice(dt.data,
                m=n.imputed, 
                maxit = 50, # number of iterations to obtain the imputed dataset
                predictorMatrix = Mlink,
                method = 'pmm', # Predictive mean matching, only ok for continuous variables, it is possible to set constrains for positive variables
                seed = 500, printFlag = FALSE)
summary(dt.mice)

## ** Step 4: Check the imputed datasets
## *** Convergence of the imputation algorithm

## chunk 10
plot(dt.mice)

## *** Visualizing the imputed values

## chunk 11
dt.mice$imp$bmi

## chunk 12
apply(dt.mice$imp$bmi,2,quantile)

## chunk 13
boxplot(dt.mice$imp$bmi)

## chunk 14
dt.bmi <- rbind(data.table(bmi = unlist(dt.mice$imp$bmi), imputed = TRUE),
                data.table(bmi = na.omit(dt.data$bmi), imputed = FALSE))

## chunk 15
gg1.bmi <- ggplot(dt.bmi, aes(bmi, group = imputed, fill = imputed))
gg1.bmi <- gg1.bmi + geom_histogram(aes(y=..count../sum(..count..)),position = "dodge")
gg1.bmi

## chunk 16
stripplot(dt.mice, bmi~.imp, pch=20, cex=2)

## ** Step 3: Fit the statical model on each imputed dataset

## chunk 17
e.mice <- with(data = dt.mice,
               lm(Y~group+season+bmi+gender+age)
               )
e.mice

## chunk 18
e.mice$analyses[[1]]

## chunk 19
dt.tempo <- copy(dt.data)
dt.tempo[is.na(bmi), bmi := dt.mice$imp$bmi[,1]]
lm(Y ~ group + season + bmi + gender + age, data  = dt.tempo)

## ** Step 4: Pool the results over the imputed datasets

## chunk 20
ePool.mice <- pool(e.mice)
summary(ePool.mice)

## chunk 21
Q.coef <- colMeans(do.call(rbind,lapply(e.mice$analyses, coef)))
Q.coef

## chunk 22
covW <- Reduce("+",lapply(e.mice$analyses, vcov))/n.imputed
covW

## chunk 23
ls.diffCoef <- lapply(e.mice$analyses, function(iI){coef(iI)-Q.coef})
covB <- Reduce("+",lapply(ls.diffCoef,tcrossprod))/(n.imputed-1)
covB

## chunk 24
covE <- covB/n.imputed
covE

## chunk 25
covT <- covW + covB + covE

## chunk 26
sqrt(diag(covT))

## * Special case: imputation using a specific law and no covariate

## chunk 27
mice.impute.SI_uninf <- function(y, ry, ...){ ## uniform distribution
    n.NA <- sum(ry==FALSE)
    sample <- stats::runif(n.NA, min = 0, max = 1)
    return(cbind(sample))
}

mice.impute.SI_tnorm <- function(y, ry, ...){ ## truncated normal law
    require(truncnorm)
    n.NA <- sum(ry==FALSE)
    sample <- rtruncnorm(n.NA, a = 0, b = 1, mean = 1, sd = 0.1)
    return(cbind(sample))
}

## chunk 28
n.imputed <- 50 ## number of imputed datasets
dt.mice2 <- mice(dt.data,
                m=n.imputed, 
                maxit = 1, # not relevant
                predictorMatrix = Mlink, # not relevant
                method = 'SI_tnorm', # function previous define (without "mice.impute.")
                seed = 500, printFlag = FALSE)

## chunk 29
quantile(unlist(dt.mice$imp$bmi))

## chunk 30
hist(unlist(dt.mice2$imp$bmi))

## chunk 31
stripplot(dt.mice2, bmi~.imp, pch=20, cex=2)

## * Reporting guideline 
