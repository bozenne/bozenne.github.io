library(lava)
library(data.table)
library(pROC)
library(ROCR)

set.seed(10)
n <- 1e4
m <- lvm(Y~Age)
categorical(m, K=2) <- ~Y

dTrain <- as.data.table(sim(m, n = n))
dTrain2 <- as.data.table(rbind(dTrain[Y==1],dTrain[Y==0][1:1000]))

dTest <- as.data.table(sim(m, n = n))
dTest2 <- rbind(dTest[Y==1],dTest[Y==0][1:1000])

prevalence <- c(train = mean(dTrain$Y), train2 = mean(dTrain2$Y), 
                test = mean(dTest$Y), test2 = mean(dTest2$Y))
print(prevalence)

## function
calcPerf <- function(dataTrain, dataTest){
  out <- list()
  
  out$e <- glm(Y~Age, data = dataTrain, family = "binomial")
  out$pred <- predict(out$e, newdata = dataTest, type = "response")
  out$roc <- roc(dataTest$Y, out$pred)
  perfTempo <- prediction(out$pred, dataTest$Y)
  out$perf <- data.frame(accuracy = performance(perfTempo, x.measure = c("acc"), measure = "err")@x.values[[1]],
                         sensitivity = performance(perfTempo, x.measure = c("sens"), measure = "err")@x.values[[1]],
                         specificity = performance(perfTempo, x.measure = c("spec"), measure = "err")@x.values[[1]],
                         threshold = performance(perfTempo, x.measure = c("acc"), measure = "err")@alpha.values[[1]])
  return(out)
}

## train models
res.TrainEq.TestEq <- calcPerf(dataTrain = dTrain, dataTest = dTest)
res.TrainEq.TestNeq <- calcPerf(dataTrain = dTrain, dataTest = dTest2)
res.TrainNeq.TestEq <- calcPerf(dataTrain = dTrain2, dataTest = dTest)
res.TrainNeq.TestNeq <- calcPerf(dataTrain = dTrain2, dataTest = dTest2)

# range(res.TrainNeq.TestEq$perf$threshold)
# range()

## check coefs
coef(res.TrainEq.TestEq$e) 
coef(res.TrainEq.TestNeq$e) # same

coef(res.TrainNeq.TestEq$e)
coef(res.TrainNeq.TestNeq$e) # same

## display roc curves
plot(res.TrainEq.TestEq$roc, col = "blue")
plot(res.TrainEq.TestNeq$roc, col = "red", add = TRUE)
plot(res.TrainNeq.TestEq$roc, col = "green", add = TRUE)
plot(res.TrainNeq.TestNeq$roc, col = "black", add = TRUE)
# independent of prevalence !

## accuracy
# prevalence in the test group
# accuracy  = sensitivity * prevalence  + specificity * (1 - prevalence)
tEq <- table(res.TrainEq.TestEq$pred>0.5, dTest$Y)
tNeq <- table(res.TrainEq.TestNeq$pred>0.5, dTest2$Y)

threshold <- 0.25
index0.5.Eq <- which.min(abs(res.TrainEq.TestEq$perf$threshold-threshold))
index0.5.Neq <- which.min(abs(res.TrainEq.TestNeq$perf$threshold-threshold))
res.TrainEq.TestEq$perf[index0.5.Eq,]
res.TrainEq.TestNeq$perf[index0.5.Neq,]
# accuracy depends on the prevalence

plot(res.TrainEq.TestEq$perf[,"accuracy"],res.TrainEq.TestEq$perf[,"threshold"], xlim = c(0,1))
points(res.TrainEq.TestNeq$perf[,"accuracy"],res.TrainEq.TestNeq$perf[,"threshold"], col = "blue")

