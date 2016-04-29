# April 29 - final script 
#
# Trying to: 
#
# 1 - ensemble set of XGBoost runs 
# 2 - ensemble set of XGB + SVM runs (if more uncorrelated = generally better pred)
# 3 - want to try average of each binary pred between yhats 
# 4 - want to try majority rule for each binary pred between yhats 

require(xgboost)

# load in x24 -> 20 importance features out of 290 + 4 scores 
load("data/April28_x24_labeled_sign_change.rda")
load("data/April28_x24_unlabeled_sign_change.rda")
load("data/y_AllRaw.rda")
y50k.unlabeled <- y_AllRaw[1:50000]
yLabeled <- y_AllRaw[50001:1578627]

# x24.labeled 
# x24.unlabeled 

Xtrain.all <- x24.labeled 
Xtest.all <- x24.unlabeled

Ytrain.all <- yLabeled 
 

# get 10percent of data for ensemble model runs 
set.seed(123)
trainIndx.10perc <- sample(1:nrow(x24.labeled), size = floor(nrow(x24.labeled) * 0.1), replace = F)
save(trainIndx.10perc, file = "submission/April29_trainIndx_10perc.rda")

trainIndx.50perc <- sample(1:nrow(x24.labeled), size = floor(nrow(x24.labeled) * 0.5), replace = F)
save(trainIndx.50perc, file = "submission/April29_trainIndx_50perc.rda")

# # # # this creates a subsample of 152k tweets .... split into 80 / 20 train / test or something 

x24.10perc.data <- x24.labeled[trainIndx.10perc, ]
y.10perc.data <- yLabeled[trainIndx.10perc]

x24.50perc.data <- x24.labeled[trainIndx.50perc, ]
y.50perc.data <- yLabeled[trainIndx.50perc]

trainSet <- sample(1:nrow(x24.10perc.data), size = floor(nrow(x24.10perc.data) * 0.8), replace = F)
save(trainSet, file = "submission/April29_80split_10perc.rda")

trainSet <- sample(1:nrow(x24.50perc.data), size = floor(nrow(x24.50perc.data) * 0.8), replace = F)

Xtrain <- x24.10perc.data[trainSet, ]
Ytrain <- y.10perc.data[trainSet]

Xtest <- x24.10perc.data[-trainSet, ]
Ytest <- y.10perc.data[-trainSet]

# 

Xtrain <- x24.50perc.data[trainSet, ]
Ytrain <- y.50perc.data[trainSet]

Xtest <- x24.50perc.data[-trainSet, ]
Ytest <- y.50perc.data[-trainSet]

# # # # roughly 122k train, 31k test 

# XGB best boost params from 300 random parameter sets + xgb.cv 
load("data/bestBoostPar.rda")

# get 24feature XGB models...accuracy threshold at 0.5 
#load("submission/April28_24features_xgb_5accur.rda")
param1 <- list("objective" = "binary:logistic",
               "max.depth" = 3,
               "eta" = 1,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

param2 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

param3 <- list("objective" = "binary:logistic",
               "max.depth" = 9,
               "eta" = 0.1629,
               "gamma" = 14.667,
               "min_child_weight" = 58.234,
               "booster" = "gbtree",
               "subsample" = 0.97,
               "colsample_bytree" = 0.911,
               "lambda" = 28.079,
               "alpha" = 13.4198)
nrounds.param3 <- 297

param4 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

xgb.param1 <- xgboost(param = param1, data = as.matrix(Xtrain), 
                      label = Ytrain, nrounds = 100, verbose = 1,
                      print.every.n = 20)

#xgb.save(xgb.param1, "submission/April29_xgb_10perc_param1.model")
#xgb.save(xgb.param1, "submission/April29_xgb_50perc_param1.model")

xgb.param2 <- xgboost(param = param2, data = as.matrix(Xtrain), 
                      label = Ytrain, nrounds = 100, verbose = 1,
                      print.every.n = 20)

#xgb.save(xgb.param2, "submission/April29_xgb_10perc_param2.model")
#xgb.save(xgb.param2, "submission/April29_xgb_50perc_param2.model")

xgb.param3 <- xgboost(param = param3, data = as.matrix(Xtrain), 
                      label = Ytrain, nrounds = nrounds.param3, verbose = 1,
                      print.every.n = 20)

#xgb.save(xgb.param3, "submission/April29_xgb_10perc_param3.model")
#xgb.save(xgb.param3, "submission/April29_xgb_50perc_param3.model")

offset <- 5000
num_rounds <- 3000

xgtest <- xgb.DMatrix(data = Xtest)
xgtrain <- xgb.DMatrix(data = as.matrix(Xtrain), label = Ytrain)
xgval <- xgb.DMatrix(data = as.matrix(Xtrain[1:offset, ]), label = Ytrain[1:offset])

watchlist <- list(val = xgval, train = xgtrain)

num.folds <- 5

xgb.param4.train <- xgb.train(params = param2, data = xgtrain, nround = num_rounds, 
                              print.ever.n = 20, watchlist = watchlist, early.stop.round = 50,
                              maximize = FALSE)

xgb.param4.train$bestScore 
xgb.param4.train$bestInd

#####

# Custom Gini functions for XGB 

SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}

NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

# wrap up into a function to be called within xgboost.train
evalgini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- NormalizedGini(as.numeric(labels),as.numeric(preds))
  return(list(metric = "Gini", value = err))
}


xgb.param4.trainMod <- xgb.train(params = param2, data = xgtrain, feval = evalgini, nround = num_rounds, 
                              print.ever.n = 20, watchlist = watchlist, early.stop.round = 50,
                              maximize = TRUE)

xgb.param4.trainMod$bestScore
xgb.param4.trainMod$bestInd

### Do predictions: 

paramFin <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

xgb.fin.notMod <- xgb.train(params = paramFin, data = xgtrain, feval = evalgini, nround = 127, 
                                 print.ever.n = 20, watchlist = watchlist, early.stop.round = 50,
                                 maximize = TRUE)

xgb.fin.giniMod <- xgb.train(params = paramFin, data = xgtrain, feval = evalgini, nround = 131, 
                            print.ever.n = 20, watchlist = watchlist, early.stop.round = 50,
                            maximize = TRUE)

xgb.fin.notMod.pred <- predict(xgb.fin.notMod, xgtest)

xgb.fin.giniMod.pred <- predict(xgb.fin.giniMod, xgtest)


#####

xgb.param4.CV <- xgb.cv(param = param2, data = as.matrix(Xtrain), 
                      label = Ytrain, nrounds = 4000,
                      nfold = num.folds, showsd = TRUE, 
                      verbose = 1, print.every.n = 20,
                      metrics = list("error", "auc"),
                      early.stop.round = 100,
                      prediction = TRUE)

xgb.param4 <- xgboost(param = param2, data = as.matrix(Xtrain), 
                      label = Ytrain, nrounds = 100, verbose = 1,
                      print.every.n = 20)

# Now get yhats! 

pred.param1 = predict(xgb.param1, as.matrix(Xtest))
err.param1 <- mean(as.numeric(pred.param1 > 0.5) != Ytest)
print(paste("test-error (param1) = ", err.param1))

xgb.param1.yhat <- pred.param1 
xgb.param1.yhat.binary <- (as.numeric(pred.param1 > 0.5) != Ytest)

# 

pred.param2 = predict(xgb.param2, as.matrix(Xtest))
err.param2 <- mean(as.numeric(pred.param2 > 0.5) != Ytest)
print(paste("test-error (param2) = ", err.param2))

xgb.param2.yhat <- pred.param2 
xgb.param2.yhat.binary <- (as.numeric(pred.param2 > 0.5) != Ytest)

# 

pred.param3 = predict(xgb.param3, as.matrix(Xtest))
err.param3 <- mean(as.numeric(pred.param3 > 0.5) != Ytest)
print(paste("test-error (param3) = ", err.param3))

xgb.param3.yhat <- pred.param3 
xgb.param3.yhat.binary <- (as.numeric(pred.param3 > 0.5) != Ytest)
xgb.param3.yhat.median <- (as.numeric(pred.param3 > median(xgb.param3.yhat)) != Ytest)

# compare results of 2 models to Y-actual 
compare.xgb.10perc <- data.frame(yhat_xgb1 = xgb.param1.yhat.binary,
                                 yhat_xgb2 = xgb.param2.yhat.binary,
                                 yhat_xgb3 = xgb.param3.yhat.binary,
                                 yactual = Ytest)

#compare.xgb.50perc <- data.frame(yhat_xgb1 = xgb.param1.yhat,
#                                 yhat_xgb2 = xgb.param2.yhat,
#                                 yhat_xgb3 = xgb.param3.yhat,
#                                 yactual = Ytest)

# SVM with C = 0.1 and Sigma = 0.1 
library(e1071)
svm.c.01 <- svm(x = as.matrix(Xtrain), y = Ytrain, 
                cost = 0.1, kernel = "linear", scale = FALSE)

save(svm.c.01, file = "submission/April29_svm_c0.1_linear.rda")

svm.c.01.pred <- predict(svm.c.01, as.matrix(Xtest))
save(svm.c.01.pred, file = "submission/April29_svm_c0.1_linear_pred.rda")

svm.c.01.radial <- svm(x = as.matrix(Xtrain), y = ytrain, 
                       cost = 0.1, sigma = 0.1, kernel = "radial", scale = FALSE)

save(svm.c.01.radial, file = "submission/April29_svm_c0.1_radial.rda")

svm.c.01.rPred <- predict(svm.c.01.radial, as.matrix(Xtest))
save(svm.c.01.rPred, file = "submission/April29_svm_c0.1_radial_pred.rda")

# Now c = 1

svm.c.1 <- svm(x = as.matrix(Xtrain), y = Ytrain, 
                cost = 1, kernel = "linear", scale = FALSE)

save(svm.c.1, file = "submission/April29_svm_c1_linear.rda")

svm.c.1.pred <- predict(svm.c.1, as.matrix(Xtest))
save(svm.c.1.pred, file = "submission/April29_svm_c1_linear_pred.rda")

svm.c.1.radial <- svm(x = as.matrix(Xtrain), y = ytrain, 
                       cost = 1, sigma = 0.1, kernel = "radial", scale = FALSE)

save(svm.c.1.radial, file = "submission/April29_svm_c1_radial.rda")

svm.c.1.rPred <- predict(svm.c.1.radial, as.matrix(Xtest))
save(svm.c.1.rPred, file = "submission/April29_svm_c1_radial_pred.rda")

# save predictions 

compare.xgb.svm.10perc <- data.frame(yhat_lin_svm0.1 = svm.c.01.pred,
                                 yhat_rad_svm0.1 = svm.c.01.rPred,
                                 yhat_lin_svm1 = svm.c.1.pred,
                                 yhat_rad_svm1 = svm.c.1.rPred,
                                 yhat_xgb1 = xgb.param1.yhat,
                                 yhat_xgb2 = xgb.param2.yhat,
                                 yhat_xgb3 = xgb.param3.yhat,
                                 yactual = Ytest)


compare.xgb.svm.10perc[1:100, ]
