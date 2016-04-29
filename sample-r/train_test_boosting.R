load("bestBoostPar.rda")
load("90_10_labelSplit.rda")

require(xgboost)
require(Matrix)

parameters = list(max_depth = bestBoostPar["max.depth"], 
                  eta = bestBoostPar["eta"], 
                  gamma = bestBoostPar["gamma"],
                  min_childe_weight = bestBoostPar["min_child_weight"], 
                  subsample = bestBoostPar["subsample"], 
                  colsample_bytree = bestBoostPar["colsample_bytree"], 
                  lambda = bestBoostPar["lambda"], 
                  alpha = bestBoostPar["alpha"])

#Xtrain = deltaTF_IDF_ImpFeatures_labeled90
#ytrain = y_labeled90

#Xtest = deltaTF_IDF_ImpFeatures_labeled10
#ytest = y_labeled10

set.seed(123)
trainIndx <- sample(1:nrow(x24.labeled), size = floor(nrow(x24.labeled) * 0.9), replace = F)

Xtrain <- x24.labeled[trainIndx, ]
ytrain <- y.labeled[trainIndx]

Xtest <- x24.labeled[-trainIndx, ]
ytest <- y.labeled[-trainIndx]


boostModel = xgboost(params = parameters, data = as.matrix(Xtrain), 
                     label = ytrain, nrounds = bestBoostPar["idxminError"], verbose = 1, 
                     nthred = 8, print.every.n = 10)

yhat = predict(boostModel, as.matrix(Xtest))
yhatPre.5 = yhat > 0.5

yhatPre.median <- yhat > yhat.median 

yhatPre.hi <- yhat > 0.51

yhatPre.vHi <- yhat > 0.55

accur.hi <- sum(yhatPre.hi == ytest) / length(ytest)
accur.vHi <- sum(yhatPre.vHi == ytest) / length(ytest)
accuracy.5 = sum(yhatPre.5 == ytest) / length(ytest)
accuracy.median = sum(yhatPre.median == ytest) / length(ytest)

save(boostModel, yhat, yhatPre.5, accuracy.5, file = "submission/April28_24features_xgb_5accur.rda")

save(boostModel, yhat, yhatPre.median, accuracy.median, file = "submission/April28_24features_xgb_median_accur.rda")

################ 
################ TRY SVM - linear - with sign flipped pos username and hashtags 
################ 

library(e1071)

svm.c.01 <- svm(x = as.matrix(Xtrain), y = ytrain, cost = 0.1, kernel = "linear")
save(svm.c.01, file = "submission/April28_svm_c0.1_linear.rda")

svm.c.01.pred <- predict(svm.c.01, as.matrix(Xtest))
save(svm.c.01.pred, file = "submission/April28_svm_c0.1_linear_pred.rda")

svm.c.01.radial <- svm(x = as.matrix(Xtrain), y = ytrain, cost = 0.1, kernel = "radial")
save(svm.c.01.radial, file = "submission/April28_svm_c0.1_radial.rda")

svm.c.01.rPred <- predict(svm.c.01.radial, as.matrix(Xtest))
save(svm.c.01.rPred, file = "submission/April28_svm_c0.1_radial_pred.rda")
