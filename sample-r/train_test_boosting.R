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

Xtrain = deltaTF_IDF_ImpFeatures_labeled90
ytrain = y_labeled90

Xtest = deltaTF_IDF_ImpFeatures_labeled10
ytest = y_labeled10


boostModel = xgboost(params = parameters, data = Xtrain, 
                     label = ytrain, nrounds = bestBoostPar["idxminError"], verbose = 0, 
                     nthred = 8)

yhat = predict(boostModel, Xtest)
yhatPre = yhat > 0.5

accuracy = sum(yhatPre == ytest) / length(ytest)

save(boostModel, yhat, yhatPre, accuracy, file = "bestBoostImpFeatures.rda")

