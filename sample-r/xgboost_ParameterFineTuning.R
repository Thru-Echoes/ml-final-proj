##The package for boosting.
library(xgboost)

##Load the data set, and split it into training and testing sets.
library(spls)
data(prostate) # load the data
pro.vars = prostate$x # predictors
pro.res = prostate$y # responses

pro.n = length(pro.res)
set.seed(1)
RandomIndex.2 = sample(1:pro.n, size = pro.n, replace = FALSE)

test.pro.vars = pro.vars[RandomIndex.2[1:25], ]
train.pro.vars = pro.vars[RandomIndex.2[26:pro.n], ]

test.pro.res = pro.res[RandomIndex.2[1:25]]
train.pro.res = pro.res[RandomIndex.2[26:pro.n]]

##Set the variable names for boosting and fine-tuning the parameters.
train_X = train.pro.vars # matrix of predictors for the training set
train_y = train.pro.res # vector of responses for the training set

##1: Fix some initial parameters to tune the parameter num_parallel_tree.
nfold = 3 # number of folds in CV
eta = 0.1 # learning rate
max_depth = 5 # maximum depth of a tree
min_child_weight = 1 # minimum sum of instance weight needed in a child for partition
gamma = 0.1 # minimum loss reduction needed for partition
subsample = 0.8 # subsample ratio for observations
colsample_bytree = 0.8 # subsample ratio for predictors 
nrounds = 2 # number of rounds for boosting
lambda = 1 # L2 regularization term on weights; increase to make model more conservative
alpha = 0 # L1 regularization term on weights; increase to make model more conservative

seqNumTree = seq(from = 10, to = 30, by = 10) # the sequence of number of trees to try
n.seqNumTree = length(seqNumTree)
ErrorMetrics_NumTree = matrix(NA, nrow = 3, ncol = n.seqNumTree)
rownames(ErrorMetrics_NumTree) = c("error", "auc", "logloss")
colnames(ErrorMetrics_NumTree) = seqNumTree
# the matrix to record test errors: "error", "auc", and "logloss"

for (i in 1:n.seqNumTree){
  parameters = list(objective = "binary:logistic", eta = eta, max_depth = max_depth, 
                    min_child_weight = min_child_weight, gamma = gamma, subsample = 
                      subsample, colsample_bytree = colsample_bytree, lambda = lambda, 
                    alpha = alpha, num_parallel_tree = seqNumTree[i])
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = nrounds)
  ErrorMetrics_NumTree[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_NumTree[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_NumTree[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_NumTree = seqNumTree[which(ErrorMetrics_NumTree["error", ] == min(ErrorMetrics_NumTree["error", ]))]
optimalAUC_NumTree = seqNumTree[which(ErrorMetrics_NumTree["auc", ] == max(ErrorMetrics_NumTree["auc", ]))]
optimalLogloss_NumTree = seqNumTree[which(ErrorMetrics_NumTree["logloss", ] == min(ErrorMetrics_NumTree["logloss", ]))]
# remark: 1) ErrorMetrics_NumTree contains the error metrics (row) for each parameter value (column)
#         2) optimalError_NumTree, optimalAUC_NumTree, and optimalLogloss_NumTree contain the optimal num_parallel_tree
#            based on their respective error metrics


##2: Use the optimal number of trees and fix the other parameters as before, 
##   to tune the parameters max_depth and min_child_weight.

##Choose the optimal number of trees using one of the metrics. Comment out the other two.
num_parallel_tree = min(optimal_NumTree[ ,"error"]) # based on error
num_parallel_tree = min(optimal_NumTree[ ,"auc"]) # based on auc
num_parallel_tree = min(optimal_NumTree[ ,"logloss"]) #based on logloss

nfold = nfold 
eta = eta 
# max_depth = max_depth 
# min_child_weight = min_child_weight 
gamma = gamma 
subsample = subsample 
colsample_bytree = colsample_bytree 
nrounds = nrounds 
lambda = lambda 
alpha = alpha 

seqMaxDepth = seq(from = 5, to = 20, by = 5) # the sequence of max_depth to try
seqMinChildWeight = seq(from = 1, to = 9, by = 2) # the sequence of min_child_weight to try

permuPar_Depth_Child = expand.grid(seqMaxDepth, seqMinChildWeight) 
# data frame of all the permutations of the two parameters

n.permuPar_Depth_Child = nrow(permuPar_Depth_Child)
ErrorMetrics_Depth_Child = matrix(NA, nrow = 3, ncol = n.permuPar_Depth_Child)
rownames(ErrorMetrics_Depth_Child) = c("error", "auc", "logloss")
# the matrix to record test errors: "error", "auc", and "logloss"

for (i in 1:n.permuPar_Depth_Child){
  parameters = list(objective = "binary:logistic", eta = eta, 
                    max_depth = permuPar_Depth_Child[i, 1], 
                    min_child_weight = permuPar_Depth_Child[i, 2], 
                    gamma = gamma, subsample = subsample, colsample_bytree = colsample_bytree, 
                    lambda = lambda, alpha = alpha, num_parallel_tree = num_parallel_tree)
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                           nfold = nfold, metrics = list("error", "auc", "logloss"), 
                           nrounds = nrounds)
  ErrorMetrics_Depth_Child[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_Depth_Child[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_Depth_Child[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_Depth_Child = permuPar_Depth_Child[which(ErrorMetrics_Depth_Child["error", ] 
                                                      == min(ErrorMetrics_Depth_Child["error", ])), ]
optimalAUC_Depth_Child = permuPar_Depth_Child[which(ErrorMetrics_Depth_Child["auc", ] 
                                                      == max(ErrorMetrics_Depth_Child["auc", ])), ]
optimalLogloss_Depth_Child = permuPar_Depth_Child[which(ErrorMetrics_Depth_Child["logloss", ] 
                                                    == min(ErrorMetrics_Depth_Child["logloss", ])), ]
# remark: 1) permuPar_Depth_Child contains the combinations of the two parameters in the rows 
#         2) ErrorMetrics_Depth_Child contains the error metrics for the combinations in the columns
#         3) optimalError_Depth_Child, optimalAUC_Depth_Child, and optimalLogloss_Depth_Child contain 
#            the optimal max_depth and min_child_weight based on their respective metrics

##3: Use the optimal num_parallel_tree, max_depth, and min_child_weight, 
##   and fix the other parameters as before, to tune the parameter gamma.
num_parallel_tree = num_parallel_tree

##Choose one and comment out the other two.
max_depth = optimalError_Depth_Child[1, 1] # based on error
max_depth = optimalAUC_Depth_Child[1, 1] # based on auc 
max_depth = optimalLogloss_Depth_Child[1, 1] # based on logloss

##Choose one and comment out the other two.
min_child_weight = optimalError_Depth_Child[1, 2] # based on error
min_child_weight = optimalAUC_Depth_Child[1, 2] # based on auc
min_child_weight = optimalLogloss_Depth_Child[1, 2] # based on logloss

nfold = nfold 
eta = eta 
# gamma = gamma 
subsample = subsample 
colsample_bytree = colsample_bytree 
nrounds = nrounds 
lambda = lambda 
alpha = alpha

seqGamma = seq(from = 0.1, to = 1, by = 0.1)
n.seqGamma = length(seqGamma)
ErrorMetrics_Gamma = matrix(NA, nrow = 3, ncol = n.seqGamma)
rownames(ErrorMetrics_Gamma) = c("error", "auc", "logloss")
colnames(ErrorMetrics_Gamma) = seqGamma

for (i in 1:n.seqGamma){
  parameters = list(objective = "binary:logistic", eta = eta, max_depth = max_depth, 
                    min_child_weight = min_child_weight, gamma = seqGamma[i], subsample = 
                      subsample, colsample_bytree = colsample_bytree, lambda = lambda, 
                    alpha = alpha, num_parallel_tree = num_parallel_tree)
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = nrounds)
  ErrorMetrics_Gamma[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_Gamma[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_Gamma[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_Gamma = seqGamma[which(ErrorMetrics_Gamma["error", ] == min(ErrorMetrics_Gamma["error", ]))]
optimalAUC_Gamma = seqGamma[which(ErrorMetrics_Gamma["auc", ] == max(ErrorMetrics_Gamma["auc", ]))]
optimalLogloss_Gamma = seqGamma[which(ErrorMetrics_Gamma["logloss", ] == min(ErrorMetrics_Gamma["logloss", ]))]
# remark: 1) ErrorMetrics_Gamma contains the error metrics (row) for each parameter value (column)
#         2) optimalError_Gamma, optimalAUC_Gamma, and optimalLogloss_Gamma contain the optimal num_parallel_tree
#            based on their respective error metrics


##4: Use the optimal num_parallel_tree, max_depth, min_child_weight, and gamma, 
##   and fix the other parameters as before, to tune the parameters subsample and colsample_bytree.
num_parallel_tree = num_parallel_tree
max_depth = max_depth
min_child_weight = min_child_weight

##Choose one and comment out the other two.
gamma = max(optimalError_Gamma) # based on error
gamma = max(optimalAUC_Gamma) # based on auc
gamma = max(optimalLogloss_Gamma) # based on logloss

nfold = nfold 
eta = eta  
# subsample = subsample 
# colsample_bytree = colsample_bytree 
nrounds = nrounds 
lambda = lambda 
alpha = alpha

seqSubsample = seq(from = 5, to = 20, by = 5) # the sequence of subsample to try
seqColsample = seq(from = 1, to = 9, by = 2) # the sequence of colsample_bytree to try

permuPar_Sub_Col = expand.grid(seqSubsample, seqColsample) 
# data frame of all the permutations of the two parameters

n.permuPar_Sub_Col = nrow(permuPar_Sub_Col)
ErrorMetrics_Sub_Col = matrix(NA, nrow = 3, ncol = n.permuPar_Sub_Col)
rownames(ErrorMetrics_Sub_Col) = c("error", "auc", "logloss")
# the matrix to record test errors: "error", "auc", and "logloss"

for (i in 1:n.permuPar_Sub_Col){
  parameters = list(objective = "binary:logistic", eta = eta, 
                    max_depth = max_depth, 
                    min_child_weight = min_child_weight, 
                    gamma = gamma, subsample = permuPar_Sub_Col[i, 1], colsample_bytree = permuPar_Sub_Col[i, 2], 
                    lambda = lambda, alpha = alpha, num_parallel_tree = num_parallel_tree)
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = nrounds)
  ErrorMetrics_Sub_Col[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_Sub_Col[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_Sub_Col[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_Sub_Col = permuPar_Sub_Col[which(ErrorMetrics_Sub_Col["error", ] 
                                                      == min(ErrorMetrics_Sub_Col["error", ])), ]
optimalAUC_Sub_Col = permuPar_Sub_Col[which(ErrorMetrics_Sub_Col["auc", ] 
                                                    == max(ErrorMetrics_Sub_Col["auc", ])), ]
optimalLogloss_Sub_Col = permuPar_Sub_Col[which(ErrorMetrics_Sub_Col["logloss", ] 
                                                        == min(ErrorMetrics_Sub_Col["logloss", ])), ]
# remark: 1) permuPar_Sub_Col contains the combinations of the two parameters in the rows 
#         2) ErrorMetrics_Sub_Col contains the error metrics for the combinations in the columns
#         3) optimalError_Sub_Col, optimalAUC_Sub_Col, and optimalLogloss_Sub_Col contain 
#            the optimal subsample and colsample_bytree based on their respective metrics


##5: Use the optimal num_parallel_tree, max_depth, min_child_weight, gamma, subsample, and colsample_bytree, 
##   and fix the other parameters as before, to tune the parameters almbda and alpha.
num_parallel_tree = num_parallel_tree
max_depth = max_depth
min_child_weight = min_child_weight
gamma = gamma
nfold = nfold 
eta = eta 
nrounds = nrounds 

##Choose one and comment out the other two.
subsample = optimalError_Sub_Col[1, 1] # based on error
subsample = optimalAUC_Sub_Col[1, 1] # based on auc 
subsample = optimalLogloss_Sub_Col[1, 1] # based on logloss

##Choose one and comment out the other two.
colsample_bytree = optimalError_Sub_Col[1, 2] # based on error
colsample_bytree = optimalError_Sub_Col[1, 2] # based on auc
colsample_bytree = optimalError_Sub_Col[1, 2] # based on logloss

seqLambda = seq(from = 5, to = 20, by = 5) # the sequence of lambda to try
seqAlpha = seq(from = 1, to = 9, by = 2) # the sequence of alpha to try

permuPar_Lambda_Alpha = expand.grid(seqLambda, seqAlpha) 
# data frame of all the permutations of the two parameters

n.permuPar_Lambda_Alpha = nrow(permuPar_Lambda_Alpha)
ErrorMetrics_Lambda_Alpha = matrix(NA, nrow = 3, ncol = n.permuPar_Lambda_Alpha)
rownames(ErrorMetrics_Lambda_Alpha) = c("error", "auc", "logloss")
# the matrix to record test errors: "error", "auc", and "logloss"

for (i in 1:n.permuPar_Lambda_Alpha){
  parameters = list(objective = "binary:logistic", eta = eta, 
                    max_depth = max_depth, 
                    min_child_weight = min_child_weight, 
                    gamma = gamma, subsample = subsample, colsample_bytree = colsample_bytree, 
                    lambda = permuPar_Lambda_Alpha[i, 1], alpha = permuPar_Lambda_Alpha[i, 2], 
                    num_parallel_tree = num_parallel_tree)
  
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = nrounds)
  
  ErrorMetrics_Lambda_Alpha[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_Lambda_Alpha[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_Lambda_Alpha[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_Lambda_Alpha = permuPar_Lambda_Alpha[which(ErrorMetrics_Lambda_Alpha["error", ] 
                                              == min(ErrorMetrics_Lambda_Alpha["error", ])), ]
optimalAUC_Lambda_Alpha = permuPar_Lambda_Alpha[which(ErrorMetrics_Lambda_Alpha["auc", ] 
                                            == max(ErrorMetrics_Lambda_Alpha["auc", ])), ]
optimalLogloss_Lambda_Alpha = permuPar_Lambda_Alpha[which(ErrorMetrics_Lambda_Alpha["logloss", ] 
                                                == min(ErrorMetrics_Lambda_Alpha["logloss", ])), ]
# remark: 1) permuPar_Lambda_Alpha contains the combinations of the two parameters in the rows 
#         2) ErrorMetrics_Lambda_Alpha contains the error metrics for the combinations in the columns
#         3) optimalError_Lambda_Alpha, optimalAUC_Lambda_Alpha, and optimalLogloss_Lambda_Alpha contain 
#            the optimal lambda and alpha based on their respective metrics

##6: Use the optimal num_parallel_tree, max_depth, min_child_weight, gamma, subsample, colsample_bytree, 
##   lambda, and alpha, 
##   and fix the other parameters as before, to tune the parameter eta.
num_parallel_tree = num_parallel_tree
max_depth = max_depth
min_child_weight = min_child_weight
gamma = gamma
subsample = subsample
colsample_bytree = colsample_bytree

##Choose one and comment out the other two.
lambda = optimalError_Lambda_Alpha[1, 1] # based on error
lambda = optimalAUC_Lambda_Alpha[1, 1] # based on auc 
lambda = optimalLogloss_Lambda_Alpha[1, 1] # based on logloss

##Choose one and comment out the other two.
alpha = optimalError_Lambda_Alpha[1, 2] # based on error
alpha = optimalAUC_Lambda_Alpha[1, 2] # based on auc 
alpha = optimalLogloss_Lambda_Alpha[1, 2] # based on logloss

nfold = nfold 
eta = eta 
nrounds = nrounds 

seqEta = seq(from = 0.1, to = 1, by = 0.1)
n.seqEta = length(seqEta)
ErrorMetrics_Eta = matrix(NA, nrow = 3, ncol = n.seqEta)
rownames(ErrorMetrics_Eta) = c("error", "auc", "logloss")
colnames(ErrorMetrics_Eta) = seqEta

for (i in 1:n.seqEta){
  parameters = list(objective = "binary:logistic", eta = seqEta[i], max_depth = max_depth, 
                    min_child_weight = min_child_weight, gamma = gamma, subsample = 
                      subsample, colsample_bytree = colsample_bytree, lambda = lambda, 
                    alpha = alpha, num_parallel_tree = num_parallel_tree)
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = nrounds)
  ErrorMetrics_Eta[1, i] = as.matrix(boostCV)[nrounds, "test.error.mean"]
  ErrorMetrics_Eta[2, i] = as.matrix(boostCV)[nrounds, "test.auc.mean"]
  ErrorMetrics_Eta[3, i] = as.matrix(boostCV)[nrounds, "test.logloss.mean"]
}

optimalError_Eta = seqEta[which(ErrorMetrics_Eta["error", ] == min(ErrorMetrics_Eta["error", ]))]
optimalAUC_Eta = seqEta[which(ErrorMetrics_Eta["auc", ] == max(ErrorMetrics_Eta["auc", ]))]
optimalLogloss_Eta = seqEta[which(ErrorMetrics_Eta["logloss", ] == min(ErrorMetrics_Eta["logloss", ]))]
# remark: 1) ErrorMetrics_Eta contains the error metrics (row) for each parameter value (column)
#         2) optimalError_Eta, optimalAUC_Eta, and optimalLogloss_Eta contain the optimal num_parallel_tree
#            based on their respective error metrics


##6: Use the optimal num_parallel_tree, max_depth, min_child_weight, gamma, subsample, colsample_bytree, 
##   lambda, alpha, and eta, 
##   to tune the parameter nrounds.
num_parallel_tree = num_parallel_tree
max_depth = max_depth
min_child_weight = min_child_weight
gamma = gamma
subsample = subsample
colsample_bytree = colsample_bytree
lambda = lambda
alpha = alpha
nfold = nfold 

##Choose one and comment out the other two.
eta = max(optimalError_Eta) # based on error
eta = max(optimalAUC_Eta) # based on auc
eta = max(optimalLogloss_Eta) # based on logloss

seqRound = seq(from = 1, to = 5, by = 1)
n.seqRound = length(seqRound)
ErrorMetrics_Round = matrix(NA, nrow = 3, ncol = n.seqRound)
rownames(ErrorMetrics_Round) = c("error", "auc", "logloss")
colnames(ErrorMetrics_Round) = seqRound

for (i in 1:n.seqRound){
  parameters = list(objective = "binary:logistic", eta = eta, max_depth = max_depth, 
                    min_child_weight = min_child_weight, gamma = gamma, subsample = 
                      subsample, colsample_bytree = colsample_bytree, lambda = lambda, 
                    alpha = alpha, num_parallel_tree = num_parallel_tree)
  
  boostCV = xgb.cv(params = parameters, data = train_X, label = train_y, 
                   nfold = nfold, metrics = list("error", "auc", "logloss"), 
                   nrounds = seqRound[i])
  
  ErrorMetrics_Round[1, i] = as.matrix(boostCV)[seqRound[i], "test.error.mean"]
  ErrorMetrics_Round[2, i] = as.matrix(boostCV)[seqRound[i], "test.auc.mean"]
  ErrorMetrics_Round[3, i] = as.matrix(boostCV)[seqRound[i], "test.logloss.mean"]
}

optimalError_Round = seqRound[which(ErrorMetrics_Round["error", ] == min(ErrorMetrics_Round["error", ]))]
optimalAUC_Round = seqRound[which(ErrorMetrics_Round["auc", ] == max(ErrorMetrics_Round["auc", ]))]
optimalLogloss_Round = seqRound[which(ErrorMetrics_Round["logloss", ] == min(ErrorMetrics_Round["logloss", ]))]
# remark: 1) ErrorMetrics_Round contains the error metrics (row) for each parameter value (column)
#         2) optimalError_Round, optimalAUC_Round, and optimalLogloss_Round contain the optimal num_parallel_tree
#            based on their respective error metrics


##Choose one and comment out the other two.
nrounds = min(optimalError_Round) # based on error
nrounds = min(optimalAUC_Round) # based on auc
nrounds = min(optimalLogloss_Round) # based on logloss


##7: Train the boosting model based on the fine-tuned parameters.
num_parallel_tree = num_parallel_tree
max_depth = max_depth
min_child_weight = min_child_weight
gamma = gamma
subsample = subsample
colsample_bytree = colsample_bytree
lambda = lambda
alpha = alpha
eta = eta
nrounds = nrounds

xgboostPar = matrix(c(num_parallel_tree, max_depth, min_child_weight, gamma, subsample, 
                      colsample_bytree, lambda, alpha, eta, nrounds), 
                    ncol = 10)
colnames(xgboostPar) = c("num_parallel_tree", "max_depth", "min_child_weight", "gamma", "subsample", 
                         "colsample_bytree", "lambda", "alpha", "eta", "nrounds")
save(xgboostPar, file = "xgboostPar.RData")

parameters = list(objective = "binary:logistic", eta = eta, 
                  max_depth = max_depth, min_child_weight = min_child_weight, 
                  gamma = gamma, subsample = subsample, 
                  colsample_bytree = colsample_bytree, 
                  num_parallel_tree = num_parallel_tree, lambda = lambda, alpha = alpha)

trainBoostModel = xgboost(data = train_X, label = train_y, params = parameters, 
                     nrounds = nrounds)
save(trainBoostModel, file = "trainBoostModel.rda")

