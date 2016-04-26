# Code for XGBoost runs
# Dependecies: paramXGB.R

require(xgboost)

# Parameter searching
source("paramXGB-0425.R")

# data = y, 101, 102, 103, ...
#target1 <- read.csv(file.choose(), header = TRUE, check.names = FALSE)
#target0 <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# omit NAs
#t1 <- na.omit(target1)
#newRows <- nrow(t1[1])
#t0 <- target0[sample(nrow(target0), newRows), ]

# make dataframe
#allDf <- rbind(t1, t0)

# shuffle
#df <- allDf[sample(nrow(allDf)), ]

# 70 / 30 split
#indices = sample(1:nrow(df), nrow(df) * .3)
#train = df[-indices, ]
#test = df[indices, ]

# write out
#write.csv(test, "data/xgb-april25/xgb_trial1_30test.csv")
#write.csv(train, "data/xgb-april25/xgb_trial1_70train.csv")
#y <- train[, 1]

load("data/xgb-april25/train80_55features_april25.RData")
load("data/xgb-april25/test20_55features_april25.RData")

trainNow <- train_55features 
testNow <- test_55features

xAllLabel <- rbind(trainNow[, -1], testNow[, -1])

yTrain <- trainNow[, 1]
yTest <- testNow[, 1]

yAllLabel <- c(yTrain, yTest)

# Need: 
#train <- NULL
#test <- NULL
#y <- NULL 

########################################
########## CREATE FILE / RM Y ##########
########################################
#x = rbind(train[, 2:56], test[, 2:56])
#x = as.matrix(x)
#x = matrix(as.numeric(x), nrow(x), ncol(x))
#trainIndx = 1:length(y)
#testIndx = (nrow(train) + 1):nrow(x)

########################################
########## INITIAL RUN OF XGB ##########
########################################

#x = matrix(rnorm(500, mean = 0, sd = 1), nrow = 100, ncol = 5)
#y = sample(0:1, size = 100, replace = T)

trainIndx = sample(1:1000000, size = 800000, replace = F)

x <- as.matrix(xSample)
y <- ySample

xMili <- x[1:1000000, ]
yMili <- y[1:1000000]

x <- xMili
y <- yMili 

paramsInit_tree <- paramXGB_init()

errorInit <- paramsInit_tree$minError
aucInit <- paramsInit_tree$maxAUC
loglossInit <- paramsInit_tree$minLogloss

param_init <- list("objective" = "binary:logistic",
              "max.depth" = 4,
              "eta" = 1,
              "nthread" = 8,
              "booster" = "gbtree")

nround = 75
bst_init <- xgboost(param = param_init, data = x[trainIndx, ], label = y[trainIndx], nrounds = nround, verbose = 2)

# Make prediction
pred_init = predict(bst_init, xMili[-trainIndx, ])
numCorrect_init = sum(pred_init == yMili[-trainIndx])

#pred = matrix(pred, 1, length(pred) / 1)
#pred = t(pred)

# first 10 lines of model
model_init <- xgb.dump(bst_init, with.stats = T)
model_init[1:10]

# each line = branch

# feature importance = averaging gain of each feature for all splits and trees

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix_init <- xgb.importance(names, model = bst_init)
# get top 10 most important features
xgb.plot.importance(importance_matrix_init[1:20, ])

# tree graph
xgb.plot.tree(feature_names = names, model = bst_init, n_first_tree = 2)


########################################
############ PARAM SEARCH ##############
########################################

# Linear boosting XGB model
#params10k_tree <- lapply(1:10000, function(i) {
#  print("Linear status: ")
#  print(i)
#  paramLin()
#})

# Tree boosting XGB model
params10k_tree <- lapply(1:10000, function(i) {
  print("Tree status: ")
  print(i)
  paramXGB()
})

########################################
######### FIND OPTIMAL PARAMS ##########
########################################

# allMeans <- lapply(1:10000, function(i) {
# 		params10k_tree[[i]]$minMean
# })
# 
# # display params for lowest mean error
# params10k_tree[[which.min(allMeans)]]


########################################
####### ALT: FIND OPTIMAL PARAMS #######
########################################

set.seed(1)
#trainIndx = sample(1:nrow(xAllLabel), size = floor(nrow(xAllLabel) * 0.25), replace = F)

#x <- as.matrix(xAllLabel)
#y <- yAllLabel

trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
x <- as.matrix(trainNow[, -1])
y <- trainNow[, 1]

params1k_tree1 <- lapply(1:5, function(i) {
	print("Tree1 status: ")
	print(i)
	paramXGB()
})

params1k_tree2 <- lapply(1:1000, function(i) {
	print("Tree2 status: ")
	print(i)
	paramXGB()
})

params1k_tree3 <- lapply(1:1000, function(i) {
  print("Tree3 status: ")
  print(i)
  paramXGB()
})

params1k_tree4 <- lapply(1:1000, function(i) {
  print("Tree4 status: ")
  print(i)
  paramXGB()
})

params1k_tree5 <- lapply(1:1000, function(i) {
  print("Tree5 status: ")
  print(i)
  paramXGB()
})

params1k_tree6 <- lapply(1:1000, function(i) {
  print("Tree6 status: ")
  print(i)
  paramXGB()
})

params1k_tree7 <- lapply(1:1000, function(i) {
  print("Tree7 status: ")
  print(i)
  paramXGB()
})

params1k_tree8 <- lapply(1:1000, function(i) {
  print("Tree8 status: ")
  print(i)
  paramXGB()
})

params1k_tree9 <- lapply(1:1000, function(i) {
  print("Tree9 status: ")
  print(i)
  paramXGB()
})

params1k_tree10 <- lapply(1:1000, function(i) {
  print("Tree10 status: ")
  print(i)
  paramXGB()
})
# ... up to _tree10

treeError <- vector()
treeAUC <- vector()
treeLogloss <- vector()

error1 <- lapply(1:5, function(i) {
	params1k_tree1[[i]]$minError
})
auc1 <- lapply(1:5, function(i) {
  params1k_tree1[[i]]$maxAUC
})
logloss1 <- lapply(1:5, function(i) {
  params1k_tree1[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree1[[which.min(error1)]]))
treeAUC <- c(treeAUC, list(params1k_tree1[[which.min(auc1)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree1[[which.min(logloss1)]]))


error2 <- lapply(1:5, function(i) {
  params1k_tree2[[i]]$minError
})
auc2 <- lapply(1:5, function(i) {
  params1k_tree2[[i]]$maxAUC
})
logloss2 <- lapply(1:5, function(i) {
  params1k_tree2[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree2[[which.min(error2)]]))
treeAUC <- c(treeAUC, list(params1k_tree2[[which.min(auc2)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree2[[which.min(logloss2)]]))


error3 <- lapply(1:1000, function(i) {
  params1k_tree3[[i]]$minError
})
auc3 <- lapply(1:1000, function(i) {
  params1k_tree3[[i]]$maxAUC
})
logloss3 <- lapply(1:1000, function(i) {
  params1k_tree3[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree3[[which.min(error3)]]))
treeAUC <- c(treeAUC, list(params1k_tree3[[which.min(auc3)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree3[[which.min(logloss3)]]))


error4 <- lapply(1:1000, function(i) {
  params1k_tree4[[i]]$minError
})
auc4 <- lapply(1:1000, function(i) {
  params1k_tree4[[i]]$maxAUC
})
logloss4 <- lapply(1:1000, function(i) {
  params1k_tree4[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree4[[which.min(error4)]]))
treeAUC <- c(treeAUC, list(params1k_tree4[[which.min(auc4)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree4[[which.min(logloss4)]]))

error5 <- lapply(1:1000, function(i) {
  params1k_tree5[[i]]$minError
})
auc5 <- lapply(1:1000, function(i) {
  params1k_tree5[[i]]$maxAUC
})
logloss5 <- lapply(1:1000, function(i) {
  params1k_tree5[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree5[[which.min(error5)]]))
treeAUC <- c(treeAUC, list(params1k_tree5[[which.min(auc5)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree5[[which.min(logloss5)]]))


error6 <- lapply(1:1000, function(i) {
  params1k_tree6[[i]]$minError
})
auc6 <- lapply(1:1000, function(i) {
  params1k_tree6[[i]]$maxAUC
})
logloss6 <- lapply(1:1000, function(i) {
  params1k_tree6[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree6[[which.min(error6)]]))
treeAUC <- c(treeAUC, list(params1k_tree6[[which.min(auc6)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree6[[which.min(logloss6)]]))


error7 <- lapply(1:1000, function(i) {
  params1k_tree7[[i]]$minError
})
auc7 <- lapply(1:1000, function(i) {
  params1k_tree7[[i]]$maxAUC
})
logloss7 <- lapply(1:1000, function(i) {
  params1k_tree7[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree7[[which.min(error7)]]))
treeAUC <- c(treeAUC, list(params1k_tree7[[which.min(auc7)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree7[[which.min(logloss7)]]))


error8 <- lapply(1:1000, function(i) {
  params1k_tree8[[i]]$minError
})
auc8 <- lapply(1:1000, function(i) {
  params1k_tree8[[i]]$maxAUC
})
logloss8 <- lapply(1:1000, function(i) {
  params1k_tree8[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree8[[which.min(error8)]]))
treeAUC <- c(treeAUC, list(params1k_tree8[[which.min(auc8)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree8[[which.min(logloss8)]]))


error9 <- lapply(1:1000, function(i) {
  params1k_tree9[[i]]$minError
})
auc9 <- lapply(1:1000, function(i) {
  params1k_tree9[[i]]$maxAUC
})
logloss9 <- lapply(1:1000, function(i) {
  params1k_tree9[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree9[[which.min(error9)]]))
treeAUC <- c(treeAUC, list(params1k_tree9[[which.min(auc9)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree9[[which.min(logloss9)]]))

error10 <- lapply(1:1000, function(i) {
  params1k_tree10[[i]]$minError
})
auc10 <- lapply(1:1000, function(i) {
  params1k_tree10[[i]]$maxAUC
})
logloss10 <- lapply(1:1000, function(i) {
  params1k_tree10[[i]]$minLogloss
})

treeError <- c(treeError, list(params1k_tree10[[which.min(error10)]]))
treeAUC <- c(treeAUC, list(params1k_tree10[[which.min(auc10)]]))
treeLogloss <- c(treeLogloss, list(params1k_tree10[[which.min(logloss10)]]))
# ... up to means10

#write.csv(treeError, "topError_treeBoost_params.csv")
#write.csv(treeAUC, "topAUC_treeBoost_params.csv")
#write.csv(treeLogloss, "topLogloss_treeBoost_params.csv")

save(treeError, file = "data/xgb-april25/topError_treeBoost_params.rda")
save(treeAUC, file = "data/xgb-april25/topAUC_treeBoost_params.rda")
save(treeLogloss, file ="data/xgb-april25/topLogloss_treeBoost_params.rda")

########################################
######## AFTER OPTIMAL PARAMS ##########
########################################

# standard run
nround = 100
bst <- xgboost(param = param, data = x[trainIndx, ], label = y, nrounds = nround, verbose = 2)

# Make prediction
pred = predict(bst, x[testIndx,])
pred = matrix(pred, 1, length(pred) / 1)
pred = t(pred)

# Output submission
pred = format(pred, digits = 2,scientific = F) # shrink the size of submission
write.csv(pred, file = 'xgb_trial1_nonBoosted_output.csv', quote = FALSE,row.names = FALSE)

########################################
########### CHECK OUT DATA! ############
########################################

# bst == model from XGB
#  -> model of 100 trees
#  		-> each tree built via recursive division of dataset

# first 10 lines of model
model <- xgb.dump(bst, with.stats = T)
model[1:10]

# each line = branch

# feature importance = averaging gain of each feature for all splits and trees

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix <- xgb.importance(names, model = bst)
# get top 10 most important features
xgb.plot.importance(importance_matrix[1:10, ])

# tree graph
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)

################ END ##################
