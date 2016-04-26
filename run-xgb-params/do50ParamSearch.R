require(xgboost)

########################################
######## PARAM SEARCH FUNCTION #########
########################################

# This is the parameter search function
paramXGB <- function() {

    param <- list("objective" = "binary:logistic",
                "max.depth" = 6,
                "eta" = 0.1,
                "gamma" = 0,
                "min_child_weight" = 1,
                "nthread" = 8,
                "booster" = "gbtree",
                "subsample" = 1,
                "colsample_bytree" = 1,
                "lambda" = 1,
                "alpha" = 0)
    allResults <- vector()

    # Random parameter assignment
    param$max.depth <- sample(3:9, 1, replace = T)
    param$eta <- runif(1, 0.01, 0.6)
    param$gamma <- runif(1, 0.0, 100)
    param$min_child_weight <- runif(1, 0.0, 100)
    param$subsample <- runif(1, 0.1, 1)
    param$colsample_bytree <- runif(1, 0.1, 1)
    param$lambda <- runif(1, 0.0, 100)
    param$alpha <- runif(1, 0.0, 100)
    num.folds <- 5

    # Cross validation
    cv.nround = 350
    bst <- xgb.cv(param = param, data = x[trainIndx, ], nfold = num.folds, label = y[trainIndx], nrounds = cv.nround, verbose = 0,
                  metrics = list("error", "auc", "logloss"))

    # Find min mean error and respective index
    minError <- min(bst[, test.error.mean])
    idxminError <- which.min(bst[, test.error.mean])

    # Find max mean auc and respective index
    maxAUC <- max(bst[, test.auc.mean])
    idxmaxAUC <- which.max(bst[, test.auc.mean])

    # Find min logloss and respective index
    minLogloss <- min(bst[, test.logloss.mean])
    idxminLogloss <- which.min(bst[, test.logloss.mean])

    # Write out results
    cvParam <- list("max.depth" = param$max.depth,
                  "eta" = param$eta,
                  "gamma" = param$gamma,
                  "min_child_weight" = param$min_child_weight,
                  "minError" = minError,
                  "idxminError" = idxminError,
                  "maxAUC" = maxAUC,
                  "idxmaxAUC" = idxmaxAUC,
                  "minLogloss" = minLogloss,
                  "idxminLogloss" = idxminLogloss)

    allResults <- c(allResults, cvParam)
    return(allResults)
}

########################################
####### LOAD ALL DATA / FEATURES #######
########################################

# Need to load in the train and test data (all features)
load("data/xgb-april25/train80_55features_april25.RData")
load("data/xgb-april25/test20_55features_april25.RData")

trainNow <- train_55features
testNow <- test_55features

xAllLabel <- rbind(trainNow[, -1], testNow[, -1])

yTrain <- trainNow[, 1]
yTest <- testNow[, 1]

yAllLabel <- c(yTrain, yTest)

########################################
#### XGB MODELS WITH RANDOM PARAMS #####
########################################

set.seed(1)
trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
x <- as.matrix(trainNow[, -1])
y <- trainNow[, 1]

### SET ANOTHER SEED HERE SO THAT RANDOM NUMBERS FOR PARAMETERS ARE DIFFERENT!
OLIVER_SEED = 9876
PAUL_SEED = 1234
CHRIS_SEED = 1738

set.seed(YOUR_SEED_HERE)

params50_tree1 <- lapply(1:50, function(i) {
	print("Tree1 status: ")
	print(i)
	paramXGB()
})

params50_tree2 <- lapply(1:50, function(i) {
	print("Tree2 status: ")
	print(i)
	paramXGB()
})

params50_tree3 <- lapply(1:50, function(i) {
  print("Tree3 status: ")
  print(i)
  paramXGB()
})

params50_tree4 <- lapply(1:50, function(i) {
  print("Tree4 status: ")
  print(i)
  paramXGB()
})

params50_tree5 <- lapply(1:50, function(i) {
  print("Tree5 status: ")
  print(i)
  paramXGB()
})

params50_tree6 <- lapply(1:50, function(i) {
  print("Tree6 status: ")
  print(i)
  paramXGB()
})


params50_tree7 <- lapply(1:50, function(i) {
  print("Tree7 status: ")
  print(i)
  paramXGB()
})

params50_tree8 <- lapply(1:50, function(i) {
  print("Tree8 status: ")
  print(i)
  paramXGB()
})

params50_tree9 <- lapply(1:50, function(i) {
  print("Tree9 status: ")
  print(i)
  paramXGB()
})

params50_tree10 <- lapply(1:50, function(i) {
  print("Tree10 status: ")
  print(i)
  paramXGB()
})

# params50_tree11 <- lapply(1:50, function(i) {
#   print("Tree11 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree12 <- lapply(1:50, function(i) {
#   print("Tree12 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree13 <- lapply(1:50, function(i) {
#   print("Tree13 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree14 <- lapply(1:50, function(i) {
#   print("Tree14 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree15 <- lapply(1:50, function(i) {
#   print("Tree15 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree16 <- lapply(1:50, function(i) {
#   print("Tree16 status: ")
#   print(i)
#   paramXGB()
# })
# 
# 
# params50_tree17 <- lapply(1:50, function(i) {
#   print("Tree17 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree18 <- lapply(1:50, function(i) {
#   print("Tree18 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree19 <- lapply(1:50, function(i) {
#   print("Tree19 status: ")
#   print(i)
#   paramXGB()
# })
# 
# params50_tree20 <- lapply(1:50, function(i) {
#   print("Tree20 status: ")
#   print(i)
#   paramXGB()
# })

treeError <- vector()
treeAUC <- vector()
treeLogloss <- vector()

error1 <- lapply(1:50, function(i) {
	params50_tree1[[i]]$minError
})
auc1 <- lapply(1:50, function(i) {
  params50_tree1[[i]]$maxAUC
})
logloss1 <- lapply(1:50, function(i) {
  params50_tree1[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree1[[which.min(error1)]]))
treeAUC <- c(treeAUC, list(params50_tree1[[which.min(auc1)]]))
treeLogloss <- c(treeLogloss, list(params50_tree1[[which.min(logloss1)]]))

error2 <- lapply(1:50, function(i) {
  params50_tree2[[i]]$minError
})
auc2 <- lapply(1:50, function(i) {
  params50_tree2[[i]]$maxAUC
})
logloss2 <- lapply(1:50, function(i) {
  params50_tree2[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree2[[which.min(error2)]]))
treeAUC <- c(treeAUC, list(params50_tree2[[which.min(auc2)]]))
treeLogloss <- c(treeLogloss, list(params50_tree2[[which.min(logloss2)]]))

error3 <- lapply(1:50, function(i) {
  params50_tree3[[i]]$minError
})
auc3 <- lapply(1:50, function(i) {
  params50_tree3[[i]]$maxAUC
})
logloss3 <- lapply(1:50, function(i) {
  params50_tree3[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree3[[which.min(error3)]]))
treeAUC <- c(treeAUC, list(params50_tree3[[which.min(auc3)]]))
treeLogloss <- c(treeLogloss, list(params50_tree3[[which.min(logloss3)]]))

error4 <- lapply(1:50, function(i) {
  params50_tree4[[i]]$minError
})
auc4 <- lapply(1:50, function(i) {
  params50_tree4[[i]]$maxAUC
})
logloss4 <- lapply(1:50, function(i) {
  params50_tree4[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree4[[which.min(error4)]]))
treeAUC <- c(treeAUC, list(params50_tree4[[which.min(auc4)]]))
treeLogloss <- c(treeLogloss, list(params50_tree4[[which.min(logloss4)]]))

error5 <- lapply(1:50, function(i) {
  params50_tree5[[i]]$minError
})
auc5 <- lapply(1:50, function(i) {
  params50_tree5[[i]]$maxAUC
})
logloss5 <- lapply(1:50, function(i) {
  params50_tree5[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree5[[which.min(error5)]]))
treeAUC <- c(treeAUC, list(params50_tree5[[which.min(auc5)]]))
treeLogloss <- c(treeLogloss, list(params50_tree5[[which.min(logloss5)]]))

error6 <- lapply(1:50, function(i) {
  params50_tree6[[i]]$minError
})
auc6 <- lapply(1:50, function(i) {
  params50_tree6[[i]]$maxAUC
})
logloss6 <- lapply(1:50, function(i) {
  params50_tree6[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree6[[which.min(error6)]]))
treeAUC <- c(treeAUC, list(params50_tree6[[which.min(auc6)]]))
treeLogloss <- c(treeLogloss, list(params50_tree6[[which.min(logloss6)]]))

error7 <- lapply(1:50, function(i) {
  params50_tree7[[i]]$minError
})
auc7 <- lapply(1:50, function(i) {
  params50_tree7[[i]]$maxAUC
})
logloss7 <- lapply(1:50, function(i) {
  params50_tree7[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree7[[which.min(error7)]]))
treeAUC <- c(treeAUC, list(params50_tree7[[which.min(auc7)]]))
treeLogloss <- c(treeLogloss, list(params50_tree7[[which.min(logloss7)]]))

error8 <- lapply(1:50, function(i) {
  params50_tree8[[i]]$minError
})
auc8 <- lapply(1:50, function(i) {
  params50_tree8[[i]]$maxAUC
})
logloss8 <- lapply(1:50, function(i) {
  params50_tree8[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree8[[which.min(error8)]]))
treeAUC <- c(treeAUC, list(params50_tree8[[which.min(auc8)]]))
treeLogloss <- c(treeLogloss, list(params50_tree8[[which.min(logloss8)]]))

error9 <- lapply(1:50, function(i) {
  params50_tree9[[i]]$minError
})
auc9 <- lapply(1:50, function(i) {
  params50_tree9[[i]]$maxAUC
})
logloss9 <- lapply(1:50, function(i) {
  params50_tree9[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree9[[which.min(error9)]]))
treeAUC <- c(treeAUC, list(params50_tree9[[which.min(auc9)]]))
treeLogloss <- c(treeLogloss, list(params50_tree9[[which.min(logloss9)]]))

error10 <- lapply(1:50, function(i) {
  params50_tree10[[i]]$minError
})
auc10 <- lapply(1:50, function(i) {
  params50_tree10[[i]]$maxAUC
})
logloss10 <- lapply(1:50, function(i) {
  params50_tree10[[i]]$minLogloss
})

treeError <- c(treeError, list(params50_tree10[[which.min(error10)]]))
treeAUC <- c(treeAUC, list(params50_tree10[[which.min(auc10)]]))
treeLogloss <- c(treeLogloss, list(params50_tree10[[which.min(logloss10)]]))

########################################
######### SAVE OUT PARAMETERS ##########
########################################

### YOU NEED TO EDIT THE NAMES OF THE FILES BELOW SO WE DONT OVERWRITE
### PREVIOUS ONES IF MULTIPLE PEOPLE ARE USING THIS SCRIPT!

save(treeError, file = "data/xgb-april25/OCM_topError_500_runs_params.rda")
save(treeAUC, file = "data/xgb-april25/OCM_topAUC_500_runs_params.rda")
save(treeLogloss, file ="data/xgb-april25/OCM_topLogloss_500_runs_params.rda")
