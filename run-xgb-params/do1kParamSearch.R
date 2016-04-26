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
    param$subsample <- runif(1, 0, 1)
    param$colsample_bytree <- runif(1, 0, 1)
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

params500_tree1 <- lapply(1:500, function(i) {
	print("Tree1 status: ")
	print(i)
	paramXGB()
})

params500_tree2 <- lapply(1:500, function(i) {
	print("Tree2 status: ")
	print(i)
	paramXGB()
})

treeError <- vector()
treeAUC <- vector()
treeLogloss <- vector()

error1 <- lapply(1:500, function(i) {
	params500_tree1[[i]]$minError
})
auc1 <- lapply(1:500, function(i) {
  params500_tree1[[i]]$maxAUC
})
logloss1 <- lapply(1:500, function(i) {
  params500_tree1[[i]]$minLogloss
})

treeError <- c(treeError, list(params500_tree1[[which.min(error1)]]))
treeAUC <- c(treeAUC, list(params500_tree1[[which.min(auc1)]]))
treeLogloss <- c(treeLogloss, list(params500_tree1[[which.min(logloss1)]]))

error2 <- lapply(1:500, function(i) {
  params500_tree2[[i]]$minError
})
auc2 <- lapply(1:500, function(i) {
  params500_tree2[[i]]$maxAUC
})
logloss2 <- lapply(1:500, function(i) {
  params500_tree2[[i]]$minLogloss
})

treeError <- c(treeError, list(params500_tree2[[which.min(error2)]]))
treeAUC <- c(treeAUC, list(params500_tree2[[which.min(auc2)]]))
treeLogloss <- c(treeLogloss, list(params500_tree2[[which.min(logloss2)]]))

########################################
######### SAVE OUT PARAMETERS ##########
########################################

### YOU NEED TO EDIT THE NAMES OF THE FILES BELOW SO WE DONT OVERWRITE
### PREVIOUS ONES IF MULTIPLE PEOPLE ARE USING THIS SCRIPT!

save(treeError, file = "data/xgb-april25/topError_1k_runs_params.rda")
save(treeAUC, file = "data/xgb-april25/topAUC_1k_runs_params.rda")
save(treeLogloss, file ="data/xgb-april25/topLogloss_1k_runs_params.rda")
