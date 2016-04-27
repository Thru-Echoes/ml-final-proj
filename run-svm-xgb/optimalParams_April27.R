########################################
## Find some optimal parameters XGBoost
## with all 169 features
########################################

paramXGB <- function() {

  #     param <- list("objective" = "binary:logistic",
  #                 "max.depth" = 6,
  #                 "eta" = 0.1,
  #                 "gamma" = 0,
  #                 "min_child_weight" = 1,
  #                 "nthread" = 8,
  #                 "booster" = "gbtree",
  #                 "subsample" = 1,
  #                 "colsample_bytree" = 1,
  #                 "lambda" = 1,
  #                 "alpha" = 0)
  #     allResults <- vector()
  #
  #     # Random parameter assignment
  #     param$max.depth <- sample(3:9, 1, replace = T)
  #     param$eta <- runif(1, 0.01, 0.6)
  #     param$gamma <- runif(1, 0.0, 100)
  #     param$min_child_weight <- runif(1, 0.0, 100)
  #     param$subsample <- runif(1, 0.1, 1)
  #     param$colsample_bytree <- runif(1, 0.1, 1)
  #     param$lambda <- runif(1, 0.0, 100)
  #     param$alpha <- runif(1, 0.0, 100)
  #     num.folds <- 5

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
  param$eta <- runif(1, 0.01, 0.8)
  param$gamma <- runif(1, 10, 80)
  param$min_child_weight <- runif(1, 25, 75)
  param$subsample <- runif(1, 0.1, 1)
  param$colsample_bytree <- runif(1, 0.1, 1)
  param$lambda <- runif(1, 0.0, 100)
  param$alpha <- runif(1, 0.0, 100)
  num.folds <- 5

  # Cross validation
  cv.nround = 1000
  bst <- xgb.cv(param = param, data = as.matrix(x[trainIndx, ]), nfold = num.folds, label = y[trainIndx], nrounds = cv.nround, verbose = 0,
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
                  "subsample" = param$subsample,
                  "colsample_bytree" = param$colsample_bytree,
                  "lambda" = param$lambda,
                  "alpha" = param$alpha,
                  "gamma" = param$gamma,
                  "minError" = minError,
                  "idxminError" = idxminError,
                  "maxAUC" = maxAUC,
                  "idxmaxAUC" = idxmaxAUC,
                  "minLogloss" = minLogloss,
                  "idxminLogloss" = idxminLogloss)

  allResults <- c(allResults, cvParam)
  return(allResults)
}

set.seed(555)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
trainIndx <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.25), replace = F)

### SET ANOTHER SEED HERE SO THAT RANDOM NUMBERS FOR PARAMETERS ARE DIFFERENT!
OLIVER_SEED = 9876
#PAUL_SEED = 1234
#CHRIS_SEED = 1738

set.seed(OLIVER_SEED)

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

######### SAVE OUT PARAMETERS ##########

### YOU NEED TO EDIT THE NAMES OF THE FILES BELOW SO WE DONT OVERWRITE
### PREVIOUS ONES IF MULTIPLE PEOPLE ARE USING THIS SCRIPT!

save(treeError, file = "run-svm-xgb/April27_169features_topError_250runs.rda")
save(treeAUC, file = "run-svm-xgb/April27_169features_topAUC_250runs.rda")
save(treeLogloss, file ="run-svm-xgb/April27_169features_topLogloss_250runs.rda")

########################################
## Find some optimal parameters XGBoost
## with all 164 features
########################################
paramXGB_164 <- function() {

  #     param <- list("objective" = "binary:logistic",
  #                 "max.depth" = 6,
  #                 "eta" = 0.1,
  #                 "gamma" = 0,
  #                 "min_child_weight" = 1,
  #                 "nthread" = 8,
  #                 "booster" = "gbtree",
  #                 "subsample" = 1,
  #                 "colsample_bytree" = 1,
  #                 "lambda" = 1,
  #                 "alpha" = 0)
  #     allResults <- vector()
  #
  #     # Random parameter assignment
  #     param$max.depth <- sample(3:9, 1, replace = T)
  #     param$eta <- runif(1, 0.01, 0.6)
  #     param$gamma <- runif(1, 0.0, 100)
  #     param$min_child_weight <- runif(1, 0.0, 100)
  #     param$subsample <- runif(1, 0.1, 1)
  #     param$colsample_bytree <- runif(1, 0.1, 1)
  #     param$lambda <- runif(1, 0.0, 100)
  #     param$alpha <- runif(1, 0.0, 100)
  #     num.folds <- 5

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
  param$eta <- runif(1, 0.01, 0.8)
  param$gamma <- runif(1, 10, 80)
  param$min_child_weight <- runif(1, 25, 75)
  param$subsample <- runif(1, 0.1, 1)
  param$colsample_bytree <- runif(1, 0.1, 1)
  param$lambda <- runif(1, 0.0, 100)
  param$alpha <- runif(1, 0.0, 100)
  num.folds <- 5

  # Cross validation
  cv.nround = 1000
  bst <- xgb.cv(param = param, data = as.matrix(x[trainIndx164, ]), nfold = num.folds, label = y[trainIndx164], nrounds = cv.nround, verbose = 0,
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
                  "subsample" = param$subsample,
                  "colsample_bytree" = param$colsample_bytree,
                  "lambda" = param$lambda,
                  "alpha" = param$alpha,
                  "gamma" = param$gamma,
                  "minError" = minError,
                  "idxminError" = idxminError,
                  "maxAUC" = maxAUC,
                  "idxmaxAUC" = idxmaxAUC,
                  "minLogloss" = minLogloss,
                  "idxminLogloss" = idxminLogloss)

  allResults <- c(allResults, cvParam)
  return(allResults)
}

set.seed(555)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
trainIndx164 <- sample(1:nrow(final_164features), size = floor(nrow(final_164features) * 0.25), replace = F)

### SET ANOTHER SEED HERE SO THAT RANDOM NUMBERS FOR PARAMETERS ARE DIFFERENT!
OLIVER_SEED = 9876
#PAUL_SEED = 1234
#CHRIS_SEED = 1738

set.seed(OLIVER_SEED)

params50_164tree1 <- lapply(1:50, function(i) {
  print("Tree1 status: ")
  print(i)
  paramXGB_164()
})

params50_164tree2 <- lapply(1:50, function(i) {
  print("Tree2 status: ")
  print(i)
  paramXGB_164()
})

params50_164tree3 <- lapply(1:50, function(i) {
  print("Tree3 status: ")
  print(i)
  paramXGB_164()
})

params50_164tree4 <- lapply(1:50, function(i) {
  print("Tree4 status: ")
  print(i)
  paramXGB_164()
})

params50_164tree5 <- lapply(1:50, function(i) {
  print("Tree5 status: ")
  print(i)
  paramXGB_164()
})

treeError164 <- vector()
treeAUC164 <- vector()
treeLogloss164 <- vector()

error164_1 <- lapply(1:50, function(i) {
  params50_164tree1[[i]]$minError
})
auc164_1 <- lapply(1:50, function(i) {
  params50_164tree1[[i]]$maxAUC
})
logloss164_1 <- lapply(1:50, function(i) {
  params50_164tree1[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params50_164tree1[[which.min(error164_1)]]))
treeAUC164 <- c(treeAUC164, list(params50_164tree1[[which.min(auc164_1)]]))
treeLogloss164 <- c(treeLogloss164, list(params50_164tree1[[which.min(logloss164_1)]]))

error164_2 <- lapply(1:50, function(i) {
  params50_164tree2[[i]]$minError
})
auc164_2 <- lapply(1:50, function(i) {
  params50_164tree2[[i]]$maxAUC
})
logloss164_2 <- lapply(1:50, function(i) {
  params50_164tree2[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params50_164tree2[[which.min(error164_2)]]))
treeAUC164 <- c(treeAUC164, list(params50_164tree2[[which.min(auc164_2)]]))
treeLogloss164 <- c(treeLogloss164, list(params50_164tree2[[which.min(logloss164_2)]]))

error164_3 <- lapply(1:50, function(i) {
  params50_164tree3[[i]]$minError
})
auc164_3 <- lapply(1:50, function(i) {
  params50_164tree3[[i]]$maxAUC
})
logloss164_3 <- lapply(1:50, function(i) {
  params50_164tree3[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params50_164tree3[[which.min(error164_3)]]))
treeAUC164 <- c(treeAUC164, list(params50_164tree3[[which.min(auc164_3)]]))
treeLogloss164 <- c(treeLogloss164, list(params50_164tree3[[which.min(logloss164_3)]]))

error164_4 <- lapply(1:50, function(i) {
  params50_164tree4[[i]]$minError
})
auc164_4 <- lapply(1:50, function(i) {
  params50_164tree4[[i]]$maxAUC
})
logloss164_4 <- lapply(1:50, function(i) {
  params50_164tree4[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params50_164tree4[[which.min(error164_4)]]))
treeAUC164 <- c(treeAUC164, list(params50_164tree4[[which.min(auc164_4)]]))
treeLogloss164 <- c(treeLogloss164, list(params50_164tree4[[which.min(logloss164_4)]]))

error164_5 <- lapply(1:50, function(i) {
  params50_164tree5[[i]]$minError
})
auc164_5 <- lapply(1:50, function(i) {
  params50_164tree5[[i]]$maxAUC
})
logloss164_5 <- lapply(1:50, function(i) {
  params50_164tree5[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params50_164tree5[[which.min(error164_5)]]))
treeAUC164 <- c(treeAUC164, list(params50_164tree5[[which.min(auc164_5)]]))
treeLogloss164 <- c(treeLogloss164, list(params50_164tree5[[which.min(logloss164_5)]]))

######### SAVE OUT PARAMETERS ##########

### YOU NEED TO EDIT THE NAMES OF THE FILES BELOW SO WE DONT OVERWRITE
### PREVIOUS ONES IF MULTIPLE PEOPLE ARE USING THIS SCRIPT!

save(treeError164, file = "run-svm-xgb/April27_164features_topError_250runs.rda")
save(treeAUC164, file = "run-svm-xgb/April27_164features_topAUC_250runs.rda")
save(treeLogloss164, file ="run-svm-xgb/April27_164features_topLogloss_250runs.rda")

########################################
## Find some optimal parameters SVM
## with all 169 features
########################################


########################################
## Find some optimal parameters SVM
## with all 164 features
########################################
