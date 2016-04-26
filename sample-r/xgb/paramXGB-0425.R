# Parameter searching for runXGB.R
#
# Generate random param set for tree boosted

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
    cv.nround = 500
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

paramXGB_init <- function() {
  
  param <- list("objective" = "binary:logistic",
                "max.depth" = 4,
                "eta" = 1,
                "nthread" = 8,
                "booster" = "gbtree")
  allResults <- vector()
  num.folds <- 5
  
  # Cross validation
  bst <- xgb.cv(param = param, data = x[trainIndx, ], nfold = num.folds, label = y[trainIndx], nrounds = 100, verbose = 2, 
                metrics = list("error", "auc", "logloss"))
  
  # Find min mean error and respective index
  minError <- min(bst[, test.error.mean])
  idxminError <- which.min(bst[, test.error.mean])
  
  # Find max mean auc and respective index
  maxAUC <- max(bst[, test.auc.mean])
  idxmaxAUC <- which.min(bst[, test.auc.mean])
  
  # Find min logloss and respective index
  minLogloss <- min(bst[, test.logloss.mean])
  idxminLogloss <- which.min(bst[, test.logloss.mean])
  
  # Write out results
  cvParam <- list("max.depth" = param$max.depth,
                  "eta" = param$eta,
                  "minError" = minError,
                  "idxminError" = idxminError, 
                  "maxAUC" = maxAUC, 
                  "idxmaxAUC" = idxmaxAUC, 
                  "minLogloss" = minLogloss, 
                  "idxminLogloss" = idxminLogloss)
  
  allResults <- c(allResults, cvParam)
  return(allResults)
}
