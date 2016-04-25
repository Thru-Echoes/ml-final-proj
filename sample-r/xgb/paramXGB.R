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
                "booster" = "gbtree")
    allResults <- vector()

    # Random parameter assignment
    param$max.depth <- sample(3:7, 1, replace = T)
    param$eta <- runif(1, 0.01, 0.6)
    param$gamma <- runif(1, 0.0, 100)
    param$min_child_weight <- runif(1, 0.0, 100)
    num.folds <- 5

    # Cross validation
    cv.nround = 1000
    bst <- xgb.cv(param = param, data = x[trind, ], nfold = num.folds, label = y, nrounds = cv.nround, verbose = 0)

    # Find min mean error and respective index
    minMean <- min(bst[, test.error.mean])
    idxMinMean <- which.min(bst[, test.error.mean])

    # Write out results
    cvParam <- list("max.depth" = param$max.depth,
                  "eta" = param$eta,
                  "gamma" = param$gamma,
                  "min_child_weight" = param$min_child_weight,
                  "minMean" = minMean,
                  "idxMinMean" = idxMinMean)

    allResults <- c(allResults, cvParam)
    return(allResults)
}
