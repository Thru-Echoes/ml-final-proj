# Few sets of optimal parameters for XGB

# AUC accounts for error of classification
#   of both positive and negative responses
#
# Min Error of test is misclassification error
#   it only really accounts for ability of
#       positive response


##### Do 169feature runs and get variable importance 
#####   and then do 164feature runs and get var importance 

# Use only 50% of the data (should not need huge dataset for so many tweets)!

set.seed(1)
trainIndx169.50 <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.5), replace = F)
trainIndx169.25 <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.25), replace = F)
x169 <- final_169features
y <- yAllLabel


trainIndx164.50 <- sample(1:nrow(final_164features.df), size = floor(nrow(final_164features.df) * 0.5), replace = F)
trainIndx164.25 <- sample(1:nrow(final_164features.df), size = floor(nrow(final_164features.df) * 0.25), replace = F)
#x <- final_164features.df[trainIndx164.25, ]
#y <- yAllLabel[trainIndx164.25]
x164 <- final_164features.df 

######### Create some fixed param set 

nrounds.default = 1000
param1 <- list("objective" = "binary:logistic",
                   "max.depth" = 6,
                   "eta" = 0.05,
                   "gamma" = 0,
                   "min_child_weight" = 1,
                   "booster" = "gbtree",
                   "subsample" = 0.5,
                   "colsample_bytree" = 0.5,
                   "lambda" = 1,
                   "alpha" = 0)

param2 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.1,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

param3 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

num.folds = 5 

#########
######### Do some CROSS VALIDATION! 
#########


# Cross validation
cv.nround = 20
bst169_1 <- xgb.cv(param = param1, data = as.matrix(x169[trainIndx169.25, ]), label = y[trainIndx169.25], nfold = num.folds, nrounds = cv.nround, verbose = 1,
                   metrics = list("error", "auc", "logloss"))

bst169_1_100 <- xgb.cv(param = param1, data = as.matrix(x169[trainIndx169.25, ]), label = y[trainIndx169.25], nfold = num.folds, nrounds = 100, verbose = 1,
                   metrics = list("error", "auc", "logloss"))

# Find min mean error and respective index
minError169_1 <- min(bst169_1[, test.error.mean])
idxminError169_1 <- which.min(bst169_1[, test.error.mean])

# Find max mean auc and respective index
maxAUC169_1 <- max(bst169_1[, test.auc.mean])
idxmaxAUC169_1 <- which.max(bst169_1[, test.auc.mean])

# Find min logloss and respective index
minLogloss169_1 <- min(bst169_1[, test.logloss.mean])
idxminLogloss169_1 <- which.min(bst169_1[, test.logloss.mean])

###################
################### 
###################

#### Larger eta - 0.1 

bst169_2_100 <- xgb.cv(param = param2, data = as.matrix(x169[trainIndx169.25, ]), label = y[trainIndx169.25], nfold = num.folds, nrounds = 100, verbose = 1,
                       metrics = list("error", "auc", "logloss"))

#### Even Larger eta - 0.5

bst169_3_100 <- xgb.cv(param = param3, data = as.matrix(x169[trainIndx169.25, ]), label = y[trainIndx169.25], nfold = num.folds, nrounds = 100, verbose = 1,
                       metrics = list("error", "auc", "logloss"))


###################
################### Now do this again for param2 and param3 
###################

#bst164_1 <- xgb.cv(param = param1, data = as.matrix(x164[trainIndx164.25, ]), label = y[trainIndx164.25], nfold = num.folds, nrounds = cv.nround, verbose = 1,
 #                  metrics = list("error", "auc", "logloss"))

bst164_1_100 <- xgb.cv(param = param1, data = as.matrix(x164[trainIndx164.25, ]), label = y[trainIndx164.25], nfold = num.folds, nrounds = 100, verbose = 1,
                   metrics = list("error", "auc", "logloss"))

# Find min mean error and respective index
minError164_1 <- min(bst164_1[, test.error.mean])
idxminError164_1 <- which.min(bst164_1[, test.error.mean])

# Find max mean auc and respective index
maxAUC164_1 <- max(bst164_1[, test.auc.mean])
idxmaxAUC164_1 <- which.max(bst164_1[, test.auc.mean])

# Find min logloss and respective index
minLogloss164_1 <- min(bst164_1[, test.logloss.mean])
idxminLogloss164_1 <- which.min(bst164_1[, test.logloss.mean])

###################
################### Now do this again for param2 and param3 
###################



# Cross validation
cv.nround = 1000

bst169_2 <- xgb.cv(param = param2, data = as.matrix(x169[trainIndx169.50, ]), label = y[trainIndx169.50], nfold = num.folds, nrounds = cv.nround, verbose = 0,
                   metrics = list("error", "auc", "logloss"))

bst169_3 <- xgb.cv(param = param3, data = as.matrix(x169[trainIndx169.50, ]), label = y[trainIndx169.50], nfold = num.folds, nrounds = cv.nround, verbose = 0,
                   metrics = list("error", "auc", "logloss"))

# Find min mean error and respective index
minError169_2 <- min(bst169_2[, test.error.mean])
idxminError169_2 <- which.min(bst169_2[, test.error.mean])

# Find max mean auc and respective index
maxAUC169_2 <- max(bst169_2[, test.auc.mean])
idxmaxAUC169_2 <- which.max(bst169_2[, test.auc.mean])

# Find min logloss and respective index
minLogloss169_2 <- min(bst169_2[, test.logloss.mean])
idxminLogloss169_2 <- which.min(bst169_2[, test.logloss.mean])

# Find min mean error and respective index
minError169_3 <- min(bst169_3[, test.error.mean])
idxminError169_3 <- which.min(bst169_3[, test.error.mean])

# Find max mean auc and respective index
maxAUC169_3 <- max(bst169_3[, test.auc.mean])
idxmaxAUC169_3 <- which.max(bst169_3[, test.auc.mean])

# Find min logloss and respective index
minLogloss169_3 <- min(bst169_3[, test.logloss.mean])
idxminLogloss169_3 <- which.min(bst169_3[, test.logloss.mean])


################### AND NOW DO THE SAME FOR 164features 

bst164_2 <- xgb.cv(param = param2, data = as.matrix(x164[trainIndx164.50, ]), label = y[trainIndx164.50], nfold = num.folds, nrounds = cv.nround, verbose = 0,
                   metrics = list("error", "auc", "logloss"))

bst164_3 <- xgb.cv(param = param3, data = as.matrix(x164[trainIndx164.50, ]), label = y[trainIndx164.50], nfold = num.folds, nrounds = cv.nround, verbose = 0,
                   metrics = list("error", "auc", "logloss"))

# Find min mean error and respective index
minError164_2 <- min(bst164_2[, test.error.mean])
idxminError164_2 <- which.min(bst164_2[, test.error.mean])

# Find max mean auc and respective index
maxAUC164_2 <- max(bst164_2[, test.auc.mean])
idxmaxAUC164_2 <- which.max(bst164_2[, test.auc.mean])

# Find min logloss and respective index
minLogloss164_2 <- min(bst164_2[, test.logloss.mean])
idxminLogloss164_2 <- which.min(bst164_2[, test.logloss.mean])

# Find min mean error and respective index
minError164_3 <- min(bst164_3[, test.error.mean])
idxminError164_3 <- which.min(bst164_3[, test.error.mean])

# Find max mean auc and respective index
maxAUC164_3 <- max(bst164_3[, test.auc.mean])
idxmaxAUC164_3 <- which.max(bst164_3[, test.auc.mean])

# Find min logloss and respective index
minLogloss164_3 <- min(bst164_3[, test.logloss.mean])
idxminLogloss164_3 <- which.min(bst164_3[, test.logloss.mean])


#########
######### Do 169feature run 
#########


model169_1 <- xgboost(param = param1, data = as.matrix(x169[trainIndx169.50, ]), label = y[trainIndx169.50], nrounds = nrounds.default, verbose = 2)

xgb.save(model169_1, "run-svm-xgb/April27_169features_xgb_model1")

# Make prediction
pred169_1 = predict(model169_1, as.matrix(x169[-trainIndx169.50, ]))

# first 10 lines of model
model169_1.seeModel <- xgb.dump(model169_1, with.stats = T)
model169_1.seeModel[1:10]

names <- dimnames(x169[trainIndx169.50, ])[[2]]
varImp169_1 <- xgb.importance(names, model = model169_1)
# get top 10 most important features
xgb.plot.importance(varImp169_1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = model169_1, n_first_tree = 2)


######### Do 164feature run 


model164_1 <- xgboost(param = param1, data = as.matrix(x164[trainIndx164.50, ]), label = y[trainIndx164.50], nrounds = nrounds.default, verbose = 2)

xgb.save(model164_1, "run-svm-xgb/April27_164features_xgb_model1")

# Make prediction
pred164_1 = predict(model164_1, as.matrix(x164[-trainIndx164.50, ]))

# first 10 lines of model
model164_1.seeModel <- xgb.dump(model164_1, with.stats = T)
model164_1.seeModel[1:10]

names <- dimnames(x164[trainIndx164.50, ])[[2]]
varImp164_1 <- xgb.importance(names, model = model164_1)
# get top 10 most important features
xgb.plot.importance(varImp164_1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = model164_1, n_first_tree = 2)






###### OLD CODE - APRIL26 - KEPT AS REFERENCE ########


########################################
############# PULL IN DATA #############
########################################
load("data/train80_55features.RData")
load("data/test20_55features.RData")

trainNow <- train_55features
testNow <- test_55features

xAllLabel <- rbind(trainNow[, -1], testNow[, -1])

yTrain <- trainNow[, 1]
yTest <- testNow[, 1]

yAllLabel <- c(yTrain, yTest)

allLabelFeatures.user.hash <- xAllLabel[, 52:55]

tfIdf.labeled <- tf_idf[50001:1578627, ]

final_169features <- cbind(tfIdf.labeled, allLabelFeatures.user.hash)
save(final_169features, file = "April26_xgbTrainAllLabel_169features.rda")

set.seed(1)
trainIndx <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.75), replace = F)
x <- final_169features
y <- yAllLabel




###### After 250 random parameter runs - try again with only 164 tf-idf features: 
#tfIdf.labeled <- tf_idf[50001:1578627, ]
final_164features <- tfIdf.labeled 
final_164features.df <- data.frame(final_164features)
final_164features.matrix <- as.matrix(final_164features)
save(final_164features.df, file = "run-svm-xgb/April27_xgbTrainAllLabel_164features.rda")

set.seed(1)
trainIndx164.25 <- sample(1:nrow(final_164features.df), size = floor(nrow(final_164features.df) * 0.25), replace = F)
trainIndx164.75 <- sample(1:nrow(final_164features.df), size = floor(nrow(final_164features.df) * 0.75), replace = F)



trainIndx164.25 <- sample(1:nrow(x), size = floor(nrow(x) * 0.75), replace = F)

#########################################################

#load("data-mod-log/testMatrix.RData")
#Scores_finalTest <- X.test

#final_169features <- cbind(tf_idf, Scores_finalTest)
#save(final_169features, file = "submission/April26_finalDF_169features.rda")

########################################
######## CREATE OPTIMAL PARAMS #########
########################################

# Index 5 from all1kAUC
nrounds.auc1 = 336
optimalAUC1 <- list("objective" = "binary:logistic",
        "max.depth" = 3,
        "eta" = 0.243441,
        "gamma" = 47.66837,
        "min_child_weight" = 30.33176,
        "booster" = "gbtree",
        "subsample" = 0.1893606,
        "colsample_bytree" = 0.2377403,
        "lambda" = 45.61725,
        "alpha" = 60.20635)

# Index 12 from all1kAUC
nrounds.auc2 = 341
optimalAUC2 <- list("objective" = "binary:logistic",
        "max.depth" = 3,
        "eta" = 0.1426307,
        "gamma" = 79.35435,
        "min_child_weight" = 36.28353,
        "booster" = "gbtree",
        "subsample" = 0.1063356,
        "colsample_bytree" = 0.5430775,
        "lambda" = 96.38863,
        "alpha" = 31.25427)

# Index 11 from all1kAUC
nrounds.auc3 = 105
optimalAUC3 <- list("objective" = "binary:logistic",
        "max.depth" = 7,
        "eta" = 0.3012009,
        "gamma" = 34.22025,
        "min_child_weight" = 56.63385,
        "booster" = "gbtree",
        "subsample" = 0.1044733,
        "colsample_bytree" = 0.8550553,
        "lambda" = 14.66577,
        "alpha" = 50.7063)

nrounds.default = 100
fixedParam <- list("objective" = "binary:logistic",
        "max.depth" = 6,
        "eta" = 0.3,
        "gamma" = 0,
        "min_child_weight" = 1,
        "booster" = "gbtree",
        "subsample" = 0.5,
        "colsample_bytree" = 0.5,
        "lambda" = 1,
        "alpha" = 0)

########################################
#### RUN MODELS WITH DEFAULT PARAMS ####
########################################
# Standard run
#### -> need to set nround
bst.default <- xgboost(param = fixedParam, data = as.matrix(x[trainIndx164.75, ]), label = y[trainIndx164.75], nrounds = nrounds.default, verbose = 2)

xgb.save(bst.default, "April26_169features_xgb.default.model")

# Make prediction
pred.default = predict(bst.default, as.matrix(x[-trainIndx164.75, ]))

# first 10 lines of model
model.default <- xgb.dump(bst.default, with.stats = T)
model.default[1:10]

names <- dimnames(x[trainIndx164.75, ])[[2]]
importance_matrix.default <- xgb.importance(names, model = bst.default)
# get top 10 most important features
xgb.plot.importance(importance_matrix.default[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc1 <- xgboost(param = optimalAUC1, data =  as.matrix(x[trainIndx164.75, ]), label = y[trainIndx164.75], nrounds = nrounds.auc1, verbose = 2)

xgb.save(bst.auc1, "April26_169features_xgb.bestAUC1.model")

# Make prediction
pred.auc1 = predict(bst.auc1, as.matrix(x[-trainIndx164.75, ]))

# first 10 lines of model
model.auc1 <- xgb.dump(bst.auc1, with.stats = T)
#model1.default[1:10]

names <- dimnames(x[trainIndx164.75, ])[[2]]
importance_matrix.auc1 <- xgb.importance(names, model = bst.auc1)
# get top 10 most important features
#xgb.plot.importance(importance_matrix.auc1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc2 <- xgboost(param = optimalAUC2, data = as.matrix(x[trainIndx164.75, ]), label = y[trainIndx164.75], nrounds = nrounds.auc2, verbose = 2)

xgb.save(bst.auc2, "April26_169features_xgb.bestAUC2.model")

# Make prediction
pred.auc2 = predict(bst.auc2, as.matrix(x[-trainIndx164.75, ]))

# first 10 lines of model
model.auc2 <- xgb.dump(bst.auc2, with.stats = T)
#model1.default[1:10]

names <- dimnames(x[trainIndx164.75, ])[[2]]
importance_matrix.auc2 <- xgb.importance(names, model = bst.auc2)
# get top 10 most important features
#xgb.plot.importance(importance_matrix.auc2[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc3 <- xgboost(param = optimalAUC3, data = as.matrix(x[trainIndx164.75, ]), label = y[trainIndx164.75], nrounds = nrounds.auc3, verbose = 2)

xgb.save(bst.auc3, "April26_169features_xgb.bestAUC3.model")

# Make prediction
pred.auc3 = predict(bst.auc3, as.matrix(x[-trainIndx164.75, ]))

# first 10 lines of model
model.auc3 <- xgb.dump(bst.auc3, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(x[trainIndx164.75, ])[[2]]
importance_matrix.auc3 <- xgb.importance(names, model = bst.auc3)
# get top 10 most important features
xgb.plot.importance(importance_matrix.auc3[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)

########################################


########################################
nrounds.default = 1000
fixedParam2 <- list("objective" = "binary:logistic",
                   "max.depth" = 6,
                   "eta" = 0.05,
                   "gamma" = 0,
                   "min_child_weight" = 10,
                   "booster" = "gbtree",
                   "subsample" = 0.5,
                   "colsample_bytree" = 0.5,
                   "lambda" = 1,
                   "alpha" = 0)

#### XGB 

#trial164xgb <- xgboost(param = optimalAUC1, data =  as.matrix(x[trainIndx164.75, ]), label = y[trainIndx164.75], nrounds = nrounds.auc1, verbose = 2)


########################################
## Find some optimal parameters with 
## all 169 features 
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
  cv.nround = 500
  bst <- xgb.cv(param = param, data = as.matrix(x[trainIndx164.25, ]), nfold = num.folds, label = y[trainIndx164.25], nrounds = cv.nround, verbose = 0,
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



#set.seed(555)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.25), replace = F)
#trainIndx <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.25), replace = F)

### SET ANOTHER SEED HERE SO THAT RANDOM NUMBERS FOR PARAMETERS ARE DIFFERENT!
#OLIVER_SEED = 9876
#PAUL_SEED = 1234
#CHRIS_SEED = 1738

#set.seed(OLIVER_SEED)

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

params10_164tree1 <- lapply(1:10, function(i) {
  print("Tree1 status: ")
  print(i)
  paramXGB()
})

################

treeError164 <- vector()
treeAUC164 <- vector()
treeLogloss164 <- vector()

error1 <- lapply(1:10, function(i) {
  params10_164tree1[[i]]$minError
})
auc1 <- lapply(1:10, function(i) {
  params10_164tree1[[i]]$maxAUC
})
logloss1 <- lapply(1:10, function(i) {
  params10_164tree1[[i]]$minLogloss
})

treeError164 <- c(treeError164, list(params10_164tree1[[which.min(error1)]]))
treeAUC164 <- c(treeAUC164, list(params10_164tree1[[which.min(auc1)]]))
treeLogloss164 <- c(treeLogloss164, list(params10_164tree1[[which.min(logloss1)]]))

save(treeError164, file = "run-svm-xgb/April27_164features_topError_10runs.rda")
save(treeAUC164, file = "run-svm-xgb/April27_164features_topAUC_10runs.rda")
save(treeLogloss164, file ="run-svm-xgb/April27_164features_topLogloss_10runs.rda")

#################

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

########################################
######### SAVE OUT PARAMETERS ##########
########################################

### YOU NEED TO EDIT THE NAMES OF THE FILES BELOW SO WE DONT OVERWRITE
### PREVIOUS ONES IF MULTIPLE PEOPLE ARE USING THIS SCRIPT!

save(treeError, file = "April26_169features_topError_250runs.rda")
save(treeAUC, file = "April26_169features_topAUC_250runs.rda")
save(treeLogloss, file ="April26_169features_topLogloss_250runs.rda")


########################################
###### RERUN WITH SPARSE MATRICES ######
########################################

trainSparse <- Matrix(data = as.matrix(trainNow[, -1]), sparse = TRUE)
testSparse <- Matrix(data = as.matrix(testNow[, -1]), sparse = TRUE)

trainY <- trainNow[, 1]
testY <- testNow[, 1]

########################################

bst.sparse.default <- xgboost(param = fixedParam, data = trainSparse, label = trainY, nrounds = nrounds.default, verbose = 2)

xgb.save(bst.sparse.default, "run-xgb-params/xgb.best_sparse_default.model")

# Make prediction
pred.sparse.default = predict(bst.sparse.default, testSparse)

# first 10 lines of model
model.sparse.default <- xgb.dump(bst.sparse.default, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(trainSparse)[[2]]
importance_matrix.sparse.default <- xgb.importance(names, model = bst.sparse.default)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.default[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)

#### #### ####

bst.sparse.auc1 <- xgboost(param = optimalAUC1, data = trainSparse, label = trainY, nrounds = nrounds.auc1, verbose = 2)

xgb.save(bst.sparse.auc1, "run-xgb-params/xgb.best_sparse_AUC1.model")

# Make prediction
pred.sparse.auc1 = predict(bst.sparse.auc1, testSparse)

# first 10 lines of model
model.sparse.auc1 <- xgb.dump(bst.sparse.auc1, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(trainSparse)[[2]]
importance_matrix.sparse.auc1 <- xgb.importance(names, model = bst.sparse.auc1)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.auc1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)

#### #### ####

bst.sparse.auc2 <- xgboost(param = optimalAUC2, data = trainSparse, label = trainY, nrounds = nrounds.auc2, verbose = 2)

xgb.save(bst.sparse.auc2, "run-xgb-params/xgb.best_sparse_AUC2.model")

# Make prediction
pred.sparse.auc2 = predict(bst.sparse.auc2, testSparse)

# first 10 lines of model
model.sparse.auc2 <- xgb.dump(bst.sparse.auc2, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(trainSparse)[[2]]
importance_matrix.sparse.auc2 <- xgb.importance(names, model = bst.sparse.auc2)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.auc2[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)

#### #### ####

bst.sparse.auc3 <- xgboost(param = optimalAUC3, data = trainSparse, label = trainY, nrounds = nrounds.auc3, verbose = 2)

xgb.save(bst.sparse.auc3, "run-xgb-params/xgb.best_sparse_AUC3.model")

# Make prediction
pred.sparse.auc3 = predict(bst.sparse.auc3, testSparse)

# first 10 lines of model
model.sparse.auc3 <- xgb.dump(bst.sparse.auc3, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(trainSparse)[[2]]
importance_matrix.sparse.auc3 <- xgb.importance(names, model = bst.sparse.auc3)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.auc3[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)

########################################

err.sparse.default <- mean(as.numeric(pred.sparse.default > 0.5) != testY)
print(paste("test-error (sparse.default) = ", err.sparse.default))

err.sparse.auc1 <- mean(as.numeric(pred.sparse.auc1 > 0.5) != testY)
print(paste("test-error (sparse.auc1) = ", err.sparse.auc1))

err.sparse.auc2 <- mean(as.numeric(pred.sparse.auc2 > 0.5) != testY)
print(paste("test-error (sparse.auc2) = ", err.sparse.auc2))

err.sparse.auc3 <- mean(as.numeric(pred.sparse.auc3 > 0.5) != testY)
print(paste("test-error (sparse.auc3) = ", err.sparse.auc3))

library(ROCR)

head(raw.data$SentimentText, 20)
raw.data$SentimentText <- as.character(raw.data$SentimentText)

paul.class.test <- function(real, predicted) {
  # Assesses the accuracy of a model's predictions
  ct <- table(real, predicted)
  # [[1]] Percent correct for each category and [[2]] Total percent correct
  return(list(diag(prop.table(ct, 1)), sum(diag(prop.table(ct)))))
}

default.pred <- as.numeric(pred.sparse.default > 0.505)
auc1.pred <- as.numeric(pred.sparse.auc1 > 0.5)
auc2.pred <- as.numeric(pred.sparse.auc2 > 0.5)
auc3.pred <- as.numeric(pred.sparse.auc3 > 0.5)

paul.class.test(testY, default.pred)
paul.class.test(testY, auc1.pred)
paul.class.test(testY, auc2.pred)
paul.class.test(testY, auc3.pred)

performance(prediction(default.pred, testY), measure="auc")@y.values[[1]]
performance(prediction(auc1.pred, testY), measure="auc")@y.values[[1]]
performance(prediction(auc2.pred, testY), measure="auc")@y.values[[1]]
performance(prediction(auc3.pred, testY), measure="auc")@y.values[[1]]

load("submission/April25_finalDF_55features.rda")

sub.test.sparse <- Matrix(data=as.matrix(final_55features[,-1]), sparse=TRUE)

pred.sparse.default.submission = as.numeric(predict(bst.sparse.default, newdata=sub.test.sparse) > 0.5)

submission = data.frame(id = 1:length(pred.sparse.default.submission), y = pred.sparse.default.submission)
write.csv(submission, file = "submission/Submission2xgb_COP.csv", row.names = FALSE)


neg.tweets <- raw.data$SentimentText[which(raw.data$Sentiment==0)]
class(neg.tweets)
any(grepl(x=neg.tweets, pattern="best"))

########################################

err.sparse.default <- mean(as.numeric(pred.sparse.default > 0.3) != testY)
print(paste("test-error (sparse.default) = ", err.sparse.default))

err.sparse.auc1 <- mean(as.numeric(pred.sparse.auc1 > 0.3) != testY)
print(paste("test-error (sparse.auc1) = ", err.sparse.auc1))

err.sparse.auc2 <- mean(as.numeric(pred.sparse.auc2 > 0.3) != testY)
print(paste("test-error (sparse.auc2) = ", err.sparse.auc2))

err.sparse.auc3 <- mean(as.numeric(pred.sparse.auc3 > 0.3) != testY)
print(paste("test-error (sparse.auc3) = ", err.sparse.auc3))

########################################

err.sparse.default <- mean(as.numeric(pred.sparse.default > 0.7) != testY)
print(paste("test-error (sparse.default) = ", err.sparse.default))

err.sparse.auc1 <- mean(as.numeric(pred.sparse.auc1 > 0.7) != testY)
print(paste("test-error (sparse.auc1) = ", err.sparse.auc1))

err.sparse.auc2 <- mean(as.numeric(pred.sparse.auc2 > 0.7) != testY)
print(paste("test-error (sparse.auc2) = ", err.sparse.auc2))

err.sparse.auc3 <- mean(as.numeric(pred.sparse.auc3 > 0.7) != testY)
print(paste("test-error (sparse.auc3) = ", err.sparse.auc3))

############################################
# CHECK INDICES WHERE ALWAYS RIGHT / WRONG #
############################################
wrong.negIndx <- which(((compare.all.models$yHat.default + compare.all.models$yHat.auc1 + compare.all.models$yHat.auc2 + compare.all.models$yHat.auc3) == 4) & (compare.all.models$yActual == 0))

length(wrong.negIndx)

wrong.posIndx <- which(((compare.all.models$yHat.default + compare.all.models$yHat.auc1 + compare.all.models$yHat.auc2 + compare.all.models$yHat.auc3) == 0) & (compare.all.models$yActual == 1))

length(wrong.posIndx)
############## END #####################

head(compare.all.models)
