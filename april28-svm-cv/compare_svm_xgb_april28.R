##Perform linear SVM modeling and prediction.
load("data/deltaTF_IDF_AllFeatures.rda")
deltaTF_labeled <- deltaTF_IDF_AllFeatures[50001:1578627, ]
save(deltaTF_labeled, file = "april28-svm-cv/April28_deltaTF_labeled_290features.rda")
x290 <- deltaTF_labeled

load("data/deltaTF_IDF_ImpFeatures.rda")
x20 <- deltaTF_IDF_ImpFeatures[50001:1578627, ]
save(x20, file = "april28-svm-cv/April28_20features_important.rda")

load("april28-svm-cv/April28_20features_important.rda")

load("data/y_AllRaw.rda")
y50k.unlabeled <- y_AllRaw[1:50000]
yLabeled <- y_AllRaw[50001:1578627]

require(kernlab)
require(xgboost)

### Set 20% data for model runs - then split that into 75% train / 25% test
set.seed(1)
trainIndx290.20 <- sample(1:nrow(x290), size = floor(nrow(x290) * 0.2), replace = F)

trainIndx20.20perc <- sample(1:nrow(x20), size = floor(nrow(x20) * 0.2), replace = F)

trainIndx20.10perc <- sample(1:nrow(x20), size = floor(nrow(x20) * 0.1), replace = F)

trainIndx20.05perc <- sample(1:nrow(x20), size = floor(nrow(x20) * 0.05), replace = F)

x290.testing <- x290[trainIndx290.20, ]
y290.testing <- yLabeled[trainIndx290.20]

set.seed(2)
indx290.train <- sample(1:nrow(x290.testing), size = floor(nrow(x290.testing) * 0.75), replace = F)

x <- x290.testing
y <- y290.testing
xTrain = x
yTrain = y

xTest = x
yTest = y

trainIndx <- indx290.train

#### Save out April27 20% subset with 75% train, 25% test
save(xTrain, file = "april28-svm-cv/April28_75_xTrain_290features.rda")
save(xTest, file = "april28-svm-cv/April28_25_xTest_290features.rda")
save(yTrain, file = "april28-svm-cv/April28_75_yTrain_290features.rda")
save(yTest, file = "april28-svm-cv/April28_25_yTest_290features.rda")


################################################################

#### Try XGBoost

param1 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.3,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

param2 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.01,
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

param4 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 1,
               "colsample_bytree" = 1,
               "lambda" = 1,
               "alpha" = 0)

param5 <- list("objective" = "binary:logistic",
               "max.depth" = 6,
               "eta" = 0.5,
               "gamma" = 0,
               "min_child_weight" = 1,
               "booster" = "gbtree",
               "subsample" = 0.5,
               "colsample_bytree" = 0.5,
               "lambda" = 1,
               "alpha" = 0)

num.folds <- 5

# Cross validation
cv.nround = 5
bst.param1 <- xgb.cv(param = param1, data = xTrain[trainIndx, ], nfold = num.folds, label = yTrain[trainIndx], nrounds = cv.nround, verbose = 0,
                     metrics = list("error", "auc", "logloss"))

bst.param2 <- xgb.cv(param = param2, data = xTrain[trainIndx, ], nfold = num.folds, label = yTrain[trainIndx], nrounds = cv.nround, verbose = 0,
                     metrics = list("error", "auc", "logloss"))

bst.param3 <- xgb.cv(param = param3, data = xTrain[trainIndx, ], nfold = num.folds, label = yTrain[trainIndx], nrounds = cv.nround, verbose = 0,
                     metrics = list("error", "auc", "logloss"))

bst.param4 <- xgb.cv(param = param4, data = xTrain[trainIndx, ], nfold = num.folds, label = yTrain[trainIndx], nrounds = cv.nround, verbose = 0,
                     metrics = list("error", "auc", "logloss"))

bst.param5 <- xgb.cv(param = param5, data = xTrain[trainIndx, ], nfold = num.folds, label = yTrain[trainIndx], nrounds = 10, verbose = 1,
                     metrics = list("error", "auc", "logloss"))

# Find min mean error and respective index
minError.param1 <- min(bst.param1[, test.error.mean])
idxminError.param1 <- which.min(bst.param1[, test.error.mean])

# Find max mean auc and respective index
maxAUC.param1 <- max(bst.param1[, test.auc.mean])
idxmaxAUC.param1 <- which.max(bst.param1[, test.auc.mean])

# Find min logloss and respective index
minLogloss.param1 <- min(bst.param1[, test.logloss.mean])
idxminLogloss.param1 <- which.min(bst.param1[, test.logloss.mean])

##

# Find min mean error and respective index
minError.param2 <- min(bst.param2[, test.error.mean])
idxminError.param2 <- which.min(bst.param2[, test.error.mean])

# Find max mean auc and respective index
maxAUC.param2 <- max(bst.param2[, test.auc.mean])
idxmaxAUC.param2 <- which.max(bst.param2[, test.auc.mean])

# Find min logloss and respective index
minLogloss.param2 <- min(bst.param2[, test.logloss.mean])
idxminLogloss.param2 <- which.min(bst.param2[, test.logloss.mean])

##

# Find min mean error and respective index
minError.param3 <- min(bst.param3[, test.error.mean])
idxminError.param3 <- which.min(bst.param3[, test.error.mean])

# Find max mean auc and respective index
maxAUC.param3 <- max(bst.param3[, test.auc.mean])
idxmaxAUC.param3 <- which.max(bst.param3[, test.auc.mean])

# Find min logloss and respective index
minLogloss.param3 <- min(bst.param3[, test.logloss.mean])
idxminLogloss.param3 <- which.min(bst.param3[, test.logloss.mean])

##

# Find min mean error and respective index
minError.param4 <- min(bst.param4[, test.error.mean])
idxminError.param4 <- which.min(bst.param4[, test.error.mean])

# Find max mean auc and respective index
maxAUC.param4 <- max(bst.param4[, test.auc.mean])
idxmaxAUC.param4 <- which.max(bst.param4[, test.auc.mean])

# Find min logloss and respective index
minLogloss.param4 <- min(bst.param4[, test.logloss.mean])
idxminLogloss.param4 <- which.min(bst.param4[, test.logloss.mean])
####


####

bst.sparse.param1 <- xgboost(param = param1, data = xTrain[trainIndx, ], label = yTrain[trainIndx], nrounds = 1, verbose = 1)

xgb.save(bst.sparse.param1, "april28-svm-cv/April28_xgb_sparse_75_25_param1_290features.model")

# Make prediction
pred.sparse.param1 = predict(bst.sparse.param1, xTest[-trainIndx, ])

# first 10 lines of model
model.sparse.param1 <- xgb.dump(bst.sparse.param1, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(xTrain)[[2]]
importance_matrix.sparse.param1 <- xgb.importance(names, model = bst.sparse.param1)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.param1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)


err.sparse.param1 <- mean(as.numeric(pred.sparse.param1 > 0.5) != yTest[-trainIndx])
print(paste("test-error (sparse.default) = ", err.sparse.param1))

###

bst.sparse.param2 <- xgboost(param = param2, data = xTrain[trainIndx, ], label = yTrain[trainIndx], nrounds = 1, verbose = 1)

xgb.save(bst.sparse.param2, "april28-svm-cv/April28_xgb_sparse_75_25_param2_290features.model")

# Make prediction
pred.sparse.param2 = predict(bst.sparse.param2, xTest[-trainIndx, ])

# first 10 lines of model
model.sparse.param2 <- xgb.dump(bst.sparse.param2, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(xTrain)[[2]]
importance_matrix.sparse.param2 <- xgb.importance(names, model = bst.sparse.param2)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.param2[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)


err.sparse.param2 <- mean(as.numeric(pred.sparse.param2 > 0.5) != yTest[-trainIndx])
print(paste("test-error (sparse.default) = ", err.sparse.param2))

####

bst.sparse.param3 <- xgboost(param = param3, data = xTrain[trainIndx, ], label = yTrain[trainIndx], nrounds = 500, verbose = 1)

xgb.save(bst.sparse.param3, "april28-svm-cv/April28_xgb_sparse_75_25_param3_290features.model")

# Make prediction
pred.sparse.param3 = predict(bst.sparse.param3, xTest[-trainIndx, ])

# first 10 lines of model
model.sparse.param3 <- xgb.dump(bst.sparse.param3, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(xTrain)[[2]]
importance_matrix.sparse.param3 <- xgb.importance(names, model = bst.sparse.param3)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.param3[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)


err.sparse.param3 <- mean(as.numeric(pred.sparse.param3 > 0.5) != yTest[-trainIndx])
print(paste("test-error (sparse.default) = ", err.sparse.param3))

###

bst.sparse.param4 <- xgboost(param = param4, data = xTrain[trainIndx, ], label = yTrain[trainIndx], nrounds = 500, verbose = 1)

xgb.save(bst.sparse.param4, "april28-svm-cv/April28_xgb_sparse_75_25_param4_290features.model")

# Make prediction
pred.sparse.param4 = predict(bst.sparse.param4, xTest[-trainIndx, ])

# first 10 lines of model
model.sparse.param4 <- xgb.dump(bst.sparse.param4, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(xTrain)[[2]]
importance_matrix.sparse.param4 <- xgb.importance(names, model = bst.sparse.param4)
# get top 10 most important features
xgb.plot.importance(importance_matrix.sparse.param4[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)


err.sparse.param4 <- mean(as.numeric(pred.sparse.param4 > 0.5) != yTest[-trainIndx])
print(paste("test-error (sparse.default) = ", err.sparse.param4))

################################################################

trainIndx <- trainIndx20.20perc

# -or-

trainIndx <- trainIndx20.10perc

set.seed(2)
testIndx <- sample(1:nrow(x20), size = floor(nrow(x20) * 0.025), replace = F)
set.seed(1)

x <- x20
y <- yLabeled
xTrain = as.matrix(x)
yTrain = as.factor(y)

xTest = as.matrix(x)
yTest = as.factor(y)

#trainIndx <- indx290.train

# -or- 

####

x <- x20[trainIndx20.05perc, ]
y <- yLabeled[trainIndx20.05perc]

# do 75 / 25 split 

trainIndx <- sample(1:nrow(x), size = floor(nrow(x) * 0.75), replace = F)

xTrain = as.matrix(x)
yTrain = as.factor(y)

xTest = as.matrix(x)
yTest = as.factor(y)

####

########################################################
########################################################
########################################################
########################################################

##### Trial runs with Oliver's laptop - April 28 

##### If not using Oliver's laptop go below this section 

########################################################
########################################################
########################################################
########################################################

### These runs use a subsample of 191k tweets 
### and then it breaks them down into 80% train, 20% test 

# this means: 152k train, 38k test tweets 

### ALSO! I am reducing the range of parameters in grid search

#### TRY SOME SVM
set.seed(1)

seqC = c( 0.01, 0.1, 1, 10, 100)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC) {
  print("5-fold CV search for linear SVM: ")
  print(i)
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
  fileIter <- paste("OCM_laptop_CVerror_lin", i, sep = "_")
  rdaLoc <- paste(fileIter, ".rda", sep = "")
  csvLoc <- paste(fileIter, ".csv", sep = "")
  save(CVerror[i], file = paste("april28-svm-cv/SVM_CV_RDA/", rdaLoc, sep = ""))
  save(CVerror[i], file = paste("april28-svm-cv/SVM_CV_CSV/", csvLoc, sep = ""))
}

save(CVerror, "april28-svm-cv/OCM_laptop_20features_Lin.SVM_CVerror.rda")

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xTrain, yTrain, type="C-svc", 
                       kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xTest[testIndx, ])

save(minC_LinearSVM, file = "april28-svm-cv/OCM_laptop_20features_minC_LinearSVM.rda")
save(train_LinearSVM, file = "april28-svm-cv/OCM_laptop_20features_train_LinearSVM.rda")
save(predict_LinearSVM, file = "april28-svm-cv/OCM_laptop_20features_predict_LinearSVM.rda")


##Perform Gaussian kernel SVM modeling and prediction.
seqC = c( 0.01, 0.1, 1, 10, 100)
seqSigma = c(0.01, 0.1, 1, 10, 100)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError.GSVM = rep(NA, times = n.permuPar)

# 5-fold CV with 20% of the data 
for (i in 1:n.permuPar) {
  print("Grid search Guassian Kernel SVM: ")
  print(i)
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type = "C-svc", 
               kernel ="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5, 
               kpar = list(sigma = permuPar[i , 2]))
  CVError.GSVM[i] = svmCV@cross
  fileIter <- paste("OCM_laptop_CVerror_gaussian", i, sep = "_")
  rdaLoc <- paste(fileIter, ".rda", sep = "")
  csvLoc <- paste(fileIter, ".csv", sep = "")
  save(CVError.GSVM[i], file = paste("april28-svm-cv/SVM_CV_RDA/", rdaLoc, sep = ""))
  save(CVError.GSVM[i], file = paste("april28-svm-cv/SVM_CV_CSV/", csvLoc, sep = ""))
}

save(CVError.GSVM, "april28-svm-cv/OCM_laptop_20features_GSVM_CVerror.rda")

minC_GSVM = permuPar[min(which.min(CVError.GSVM)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError.GSVM)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

save(minC_GSVM, "april28-svm-cv/OCM_laptop_20features_minC_GSVM.rda")
save(minSigma_GSVM, "april28-svm-cv/OCM_laptop_20features_minSigma_GSVM.rda")

train_GSVM = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type = "C-svc", 
                  kernel = "rbfdot", scaled = c(), C = minC_GSVM, 
                  kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xTest[testIndx, ])

save(min_GSVM, file = "april28-svm-cv/OCM_laptop_20features_min_GSVM.rda")
save(train_GSVM, file = "april28-svm-cv/OCM_laptop_20features_train_GSVM.rda")
save(predict_GSVM, file = "april28-svm-cv/OCM_laptop_20features_predict_GSVM.rda")

########################################################
########################################################
########################################################
########################################################

##### If not using Oliver's laptop - start here - 

########################################################
########################################################
########################################################
########################################################

set.seed(1)

#seqC = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)
seqC = c(0.01, 0.1, 1, 10, 100)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC) {
  print("5-fold CV search for linear SVM: ")
  print(i)
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
  fileIter <- paste("05perc_CVerror_lin", i, sep = "_")
  rdaLoc <- paste(fileIter, ".rda", sep = "")
  csvLoc <- paste(fileIter, ".csv", sep = "")
  save(CVerror[i], file = paste("april28-svm-cv/SVM_CV_RDA/", rdaLoc, sep = ""))
  save(CVerror[i], file = paste("april28-svm-cv/SVM_CV_CSV/", csvLoc, sep = ""))
}

save(CVerror, "april28-svm-cv/April28_20features_Lin.SVM_05perc_CVerror.rda")

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xTrain, yTrain, type="C-svc", 
                       kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xTest[testIndx, ])

save(minC_LinearSVM, file = "april28-svm-cv/April28_20features_05perc_minC_LinearSVM.rda")
save(train_LinearSVM, file = "april28-svm-cv/April28_20features_05perc_train_LinearSVM.rda")
save(predict_LinearSVM, file = "april28-svm-cv/April28_20features_05perc_predict_LinearSVM.rda")


##Perform Gaussian kernel SVM modeling and prediction.
#seqC = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
seqC = c(0.01, 0.1, 1, 10, 100)
#seqSigma = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
seqSigma = c(0.01, 0.1, 1, 10, 100)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError.GSVM = rep(NA, times = n.permuPar)

# 5-fold CV with 5% or 10% or 20% of the data 
for (i in 1:n.permuPar) {
  print("Grid search Guassian Kernel SVM: ")
  print(i)
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type = "C-svc", 
               kernel ="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5, 
               kpar = list(sigma = permuPar[i , 2]))
  CVError.GSVM[i] = svmCV@cross
  fileIter <- paste("05perc_CVerror_gaussian", i, sep = "_")
  rdaLoc <- paste(fileIter, ".rda", sep = "")
  csvLoc <- paste(fileIter, ".csv", sep = "")
  save(CVError.GSVM[i], file = paste("april28-svm-cv/SVM_CV_RDA/", rdaLoc, sep = ""))
  save(CVError.GSVM[i], file = paste("april28-svm-cv/SVM_CV_CSV/", csvLoc, sep = ""))
}

save(CVError.GSVM, "april28-svm-cv/April28_20features_05perc_GSVM_CVerror.rda")

minC_GSVM = permuPar[min(which.min(CVError.GSVM)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError.GSVM)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

save(minC_GSVM, "april28-svm-cv/April28_20features_05perc_minC_GSVM.rda")
save(minSigma_GSVM, "april28-svm-cv/April28_20features_05perc_minSigma_GSVM.rda")

train_GSVM = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type = "C-svc", 
                  kernel = "rbfdot", scaled = c(), C = minC_GSVM, 
                  kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xTest[testIndx, ])

save(min_GSVM, file = "april28-svm-cv/April28_20features_05perc_min_GSVM.rda")
save(train_GSVM, file = "april28-svm-cv/April28_20features_05perc_train_GSVM.rda")
save(predict_GSVM, file = "april28-svm-cv/April28_20features_05perc_predict_GSVM.rda")

########## OLD ##########
########## OLD ##########
########## OLD ##########
########## OLD ##########

seqC = c(10, 20, 30, 50)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC){
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
}

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xTrain, yTrain, type="C-svc", 
                       kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xTest)

save(minC_LinearSVM, file = "april28-svm-cv/April28_20features_minC_LinearSVM.rda")
save(train_LinearSVM, file = "april28-svm-cv/April28_20features_train_LinearSVM.rda")
save(predict_LinearSVM, file = "april28-svm-cv/April28_20features_predict_LinearSVM.rda")

############### OLD 

seqC = c(10, 20, 30, 50)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC){
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc",
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
}

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xTrain, yTrain, type="C-svc",
                       kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xTest)

save(minC_LinearSVM, file = "april28-svm-cv/April28_minC_LinearSVM_290features.rda")
save(train_LinearSVM, file = "april28-svm-cv/April28_train_LinearSVM_290features.rda")
save(predict_LinearSVM, file = "april28-svm-cv/April28_predict_LinearSVM_290features.rda")

##Perform Gaussian kernel SVM modeling and prediction.
seqC = c(10, 20, 50, 100)
seqSigma = c(0.1, 0.5, 1, 2)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError = rep(NA, times = n.permuPar)

for (i in 1:n.permuPar){
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc",
               kernel="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5,
               kpar = list(sigma = permuPar[i , 2]))
  CVerror[i] = svmCV@cross
}

minC_GSVM = permuPar[min(which.min(CVError)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

xTrain <- xTrain[trainIndx, ]
yTrain <- yTrain[trainIndx]
xTest <- xTest[-trainIndx, ]

train_GSVM = ksvm(xTrain, yTrain, type = "C-svc",
                  kernel = "rbfdot", scaled = c(), C = minC_GSVM,
                  kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xTest)

save(min_GSVM, file = "april28-svm-cv/April28_min_GSVM_290features.rda")
save(train_GSVM, file = "april28-svm-cv/April28_train_GSVM_290features.rda")
save(predict_GSVM, file = "april28-svm-cv/April28_predict_GSVM_290features.rda")


### END ###
