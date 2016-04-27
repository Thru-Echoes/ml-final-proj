# Do comparison of XGB and SVM
#
# use both 164features and 169features

# AUC accounts for error of classification
#   of both positive and negative responses
#
# Min Error of test is misclassification error
#   it only really accounts for ability of
#       positive response

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

final_164features <- tfIdf.labeled
save(final_164features, file = "run-svm-xgb/April27_trainAllLabel_164features.rda")

final_169features <- cbind(tfIdf.labeled, allLabelFeatures.user.hash)
save(final_169features, file = "run-svm-xgb/April27_trainAllLabel_169features.rda")

###### Get 75% train / 25% test split

set.seed(1)
trainIndx <- sample(1:nrow(final_169features), size = floor(nrow(final_169features) * 0.75), replace = F)
x <- final_169features
y <- yAllLabel

###### After 250 random parameter runs - try again with only 164 tf-idf features:

set.seed(1)
trainIndx <- sample(1:nrow(final_164features), size = floor(nrow(final_164features) * 0.75), replace = F)
x <- final_164features
y <- yAllLabel

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
bst.default <- xgboost(param = fixedParam, data = as.matrix(x[trainIndx, ]), label = y[trainIndx], nrounds = nrounds.default, verbose = 2)

xgb.save(bst.default, "run-svm-xgb/April27_169features_xgb.default.model")

# Make prediction
pred.default = predict(bst.default, as.matrix(x[-trainIndx, ]))

# first 10 lines of model
model.default <- xgb.dump(bst.default, with.stats = T)
model.default[1:10]

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix.default <- xgb.importance(names, model = bst.default)
# get top 10 most important features
xgb.plot.importance(importance_matrix.default[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc1 <- xgboost(param = optimalAUC1, data =  as.matrix(x[trainIndx, ]), label = y[trainIndx], nrounds = nrounds.auc1, verbose = 2)

xgb.save(bst.auc1, "run-svm-xgb/April27_169features_xgb.AUC1.model")

# Make prediction
pred.auc1 = predict(bst.auc1, as.matrix(x[-trainIndx, ]))

# first 10 lines of model
model.auc1 <- xgb.dump(bst.auc1, with.stats = T)
#model1.default[1:10]

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix.auc1 <- xgb.importance(names, model = bst.auc1)
# get top 10 most important features
#xgb.plot.importance(importance_matrix.auc1[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc2 <- xgboost(param = optimalAUC2, data = as.matrix(x[trainIndx, ]), label = y[trainIndx], nrounds = nrounds.auc2, verbose = 2)

xgb.save(bst.auc2, "run-svm-xgb/April27_169features_xgb.AUC2.model")

# Make prediction
pred.auc2 = predict(bst.auc2, as.matrix(x[-trainIndx, ]))

# first 10 lines of model
model.auc2 <- xgb.dump(bst.auc2, with.stats = T)
#model1.default[1:10]

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix.auc2 <- xgb.importance(names, model = bst.auc2)
# get top 10 most important features
#xgb.plot.importance(importance_matrix.auc2[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.default, n_first_tree = 2)

########################################
#### RUN MODELS WITH OPTIMAL PARAMS ####
########################################

bst.auc3 <- xgboost(param = optimalAUC3, data = as.matrix(x[trainIndx, ]), label = y[trainIndx], nrounds = nrounds.auc3, verbose = 2)

xgb.save(bst.auc3, "run-svm-xgb/April27_169features_xgb.AUC3.model")

# Make prediction
pred.auc3 = predict(bst.auc3, as.matrix(x[-trainIndx, ]))

# first 10 lines of model
model.auc3 <- xgb.dump(bst.auc3, with.stats = T)
#model1.auc3[1:10]

names <- dimnames(x[trainIndx, ])[[2]]
importance_matrix.auc3 <- xgb.importance(names, model = bst.auc3)
# get top 10 most important features
xgb.plot.importance(importance_matrix.auc3[1:10, ])

# tree graph
#xgb.plot.tree(feature_names = names, model = bst.auc3, n_first_tree = 2)


########################################


########################################


########################################
###### RERUN WITH SPARSE MATRICES ######
########################################

trainSparse <- Matrix(data = as.matrix(x[trainIndx, ]), sparse = TRUE)
testSparse <- Matrix(data = as.matrix(x[-trainIndx, ]), sparse = TRUE)

trainY <- y[trainIndx]
testY <- y[-trainIdx]

########################################

bst.sparse.default <- xgboost(param = fixedParam, data = trainSparse, label = trainY, nrounds = nrounds.default, verbose = 2)

xgb.save(bst.sparse.default, "run-svm-xgb/April27_xgb.best_sparse_75_25_default.model")

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

xgb.save(bst.sparse.auc1, "run-svm-xgb/April27_xgb.best_sparse_75_25_AUC1.model")

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

xgb.save(bst.sparse.auc2, "run-svm-xgb/April27_xgb.best_sparse_75_25_AUC2.model")

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

xgb.save(bst.sparse.auc3, "run-svm-xgb/April27_xgb.best_sparse_75_25_AUC3.model")

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
