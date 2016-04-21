###############################################################################################
# Overview: set of helper functions for performing feature selection
#               as well as model validity / accuracy / stability
#
#
# General Topics:
# # 1. get bag-of-words scores for data and feed into randomForest()
# # 2. compare splits ranging from 90/10 (train/test) to 50/50
# # 3. use a Confusion Matrix (i.e. Contingency Table)
#
#
# Estimate Model Accuracy Methods:
# # 1. Data split
# # 2. Bootstrap
# # 3. k-fold Cross Validation
# # 4. Repeated k-fold Cross Validation
# # 5. Leave One Out Cross Validation
#
#
# Validity Measurements / Tests:
# # 1. AIC (for parameter models)
# # 2. Cross Validation (CV)
# # 3. Area Under the Curve (AUC)
# # 4. Region Over Curve (ROC)
# # 5. Logloss of train and test
# # 6. Flat accuracy (i.e. <code>if y_hat of i = y of i</code>)
# # 7. F1
# # 8. Term Frequency-Inverse Document Frequency (TF-IDF)
#
#
# Notes:
# # i. bag-of-scores ~= word2vec in Python
# #     creates a set of predictors where each feature (i.e. each column) is a word
# #     and the row entry is the corresponding frequency for that word in that Tweet
# #
# # ii. TF-IDF = commonly used metric for scoring phrase's importance in Tweet
# #     compares phrase's frequency in Tweet to frequency of occurrence in collection of Tweets
# #
# # iii. cluster analysis = behavior of groups of words (e.g. hclust(...))
#
#
# More References:
# # http://www.r-bloggers.com/computing-classification-evaluation-metrics-in-r/
# # http://artax.karlin.mff.cuni.cz/r-help/library/rminer/html/mmetric.html
# # http://blog.dato.com/how-to-evaluate-machine-learning-models-part-2a-classification-metrics
# # http://search.r-project.org/library/performanceEstimation/html/classificationMetrics.html
# # http://scg.sdsu.edu/rf_r/
###############################################################################################
# Random Forest with bag-of-scores
###############################################################################################
library(rpart)
library(randomForest)
#bagScores.forest <- randomForest(...)

## ...

###############################################################################################
# Random Forests: version 1
###############################################################################################
library(randomForest)

# set number of trees in forest
numTrees <- 1000
isImportance = TRUE

# random seed
set.seed(1892)

# predict
rfModel <- randomForest(x = xTrain, y = as.factor(yTrain), ntree = numTrees, do.trace = TRUE, importance = isImportance)
rfModel.myPred <- predict(rfModel, xTest)

# correct classification (for binary response)
rfModel.correctPred <- (rfModel.myPred == yTest)
rfModel.numCorrect <- length(rfModel.correctPred)

# misclassification error
rfModel.misClass <- 1 - sum(rfModel.correctPred) / rfModel.numCorrect

# Get variable importance plot:
varImpPlot(rfModel)

## output of rfModel as it runs:
### OOB: out of bag
### col 1:
### col 2:

###############################################################################################
# Random Forests: version 2
###############################################################################################

## Function to do randomForest
# train: training data
# test: testing data
# numTrees: number of trees
doRF <- function(xTrain, yTrain, xTest, yTest, numTrees) {
    library(randomForest)
    isImportance = TRUE

    # random seed
    set.seed(1892)

    # predict
    rfModel <- randomForest(x = xTrain, y = as.factor(yTrain), ntree = numTrees, do.trace = TRUE, importance = isImportance)
    rfModel.myPred <- predict(rfModel, xTest)

    # correct classification (for binary response)
    rfModel.correctPred <- (rfModel.myPred == yTest)
    rfModel.numCorrect <- length(rfModel.correctPred)

    # misclassification error
    rfModel.misClass <- 1 - sum(rfModel.correctPred) / rfModel.numCorrect

    return(rfModel)
}

## Example usage:
#exRF <- doRF(xTrain, yTrain, xTest, yTest, 100)
#exRF.numCorrect
## See the variable importance plot
#varImpPlot(exRF)


###############################################################################################
# Gradient Boosting & XGBoost
###############################################################################################
#library(gbm)

#library(xgboost)

###############################################################################################
# Feature Selection
###############################################################################################
library(mlbench)
library(caret)

## Machine Learning blog - general model accuracy methods described + code
# http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# http://machinelearningmastery.com/classification-accuracy-is-not-enough-more-performance-measures-you-can-use/

## For binary classification - evaluation using ROCR, accuracy, F measure, percision + code
# https://brenocon.com/blog/2009/04/binary-classification-evaluation-in-r-via-rocr/

###############################################################################################
# Feature Engineering
###############################################################################################
### Argument: data frame of features
### Return: data frame of features augmented

augmentFeatures <- function(df) {
    ## function from this tutorial
    # https://www.kaggle.com/c/bike-sharing-demand/forums/t/11525/tutorial-0-433-score-with-randomforest-in-r
    # ...
    # is needed?
}

###############################################################################################
# ArgMax Score
###############################################################################################

## From GSI:
#ArgMax = function(x) {
#  res = numeric(nrow(x))
#  for(i in 1:nrow(x)) {
#    res[i] = which.max(x[i,])
#  }
#  return(res)
#}
#yhat = ArgMax(yhat) - 1
#accuracy_score(y[valid], yhat)

doArgMax <- function(x) {
    getRes <- numeric(nrow(x))

    for (i in 1:nrow(x)) {
        getRes[i] <- which.max(x[i, ])
    }

    return(getRes)
}

yHat <- doArgMax(yHat) - 1
# accuracy_score(yTest, yHat)

###############################################################################################
# Classification Metrics: part 1
###############################################################################################

# http://stackoverflow.com/questions/32929344/k-fold-cross-validation-using-r-for-linear-tree-model-classification

## Calculate yHat.
### myPred:
### threshold: default 0.5, no need to change since binary classification
### labels: response types / factors (i.e. 0 or 1)
getYhat <- function(myPred, threshold = 0.5, labels = class.labels) {
    factor(as.numeric(myPred > threshold), levels = 0:1, labels = labels)
}

## Calculate specificity
getSpec <- function(class, yhat) {
  conf <- table(class, yhat)
  conf[1,1] / sum(conf[1, ])
}

## Calculate sensitivity
getSens <- function(class, yhat) {
  conf <- table(class, yhat)
  conf[2,2] / sum(conf[2, ])
}

## Calculate accuracy
getAcc <- function(class, yhat) {
  sum(diag(table(class, yhat))) / length(class)
}

## Calculate summary statistics
getStats <- function(class, yhat) {
  c(accuracy = getAcc(class, yhat),
    sensitivity = getSens(class, yhat),
    specificity = getSpec(class, yhat))
}

neg.label <- 0
pos.label <- 1
class.labels <- c(neg.label, pos.label)

# DO NOT THINK WE NEED THIS NEXT SECTION...
#############################################
### augment data to have clearly identified pos label class
#someData <- read.csv(file.choose())
#someData$posResponse <- factor(as.numeric(someData$posResponse == pos.label), labels = 0:1, labels = class.labels)
#############################################

### Use with randomForest() --> doRF() function from above
exRF <- doRF(xTrain, yTrain, xTest, yTest, 100)

# predictions in exRF.myPred --> .myPred is ~= yHat
exRF.myYhat <- getYhat(exRF.myPred)

# get summary statistics
exRF.summaryStats <- getStats(yTest, exRF.myYhat)
exRF.summaryStats

###############################################################################################
# Classification Metrics: part 2
###############################################################################################

# df could be xTrain etc
#numRows <- nrow(df)
numRows <- 100 # default

### CV - 10 fold
nFolds <- 10
getIdx <- sample(rep(1:nFolds, length = numRows))

### Prediction vectors
yHat.linear <- rep(NA, numRows)
yHat.tree <- rep(NA, numRows)

### build k-fold models
for (k in 1:n.folds) {
    fold <- which(fold.idx == k)
    linear.model <- lm(f, data.set[-fold, ])
    tree.model <- tree(f, data.set[-fold, ])
    yhat.linear[fold] <- predict(linear.model, data.set[fold, ])
    yhat.tree[fold] <- predict(tree.model, data.set[fold, ])
}

y <- data.set$medv
rmse.linear <- sqrt(mean((y-yhat.linear)^2))
rmse.tree <- sqrt(mean((y-yhat.tree)^2))



##
