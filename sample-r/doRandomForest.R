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


### 10 trees 


# random seed
set.seed(1892)

# predict
rfModel10 <- randomForest(x = xTrain, y = as.factor(yTrain), ntree = 10, do.trace = TRUE, importance = isImportance)
rfModel10.myPred <- predict(rfModel10, xTest)

# correct classification (for binary response)
rfModel10.correctPred <- (rfModel10.myPred == yTest)
rfModel10.numCorrect <- length(rfModel10.correctPred)

# misclassification error
rfModel10.misClass <- 1 - sum(rfModel10.correctPred) / rfModel10.numCorrect

# Get variable importance plot:
varImpPlot(rfModel10)
