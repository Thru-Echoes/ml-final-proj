# LASSO/Elastic-Net GLM
suppressMessages(library(glmnet))

# I measured the quality of my classifiers simply by calculating the overall
# prediction success rate and the success rates for each category of the 
# response variable. I created the following function to do this:
class.test <- function(real, predicted) {
  # Assesses the accuracy of a model's predictions
  ct <- table(real, predicted)
  # [[1]] Percent correct for each category and [[2]] Total percent correct
  return(list(diag(prop.table(ct, 1)), sum(diag(prop.table(ct)))))
}

################################### NEW DATA ###################################

load("April26_tf_idfMatrix_AllRaw.rda")
load("../data/featureMatrix.RData")
load("../data/y.RData")

X <- Matrix(cbind(tf_idf[50001:1578627,], as.matrix(X)), sparse=TRUE)

rm(tf_idf)

# Partitioning the data set
n <- nrow(X)
sample.size <- floor(0.8*n)
set.seed(123)
train.sample <- sample(1:n, sample.size)
train <- list(X=X[train.sample,], y=y[train.sample])
test <- list(X=X[-train.sample,], y=y[-train.sample])

save.image(file="FINAL.RData")

################################### NEW DATA ###################################

################################### OLD DATA ###################################

load("../data/xgb-april25/train80_55features_april25.RData")
load("../data/xgb-april25/test20_55features_april25.RData")

train.data <- subset(train_55features, select=-tweet.lengths)
test.data <- subset(test_55features, select=-tweet.lengths)

train.data <- list(x=as.matrix(subset(train.data, select=-Sentiment)), y=as.factor(train.data$Sentiment))

test.data <- list(x=as.matrix(subset(test.data, select=-Sentiment)), y=as.factor(test.data$Sentiment))

head(train.data$x)
head(train.data$y)

head(test.data$x)
head(test.data$y)

###############################################################
train.data$x <- Matrix(data=train.data$x, sparse=TRUE)
###############################################################

###############################################################
test.data$x <- Matrix(data=test.data$x, sparse=TRUE)
###############################################################

################################### OLD DATA ###################################

################################## START HERE ##################################

load("FINAL.RData")

# LASSO Logistic Regression
lasso.log.model <- cv.glmnet(train$X, train$y,
                             family="binomial", nfolds=3,
                             type.measure="class", grouped=FALSE)
# Misclassification error is the criterion for leave-one-out cross-validation.

plot(lasso.log.model)

lasso.log.model$lambda.min
lasso.log.model$lambda.1se

lasso.coef.min <- as.matrix(coef(lasso.log.model, s="lambda.min"))
which(lasso.coef.min[,1] != 0)
# These are the predictors that LASSO (lambda.min) deemed the most important for
# classification of the response.
lasso.coef.min[which(lasso.coef.min[,1] != 0), 1]
# These are the coefficients of the sparse (lambda.min) estimator.

lasso.coef.1se <- as.matrix(coef(lasso.log.model, s="lambda.1se"))
which(lasso.coef.1se[,1] != 0)
# These are the predictors that LASSO (lambda.1se) deemed the most important for
# classification of the response.
lasso.coef.1se[which(lasso.coef.1se[,1] != 0), 1]
# These are the coefficients of the sparse (lambda.1se) estimator.

# Classification
lasso.test.min <- as.numeric(predict(lasso.log.model, newx=test$X,
                                     s="lambda.min", type="class"))

# Probabilities
lasso.test.min <- predict(lasso.log.model, newx=test$X,
                          s="lambda.min", type="response")

lasso.test.min <- ifelse(predict(lasso.log.model, newx=test$X,
                                 s="lambda.min", type="response") > 0.5, 1, 0)

# Performance
class.test(test$y, lasso.test.min)

# Classification
lasso.test.1se <- as.numeric(predict(lasso.log.model, newx=test$X,
                                     s="lambda.1se", type="class"))

# Probabilities
lasso.test.1se <- predict(lasso.log.model, newx=test$X,
                          s="lambda.1se", type="response")

lasso.test.1se <- ifelse(predict(lasso.log.model, newx=test$X,
                                 s="lambda.1se", type="response") > 0.5, 1, 0)

# Performance
class.test(test$y, lasso.test.1se)
