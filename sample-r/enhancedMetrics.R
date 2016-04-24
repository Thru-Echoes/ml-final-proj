# Note: all (or most) of this is from class / discussion R script
# load like this:
# # > source("ClassificationMetrics.R")

library(glmnet)
accuracy_score = function(y, yhat) {
  # Accuracy of yhat
  sum(y == yhat) / length(y)
}

zero_one_loss = function(y, yhat) {
  # 1 - the accuracy score is called the zero one loss
  1 - accuracy_score(y, yhat)
}

# Area Under Curve Measure of Fit
roc_auc_score = glmnet::auc

confusion_matrix = function(y, yhat) {
  a = matrix(0, nrow = 2, ncol = 2)
  a[1,1] = sum((yhat == 1) & (y == 1))
  a[2,1] = sum((yhat == 0) & (y == 1))
  a[1,2] = sum((yhat == 1) & (y == 0))
  a[2,2] = sum((yhat == 0) & (y == 0))
  return(a)
}

precision_score = function(y, yhat) {
  a = confusion_matrix(y, yhat)
  a[1,1] / (a[1,1] + a[1,2])
}

recall_score = function(y, yhat) {
  a = confusion_matrix(y, yhat)
  a[1,1] / (a[1,1] + a[2,1])
}

f1_score = function(y, yhat) {
  precision = precision_score(y, yhat)
  recall = recall_score(y, yhat)
  2 * (precision * recall) / (precision + recall)
}

all_scores = function(y, prob, yhat = NULL) {
  if(is.null(yhat)) {
    yhat = as.numeric(prob > 0.5)
  }
  base_acc = max(table(y)) / length(y)
  set.seed(1)
  base_roc = roc_auc_score(y, rnorm(length(y)))
  base_f1  = f1_score(y, rep(1, length(y)))

  cat(sprintf("Type              | Ours  | Baseline\n" ))
  cat(sprintf("Accuracy Score    | %.4f|   %.4f\n", accuracy_score(y, yhat), base_acc))
  cat(sprintf("ROC Score         | %.4f|   %.4f\n", roc_auc_score(y, prob), base_roc))
  cat(sprintf("F1 Score          | %.4f|   %.4f\n", f1_score(y, yhat), base_f1))
}


####################################################################################
########## Next set of helper functions
####################################################################################

GetLDAProb = function(Xtrain, ytrain, Xvalid) {
  model = lda(x = Xtrain, grouping = ytrain)
  predict(model, newdata = Xvalid)$posterior[,2]
}

GetQDAProb = function(Xtrain, ytrain, Xvalid) {
  model = qda(x = Xtrain, grouping = ytrain)
  predict(model, newdata = Xvalid)$posterior[,2]
}

GetLogisticL1Prob = function(Xtrain, ytrain, Xvalid, lambda) {
  model = glmnet(x = Xtrain, y = ytrain, family = "binomial", lambda = lambda)
  predict(model, Xvalid)
}

GetLogisticL1ProbFunc = function(lambda) {
  return(function(Xtrain, ytrain, Xvalid) GetLogisticL1Prob(Xtrain, ytrain, Xvalid, lambda))
}

GetFoldID = function(n, n_fold) {
  fold_id = rep(1:n_fold, 1 + n / n_fold)
  set.seed(2)
  fold_id = sample(fold_id, size = n)
  fold_id = fold_id[1:n]
  return(fold_id)
}

GetCVPrediction = function(model, X, y, n_fold) {
  n = length(y)
  fold_id = GetFoldID(n, n_fold)
  prob = numeric(n)
  for (fold in 1:n_fold) {
    prob[fold_id == fold] = model(X[fold_id != fold, ], y[fold_id != fold],
                                  X[fold_id == fold, ])
  }
  return(prob)
}

####################################################################################
########## Example usage
####################################################################################

lda_prob = GetCVPrediction(GetLDAProb, X, y, 5)
all_scores(y, lda_prob)
qda_prob = GetCVPrediction(GetQDAProb, X, y, 5)
all_scores(y, qda_prob)
best_lambda = cv.glmnet(X, y, type.measure = "class", family = "binomial")$lambda.min
log_prob = GetCVPrediction(GetLogisticL1ProbFunc(best_lambda), X, y, 5)
all_scores(y, log_prob)
