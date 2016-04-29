# Loading libraries
library(e1071)
library(Matrix)

# Loading the partitioned data
load("SVMsparse.RData")

# I measured the quality of my classifiers simply by calculating the overall 
# prediction success rate and the success rates for each category of the 
# response variable. I created the following function to do this:
class.test <- function(real, predicted) {
  # Assesses the accuracy of a model's predictions
  ct <- table(real, predicted)
  # [[1]] Percent correct for each category and [[2]] Total percent correct
  return(list(diag(prop.table(ct, 1)), sum(diag(prop.table(ct)))))
}

# Model
svm.model <- svm(x=Xtrain, y=ytrain, gamma=1/294, cost=1, kernel="linear")

# Prediction
svm.model.predictions <- predict(svm.model, newdata=Xtest)

# Performance
svm.model.performance <- class.test(ytest, svm.model.predictions)

# Save Information
save(svm.model, svm.model.predictions, svm.model.performance, file="SVMresults.RData")
