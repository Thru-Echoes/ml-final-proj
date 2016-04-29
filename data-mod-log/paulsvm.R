suppressMessages(library(e1071))

# I measured the quality of my classifiers simply by calculating the overall 
# prediction success rate and the success rates for each category of the 
# response variable. I created the following function to do this:
class.test <- function(real, predicted) {
  # Assesses the accuracy of a model's predictions
  ct <- table(real, predicted)
  # [[1]] Percent correct for each category and [[2]] Total percent correct
  return(list(diag(prop.table(ct, 1)), sum(diag(prop.table(ct)))))
}

# Loading data
url <- "http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
heart <- read.table(url, sep=",", head=TRUE, row.names=1)
head(heart)
any(is.na(heart))
heart$chd <- as.factor(heart$chd)

# Partitioning the data set
n <- nrow(heart)
sample.size <- floor(0.8*n)
set.seed(123)
train.sample <- sample(1:n, sample.size)
heart.train <- heart[train.sample,]
heart.test <- heart[-train.sample,]

# Parameter Tuning
set.seed(123)
svm.model.tuned <- tune.svm(chd ~ ., data=heart.train, gamma=2^(-7:2), cost=2^(0:7))
svm.model.tuned$performances
svm.model.tuned$best.model

# The tune.svm function, using the above parameter value ranges, determined that
# the model with the best classification performance has parameter values
# gamma=0.0078125 and cost=8.

# Model
svm.model <- svm(chd ~ ., data=heart.train, gamma=0.0078125, cost=8)

# Prediction
svm.model.predictions <- predict(svm.model, newdata=heart.test[,-10])

# Performance
class.test(heart.test[,10], svm.model.predictions)
