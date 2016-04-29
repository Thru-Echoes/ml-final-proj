##Take in prediction and true response. Compute various metrics.
library(ROCR)
yhatPro = yhat
yhatPre = yhatPre
yTrue =y_labeled10

accuracy = sum(yhatPre == yTrue) / length(yTrue)
print(accuracy)

pre = prediction(predictions = yhatPro, labels = yTrue)
auc = as.numeric(performance(pre, "auc")@y.values)
print(auc)
