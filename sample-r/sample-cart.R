##############################
# CART - decision tree 
##############################
library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, method = "class", 
             data = kyphosis, maxdepth = 5, minsplit = 20)
yhat <- predict(fit, kyphosis, type = "class")
plot(fit, uniform = TRUE, main = "Classification Tree for Kyphosis")
text(fit, use.n = TRUE, all = TRUE, cex = 0.8)

### Display Results 
printcp(fit)
### Visualize cross-validation results 
plotcp(fit) 

summary(fit)

### cp = complexity, higher number means smaller tree

##############################
# Random Forest
##############################
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start, data = kyphosis, ntree = 2000)
print(fit)
importance(fit) 

##############################
# Gini Index (palabara upside-down) 
##############################
## = w * [p * (1 - p)] 
## measures purity of node 
## if all y = 0, Gini = 0 
## if all y = 1, Gini = 0 
#
## bigger Gini = higher cost (want small Gini)
#
## if 10 rows, first row y = 0, & split at first node... 
## Gini has weights of nodes 
## first split has y1 = 0, y2 to y10 = [0, 1, 0, 1, 1, 1, 0, 1, 1]
### (1 / 10) * (0) + (9 / 10) * (3 / 9) = 0.004 
#
#
# Example Gini with: Y = [0, 0, 1, 0, 1, 1, 1, 0, 1, 1]
## If split at... [0, 0, 1, 0] SPLIT [1, 1, 1, 0, 1, 1]
## 4/10 * [(3/4) * (1/4)] + 6/10 * [(5/6) * (1/6)] 

##############################
# Entropy (upside-down V)
##############################
## p log(p) + (1 - p) log(1 - p)
