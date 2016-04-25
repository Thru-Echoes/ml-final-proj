# Code for XGBoost runs
# Dependecies: paramXGB.R

require(xgboost)

# Parameter searching
source("paramXGB.r")

# data = y, 101, 102, 103, ...
target1 <- read.csv(file.choose(), header = TRUE, check.names = FALSE)
target0 <- read.csv(file.choose(), header = TRUE, check.names = FALSE)

# omit NAs
t1 <- na.omit(target1)
newRows <- nrow(t1[1])
t0 <- target0[sample(nrow(target0), newRows), ]

# make dataframe
allDf <- rbind(t1, t0)

# shuffle
df <- allDf[sample(nrow(allDf)), ]

# 70 / 30 split
indices = sample(1:nrow(df), nrow(df) * .3)
train = df[-indices, ]
test = df[indices, ]

# write out
write.csv(test, "data/xgb-april25/xgb_trial1_30test.csv")
write.csv(train, "data/xgb-april25/xgb_trial1_70train.csv")
y <- train[, 1]


########################################
########## CREATE FILE / RM Y ##########
########################################
x = rbind(train[,2:20],test[,2:20])
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)


########################################
############ PARAM SEARCH ##############
########################################

# Linear boosting XGB model
#params10k_tree <- lapply(1:10000, function(i) {
#  print("Linear status: ")
#  print(i)
#  paramLin()
#})

# Tree boosting XGB model
params10k_tree <- lapply(1:10000, function(i) {
  print("Tree status: ")
  print(i)
  paramXGB()
})

########################################
######### FIND OPTIMAL PARAMS ##########
########################################

allMeans <- lappy(1:10000, function(i) {
		params10k_tree[[i]]$minMean
})

# display params for lowest mean error
params10k_tree[[which.min(allMeans)]]


########################################
####### ALT: FIND OPTIMAL PARAMS #######
########################################

params1k_tree1 <- lappy(1:1000, function(i) {
	print("Tree status: ")
	print(i)
	paramXGB()
})

params1k_tree2 <- lappy(1:1000, function(i) {
	print("Tree status: ")
	print(i)
	paramXGB()
})

# ... up to _tree10

treeMeans <- vector()

means1 <- lappy(1:1000, function(i) {
	params1k_tree1[[i]]$minMean
})

treeMeans <- c(treeMeans, list(params1k_tree1[[which.min(means1)]]))

means2 <- lappy(1:1000, function(i) {
	params1k_tree2[[i]]$minMean
})

treeMeans <- c(treeMeans, list(params1k_tree2[[which.min(means2)]]))

# ... up to means10

write.csv(treeMeans, "top_treeBoost_params.csv")

########################################
######## AFTER OPTIMAL PARAMS ##########
########################################

# standard run
nround = 100
bst <- xgboost(param = param, data = x[trind, ], label = y, nrounds = nround, verbose = 2)

# Make prediction
pred = predict(bst, x[teind,])
pred = matrix(pred, 1, length(pred) / 1)
pred = t(pred)

# Output submission
pred = format(pred, digits = 2,scientific = F) # shrink the size of submission
write.csv(pred, file = 'xgb_trial1_nonBoosted_output.csv', quote = FALSE,row.names = FALSE)

########################################
########### CHECK OUT DATA! ############
########################################

# bst == model from XGB
#  -> model of 100 trees
#  		-> each tree built via recursive division of dataset

# first 10 lines of model
model <- xgb.dump(bst, with.stats = T)
model[1:10]

# each line = branch

# feature importance = averaging gain of each feature for all splits and trees

names <- dimnames(x[trind, ])[[2]]
importance_matrix <- xgb.importance(names, model = bst)
# get top 10 most important features
xgb.plot.importance(importance_matrix[1:10, ])

# tree graph
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)

################ END ##################
