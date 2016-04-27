##Perform linear SVM modeling and prediction.
load("train80_55features_april25.rdata")
load("test20_55features_april25.rdata")
require(kernlab)

trainNow <- train_55features
testNow <- test_55features
set.seed(1)
trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.05), replace = F)

xTrain = as.matrix(trainNow[, -1])
yTrain = as.factor(trainNow[, 1])

xTest = as.matrix(testNow[, -1])
yTest = as.factor(testNow[, 1])

set.seed(1)

seqC = c(10, 20, 30, 50)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC){
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
}

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xTrain, yTrain, type="C-svc", 
                kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xTest)

save(minC_LinearSVM, file = "minC_LinearSVM.rda")
save(train_LinearSVM, file = "train_LinearSVM.rda")
save(predict_LinearSVM, file = "predict_LinearSVM.rda")

##Perform Gaussian kernel SVM modeling and prediction.
seqC = c(10, 20, 50, 100)
seqSigma = c(0.1, 0.5, 1, 2)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError = rep(NA, times = n.permuPar)

for (i in 1:n.permuPar){
  svmCV = ksvm(xTrain[trainIndx, ], yTrain[trainIndx], type="C-svc", 
               kernel="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5, 
               kpar = list(sigma = permuPar[i , 2]))
  CVerror[i] = svmCV@cross
}

minC_GSVM = permuPar[min(which.min(CVError)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

train_GSVM = ksvm(xTrain, yTrain, type = "C-svc", 
                 kernel = "rbfdot", scaled = c(), C = minC_GSVM, 
                 kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xTest)

save(min_GSVM, file = "min_GSVM.rda")
save(train_GSVM, file = "train_GSVM.rda")
save(predict_GSVM, file = "predict_GSVM.rda")