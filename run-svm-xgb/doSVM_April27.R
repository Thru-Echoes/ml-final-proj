##Perform linear SVM modeling and prediction.
#load("train80_55features_april25.rdata")
#load("test20_55features_april25.rdata")
require(kernlab)

#trainNow <- train_55features
#testNow <- test_55features
#set.seed(1)
#trainIndx <- sample(1:nrow(trainNow), size = floor(nrow(trainNow) * 0.05), replace = F)

#xTrain = as.matrix(trainNow[, -1])
#yTrain = as.factor(trainNow[, 1])

#xTest = as.matrix(testNow[, -1])
#yTest = as.factor(testNow[, 1])

############################################
# Have the following code from xgboost code 
############################################

load("run-svm-xgb/April27_xgbTrainAllLabel_169features.rda")
load("run-svm-xgb/April27_xgbTrainAllLabel_164features.rda")
load("data/y.RData")

x164 <- final_164features.df
x169 <- final_169features

#trainIndx164.25, trainIndx164.50
#trainIndx169.25, trainIndx169.50
set.seed(1)
trainIndx164.05 <- sample(1:nrow(x164), size = floor(nrow(x164) * 0.05), replace = F)
trainIndx169.05 <- sample(1:nrow(x169), size = floor(nrow(x169) * 0.05), replace = F)

xSVM164 <- x164[trainIndx164.05, ]
ySVM164 <- y[trainIndx164.05]

xSVM168 <- x169[trainIndx169.05, ]
ySVM168 <- y[trainIndx169.05]

svmIndx.164 <- sample(1:nrow(xSVM164), size = floor(nrow(xSVM164) * 0.75), replace = F)
svmIndx.168 <- sample(1:nrow(xSVM168), size = floor(nrow(xSVM168) * 0.75), replace = F)

svmIndx.164.tester <- sample(1:nrow(xSVM164), size = floor(nrow(xSVM164) * 0.75), replace = F)
svmIndx.168.tester <- sample(1:nrow(xSVM168), size = floor(nrow(xSVM168) * 0.75), replace = F)

############################################
# Try some initial linear-SVM 
############################################

########### TESTER RUN! 

train_Linear164SVM.tester = ksvm(as.matrix(xSVM164[svmIndx.164.tester, ]), y[svmIndx.164.tester], type="C-svc", 
                          kernel="vanilladot", scaled = c(), C = 1)


############

train_Linear164SVM = ksvm(as.matrix(xSVM164[svmIndx.164, ]), y[svmIndx.164], type="C-svc", 
                       kernel="vanilladot", scaled = c(), C = 1)
predict_Linear164SVM = predict(train_Linear164SVM, newdata = xSVM164[-svmIndx.164, ])

save(train_Linear164SVM, file = "run-svm-xgb/April27_train_LinearSVM_164features.rda")
save(predict_Linear164SVM, file = "run-svm-xgb/April27_predict_LinearSVM_164features.rda")

#####
#####
#####

train_Linear168SVM = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type="C-svc", 
                       kernel="vanilladot", scaled = FALSE, C = 1)
predict_Linear168SVM = predict(train_Linear168SVM, newdata = xSVM168[-svmIndx.168, ])

save(train_Linear168SVM, file = "run-svm-xgb/April27_train_LinearSVM_168features.rda")
save(predict_Linear168SVM, file = "run-svm-xgb/April27_predict_LinearSVM_168features.rda")

############################################
# Try some initial kernel-SVM 
############################################

train_164GSVM = ksvm(xSVM164[svmIndx.164, ], y[svmIndx.164], type = "C-svc", 
                  kernel = "rbfdot", scaled = c(), C = 10, 
                  kpar = list(sigma = minSigma_GSVM))

predict_164GSVM = predict(train_164GSVM, newdata = xSVM164[-svmIndx.164, ])

save(train_164GSVM, file = "run-svm-xgb/April27_train_GSVM_164features.rda")
save(predict_164GSVM, file = "run-svm-xgb/April27_predict_GSVM_164features.rda")

#####
#####
#####

train_168GSVM = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type = "C-svc", 
                  kernel = "rbfdot", scaled = c(), C = 10, 
                  kpar = list(sigma = minSigma_GSVM))

predict_168GSVM = predict(train_168GSVM, newdata = xSVM168[-svmIndx.168, ])

save(train_168GSVM, file = "run-svm-xgb/April27_train_GSVM_168features.rda")
save(predict_168GSVM, file = "run-svm-xgb/April27_predict_GSVM_168features.rda")

############################################
# BELOW IS SVM PARAMETER TUNING 
############################################

seqC = c(10, 20, 30, 50)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC){
  svmCV = ksvm(xSVM164[svmIndx.164, ], y[svmIndx.164], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
}

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xSVM164[svmIndx.164, ], y[svmIndx.164], type="C-svc", 
                kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xSVM164[-svmIndx.164, ])

save(minC_LinearSVM, file = "run-svm-xgb/April27_minC_LinearSVM_164features.rda")
save(train_LinearSVM, file = "run-svm-xgb/April27_train_LinearSVM_164features.rda")
save(predict_LinearSVM, file = "run-svm-xgb/April27_predict_LinearSVM_164features.rda")

##Perform Gaussian kernel SVM modeling and prediction.
seqC = c(10, 20, 50, 100)
seqSigma = c(0.1, 0.5, 1, 2)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError = rep(NA, times = n.permuPar)

for (i in 1:n.permuPar){
  svmCV = ksvm(xSVM164[svmIndx.164, ], y[svmIndx.164], type="C-svc", 
               kernel="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5, 
               kpar = list(sigma = permuPar[i , 2]))
  CVerror[i] = svmCV@cross
}

minC_GSVM = permuPar[min(which.min(CVError)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

train_GSVM = ksvm(xSVM164[svmIndx.164, ], y[svmIndx.164], type = "C-svc", 
                 kernel = "rbfdot", scaled = c(), C = minC_GSVM, 
                 kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xSVM164[-svmIndx.164, ])

save(min_GSVM, file = "run-svm-xgb/April27_min_GSVM_164features.rda")
save(train_GSVM, file = "run-svm-xgb/April27_train_GSVM_164features.rda")
save(predict_GSVM, file = "run-svm-xgb/April27_predict_GSVM_164features.rda")

############################################
# Now redo all above with 168features  
############################################

seqC = c(10, 20, 30, 50)
n.seqC = length(seqC)
CVerror = rep(NA, times = n.seqC)

for (i in 1:n.seqC){
  svmCV = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type="C-svc", 
               kernel="vanilladot", scaled = c(), C = seqC[i], cross = 5)
  CVerror[i] = svmCV@cross
}

minC_LinearSVM = min(seqC[which.min(CVerror)])
train_LinearSVM = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type="C-svc", 
                       kernel="vanilladot", scaled = c(), C = minC_LinearSVM)
predict_LinearSVM = predict(train_LinearSVM, newdata = xSVM168[-svmIndx.168, ])

save(minC_LinearSVM, file = "run-svm-xgb/April27_minC_LinearSVM_168features.rda")
save(train_LinearSVM, file = "run-svm-xgb/April27_train_LinearSVM_168features.rda")
save(predict_LinearSVM, file = "run-svm-xgb/April27_predict_LinearSVM_168features.rda")

##Perform Gaussian kernel SVM modeling and prediction.
seqC = c(10, 20, 50, 100)
seqSigma = c(0.1, 0.5, 1, 2)

permuPar = expand.grid(seqC, seqSigma)
n.permuPar = nrow(permuPar)

CVError = rep(NA, times = n.permuPar)

for (i in 1:n.permuPar){
  svmCV = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type="C-svc", 
               kernel="rbfdot", scaled = c(), C = permuPar[i, 1], cross = 5, 
               kpar = list(sigma = permuPar[i , 2]))
  CVerror[i] = svmCV@cross
}

minC_GSVM = permuPar[min(which.min(CVError)), 1]
minSigma_GSVM = permuPar[min(which.min(CVError)), 2]
min_GSVM = c(minC_GSVM, minSigma_GSVM)

train_GSVM = ksvm(xSVM168[svmIndx.168, ], y[svmIndx.168], type = "C-svc", 
                  kernel = "rbfdot", scaled = c(), C = minC_GSVM, 
                  kpar = list(sigma = minSigma_GSVM))

predict_GSVM = predict(train_GSVM, newdata = xSVM168[-svmIndx.168, ])

save(min_GSVM, file = "run-svm-xgb/April27_min_GSVM_168features.rda")
save(train_GSVM, file = "run-svm-xgb/April27_train_GSVM_168features.rda")
save(predict_GSVM, file = "run-svm-xgb/April27_predict_GSVM_168features.rda")