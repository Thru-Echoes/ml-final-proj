##########################################################################################
##########################################################################################
##########################################################################################
# April 24 BOW on all data
##########################################################################################
##########################################################################################
##########################################################################################

load("data/raw.RData")
#
#
# raw.data = all data with -1 tweets 
#
# rawLabeled = all data without the -1 tweets 

# Clean
raw.data$cleanedText = tolower(gsub("[[:punct:]]", "", raw.data$SentimentText))
rawLabeled.df = raw.data[raw.data$Sentiment != -1, ]

#rawLabeled.df$cleanedText = tolower(gsub("[[:punct:]]", "", rawLabeled.df$SentimentText))

#set.seed(1)
#trainRatio = 0.8
#trainIndex = sample(1:nrow(labeledRaw.df), size = nrow(labeledRaw.df)*trainRatio)

#trainRaw.df = labeledRaw.df[trainIndex, ]
#testRaw.df = labeledRaw.df[-trainIndex, ]


##########################################################################################
##Construct a corpus using the raw labeled data
##########################################################################################
library(tm)
rawLabeledCorpus = Corpus(VectorSource(rawLabeled.df$cleanedText))
save(rawLabeledCorpus, file = "data/bow-april24/BOW_wordCorpus_all_april24.RData")

##Construct a term frequency matrix
# tf matrix does not use the stop words or numbers
# a list of stop words can be found at
# http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
stopWords = readLines("http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")

#tf = DocumentTermMatrix(rawLabeledCorpus, control = list(stopwords = stopwords("english"), removeNumbers = TRUE))

# Only include words that occur in at least 1% of the data
#tf = removeSparseTerms(tf, 0.99)

# Convert to dense matrix for analysis
#tf = as.matrix(tf)
#save(tf, file = "data/bow-april24/BOW_tf_all_april24.RData")

##Construct a word frequency data frame of the 1% most-used words in the training data
# Sum up the columns to find the occurrences of each word in the corpus
#wordFreq = colSums(tf)
#wordFreqDF = data.frame(word = colnames(tf), freq = wordFreq)
#rownames(wordFreqDF) = NULL

##########################################################################################
##Write a function to get the word frequency data frame
##########################################################################################
getWordFreqDF = function(DocumentVec, sparsity, stopwords){
  # Construct corpus
  tempCorpus = Corpus(VectorSource(DocumentVec))

  # Construct TF matrix and remove sparse terms
  tempTF = DocumentTermMatrix(tempCorpus,
                              control = list(stopwords = stopwords("english"),
                                             removeNumbers = TRUE))
  tempTF = removeSparseTerms(tempTF, sparsity)
  tempTF = as.matrix(tempTF)

  freqWord = colSums(tempTF)
  freqWordDF = data.frame(word = names(freqWord), freq = freqWord)
  rownames(freqWordDF) = NULL

  return(freqWordDF)
}

getPosWordFreqDF = function(DocumentVec, sparsity, stopwords){
  # Construct corpus
  tempCorpus = Corpus(VectorSource(DocumentVec))
  
  save(tempCorpus, file = "data/bow-april24/BOW_posCorpus_all_april24.RData")
  
  # Construct TF matrix and remove sparse terms
  tempTF = DocumentTermMatrix(tempCorpus,
                              control = list(stopwords = stopwords("english"),
                                             removeNumbers = TRUE))
  tempTF = removeSparseTerms(tempTF, sparsity)
  tempTF = as.matrix(tempTF)
  
  save(tempTF, file = "data/bow-april24/BOW_posTF_all_april24.RData")
  
  freqWord = colSums(tempTF)
  freqWordDF = data.frame(word = names(freqWord), freq = freqWord)
  rownames(freqWordDF) = NULL
  
  return(freqWordDF)
}


getNegWordFreqDF = function(DocumentVec, sparsity, stopwords){
  # Construct corpus
  tempCorpus = Corpus(VectorSource(DocumentVec))
  
  save(tempCorpus, file = "data/bow-april24/BOW_negCorpus_all_april24.RData")
  
  # Construct TF matrix and remove sparse terms
  tempTF = DocumentTermMatrix(tempCorpus,
                              control = list(stopwords = stopwords("english"),
                                             removeNumbers = TRUE))
  tempTF = removeSparseTerms(tempTF, sparsity)
  tempTF = as.matrix(tempTF)
  
  save(tempTF, file = "data/bow-april24/BOW_negTF_all_april24.RData")
  
  freqWord = colSums(tempTF)
  freqWordDF = data.frame(word = names(freqWord), freq = freqWord)
  rownames(freqWordDF) = NULL
  
  return(freqWordDF)
}


##########################################################################################
##Get the word frequency data frame for both the training positive and negative Tweets
##########################################################################################
wordFreqDF.pos = getPosWordFreqDF(DocumentVec = rawLabeled.df[rawLabeled.df$Sentiment == 1, "cleanedText"], sparsity = 0.99, stopwords = stopWords)
 
wordFreqDF.neg = getNegWordFreqDF(DocumentVec = rawLabeled.df[rawLabeled.df$Sentiment == 0, "cleanedText"], sparsity = 0.99, stopwords = stopWords)

save(wordFreqDF.pos, file = "data/bow-april24/BOW_posWordFreq_all_april24.RData")
save(wordFreqDF.neg, file = "data/bow-april24/BOW_negWordFreq_all_april24.RData")
# 
# ##Merge the two data frames
wordFreqDF.all = merge(wordFreqDF.pos, wordFreqDF.neg, by = "word", all = TRUE)

# 
# # Set the column names and set the NAs to 0s.
colnames(wordFreqDF.all) = c("word", "freqPos", "freqNeg")
wordFreqDF.all$freqPos[is.na(wordFreqDF.all$freqPos)] = 0
wordFreqDF.all$freqNeg[is.na(wordFreqDF.all$freqNeg)] = 0
# 
# # Compute the difference in postive and negative frequency
wordFreqDF.all$diff = abs(wordFreqDF.all$freqPos - wordFreqDF.all$freqNeg)
save(wordFreqDF.all, file = "data/bow-april24/BOW_allWordFreq_all_april24.RData")
#save(wordFreqDF.all$diff, file = "data/bow-april24/BOW_allWordFreqDiffs_all_april24.RData")

######################################################
# April 24 - do on all data (that is not -1)
######################################################
# 
# wordFreqDF.pos = getWordFreqDF(DocumentVec = trainRaw.df[trainRaw.df$Sentiment == 1, "cleanedText"], sparsity = 0.99, stopwords = stopWords)
# 
# wordFreqDF.neg = getWordFreqDF(DocumentVec = trainRaw.df[trainRaw.df$Sentiment == 0, "cleanedText"], sparsity = 0.99, stopwords = stopWords)
# 
# save(wordFreqDF.pos, file = "data/bow-all-data/BOW_trainPosWords_all_april21.RData")
# save(wordFreqDF.neg, file = "data/bow-all-data/BOW_trainNegWords_all_april21.RData")
# 
# ##Merge the two data frames
# wordFreqDF.all = merge(wordFreqDF.pos, wordFreqDF.neg, by = "word", all = TRUE)
# 
# # Set the column names and set the NAs to 0s.
# colnames(wordFreqDF.all) = c("word", "freqPos", "freqNeg")
# wordFreqDF.all$freqPos[is.na(wordFreqDF.all$freqPos)] = 0
# wordFreqDF.all$freqNeg[is.na(wordFreqDF.all$freqNeg)] = 0
# 
# # Compute the difference in postive and negative frequency
# wordFreqDF.all$diff = abs(wordFreqDF.all$freqPos - wordFreqDF.all$freqNeg)


##########################################################################################
### Get word count of top N words:
##########################################################################################

library(stringr)
getPattern = function(data, sub.index, char.col, pattern, pat.name){
  sub.data = data.frame(data[sub.index, ]) #subset the data frame.
  char.vec = as.character(sub.data[ , char.col]) #get the character strings.
  matched.list = apply(X = as.matrix(char.vec), MARGIN = 1,
                       FUN = str_extract_all, pattern = pattern)
  #list of matched pattern(s) for each character string.

  sub.data[ , ncol(sub.data) + 1] = rep(NA, times = nrow(sub.data))
  sub.data[ , ncol(sub.data) + 1] = rep(0, times = nrow(sub.data))
  #add two columns.

  n.col = ncol(sub.data) #number of columns in the augmented data frame.

  colnames(sub.data)[n.col - 1] = pat.name
  colnames(sub.data)[n.col] = paste(pat.name, "count")
  #change the names of the additional two columns.

  for (i in 1:nrow(sub.data)){
    num.matched = length(matched.list[[i]][[1]])
    if (num.matched > 0){
      sub.data[i, n.col - 1] = matched.list[i]
      sub.data[i, n.col] = num.matched
      #if there is at least one match (num.matched > 0),
      #record the list of the matched patterns and the number of matches.
    }
  }
  return(sub.data)
}

##########################################################################################
# Find word count - word importance
##########################################################################################

# source("sample-r/getPattern.R")

## Trial - 1k train df
# sample 1k indices
#tmpIndx <- sample(1:nrow(trainRaw.df), size = 1000, replace = FALSE)
#length(unique(tmpIndx))
## 1000
#train_1k_april24.df <- trainRaw.df[tmpIndx, ]


#getPattern_april24 = function(data, sub.index, char.col, pattern, pat.name, curr.indx) {
  
#  browser() 
  
#   sub.data = data.frame(data[sub.index, ]) #subset the data frame.
#   char.vec = as.character(sub.data[ , char.col]) #get the character strings.
#   matched.list = apply(X = as.matrix(char.vec), MARGIN = 1,
#                        FUN = str_extract_all, pattern = pattern)
#   #list of matched pattern(s) for each character string.
#   
#   sub.data[ , ncol(sub.data) + 1] = rep(NA, times = nrow(sub.data))
#   sub.data[ , ncol(sub.data) + 1] = rep(0, times = nrow(sub.data))
#   #add two columns.
#   
#   n.col = ncol(sub.data) + curr.indx - 1 #number of columns in the augmented data frame.
#   
#   colnames(sub.data)[n.col - 1] = pat.name
#   colnames(sub.data)[n.col] = paste(pat.name, "count")
#   #change the names of the additional two columns.
#   
#   for (i in 1:nrow(sub.data)){
#     num.matched = length(matched.list[[i]][[1]])
#     if (num.matched > 0){
#       sub.data[i, n.col - 1] = matched.list[i]
#       sub.data[i, n.col] = num.matched
#       #if there is at least one match (num.matched > 0),
#       #record the list of the matched patterns and the number of matches.
#     }
#   }
#   return(sub.data)
# }

## Trial - find important words
#n = 50
#importantWords_april24 = wordFreqDF.all$word[order(wordFreqDF.all$diff, decreasing = TRUE)[1:n]]
#importantWords_april24 = as.character(importantWords_april24)

#for (i in 1:n) {
#    print(list("curr i: ", i))
#    trainClean_april24.df = getPattern_april24(data = train_1k_april24.df, sub.index = 1:nrow(train_1k_april24.df), char.col = 5,
#                       pattern = importantWords_april24[i], pat.name = importantWords_april24[i], curr.indx = i)
#  #testClean_april24.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
#  #                      pattern = importantWords_april24[i], pat.name = importantWords_april24[i])
#}

#
#
#

#for (i in 1:n) {
#  train_1k_april24.df = getPattern(data = train_1k_april24.df, sub.index = 1:nrow(train_1k_april24.df), char.col = 5,
#                                             pattern = importantWords_april24[i], pat.name = importantWords_april24[i])
  #testClean_april24.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
  #                      pattern = importantWords_april24[i], pat.name = importantWords_april24[i])
#}

#
#
#

##Find the word count of the 50 "important" words in the training and testing set.
#n = 50
#importantWords = wordFreqDF.all$word[order(wordFreqDF.all$diff, decreasing = TRUE)[1:n]]
#importantWords = as.character(importantWords)

#for (i in 1:n) {
#  trainRaw.df = getPattern(data = trainRaw.df, sub.index = 1:nrow(trainRaw.df), char.col = 5,
#                       pattern = importantWords[i], pat.name = importantWords[i])
#  testRaw.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
#                      pattern = importantWords[i], pat.name = importantWords[i])
#}

patternLabeled.df <- rawLabeled.df
n <- 50
importantWords = wordFreqDF.all$word[order(wordFreqDF.all$diff, decreasing = TRUE)[1:n]]
importantWords = as.character(importantWords)
for (i in 1:n) {
  print(list("Current in loop: ", i))
  patternLabeled.df = getPattern(data = patternLabeled.df, sub.index = 1:nrow(patternLabeled.df), char.col = 5,
                           pattern = importantWords[i], pat.name = importantWords[i])
  #testRaw.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
  #                        pattern = importantWords[i], pat.name = importantWords[i])
}


#
# April 25 - redo last set of words because of power outage on campus 
#

n <- 50
importantWords = wordFreqDF.all$word[order(wordFreqDF.all$diff, decreasing = TRUE)[1:n]]
importantWords = as.character(importantWords)
for (i in 47:n) {
  print(list("Current in loop: ", i))
  patternLabeled.df = getPattern(data = patternLabeled.df, sub.index = 1:nrow(patternLabeled.df), char.col = 5,
                                 pattern = importantWords[i], pat.name = importantWords[i])
  #testRaw.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
  #                        pattern = importantWords[i], pat.name = importantWords[i])
}

#
#
# April 25 - power outage - restarting but already partially in loop 
#


##########################################################################################
# Clean data and save as RData
##########################################################################################

# April 24 - 1k sample clean up
#trainCleanedDF_1k = data.frame(Sentiment = train_1k_april24.df$Sentiment,
#                               train_1k_april24.df[ , seq(from = 7, to = ncol(train_1k_april24.df), by = 2)])
#BOW_trainCleanedDF_1k = trainCleanedDF_1k

#
#
#

# Clean up
patternLabeled.clean = data.frame(Sentiment = patternLabeled.df$Sentiment,
                            patternLabeled.df[ , seq(from = 7, to = ncol(patternLabeled.df), by = 2)])
BOW_patternLabeled_clean = patternLabeled.clean

#testCleanedDF = data.frame(Sentiment = testRaw.df$Sentiment,
#                           testRaw.df[ , seq(from = 7, to = ncol(testRaw.df), by = 2)])
#BOW_testCleanedDF = testCleanedDF

# Save those two data frames for analysis
#save(BOW_trainCleanedDF, file = "data/bow-all-data/BOW_trainCleanDF_all.RData")
#save(BOW_testCleanedDF, file = "data/bow-all-data/BOW_testCleanDF_all.RData")

# Add data to .RData files
save(BOW_patternLabeled_clean, file = "data/bow-april24/BOW_allCleanData_april24.RData")
#save(BOW_testCleanedDF, file = "data/bow-all-data/BOW_testClean_all_april21.RData")


##########################################################################################
### Trial randomForest run - 100 and 1k trees
##########################################################################################

## April24 - Trial randomForest on 1k 55 features 

# do sample 80 / 20 split with 1k 
#features55_indx <- sample(1:nrow(total_features_1k), size = nrow(total_features_1k) * 0.8, replace = FALSE)
#train1k_55features <- total_features_1k[features55_indx, ]
#test1k_55features <- total_features_1k[-features55_indx, ]

#naIndx <- train1k_55features[!complete.cases(train1k_55features), ]

#train1k_na <-

#set.seed(1)
#features55.rfModel1k = randomForest(x = as.matrix(train1k_55features[, -1]), y = as.factor(train1k_55features[, 1]), mtry = 10, ntree = 1000)

#features55.rfModel1k.myPredict = predict(features55.rfModel1k, as.matrix(test1k_55features[ , -1]))
#features55.rfModel1k.accur = sum(features55.rfModel1k.myPredict == as.factor(test1k_55features[ , 1])) / length(test1k_55features[ , 1])

### Variable Importance Plot
#varImpPlot(features55.rfModel1k)

#
#
#

##########################################################################################
### Add username and hashtag scores to create 55 feature dataset 
##########################################################################################

# X.trainRaw.df <- XallScores[(trainIndex), ]
# X.trainRaw.df
total_55features_allData <- cbind(BOW_patternLabeled_clean, XallScores)
save(total_55features_allData, file = "data/bow-april24/BOW_total_55features_allData_april24.RData")

##########################################################################################
### Try randomForest on 80 / 20 split 
##########################################################################################
set.seed(1)
# do sample 80 / 20 split with 1k 
features55_indx <- sample(1:nrow(total_55features_allData), size = nrow(total_55features_allData) * 0.8, replace = FALSE)
train_55features <- total_55features_allData[features55_indx, ]
test_55features <- total_55features_allData[-features55_indx, ]

save(train_55features, file = "data/bow-april24/RF_train_55features_april24.RData")
save(test_55features, file = "data/bow-april24/RF_test_55features_april24.RData")

##########################################################################################
### April 25 - randomForest did not work...
##########################################################################################

# April 25 - randomForest did not work...
# error: " long vectors (argument 24) are not supported in .Fortran " 
#
load("data/bow-april24/RF_train_55features_april24.RData")
load("data/bow-april24/RF_test_55features_april24.RData")
trialRF <- randomForest(x = train_55features[, -1], y = as.factor(train_55features[, 1]), mtry = 10, ntree = 2, do.trace = TRUE)
# 
##########################################################################################
#

allData.rfModel100 = randomForest(x = train_55features[ , -1], y = as.factor(train_55features[ , 1]), mtry = 10, ntree = 100, do.trace = TRUE)

allData.rfModel100.myPredict = predict(allData.rfModel100, test_55features[ , -1])
allData.rfModel100.accur = sum(allData.rfModel100.myPredict == as.factor(test_55features[ , 1])) / length(test_55features[ , 1])

#
save(allData.rfModel100, file = "data/bow-april24/RF_100tree_model.rda")
#

allData.rfModelAll = randomForest(x = train_55features[ , -1], y = as.factor(train_55features[ , 1]), mtry = 10, ntree = 1000, do.trace = TRUE)

allData.rfModelAll.myPredict = predict(allData.rfModelAll, test_55features[ , -1])
allData.rfModelAll.accur = sum(allData.rfModelAll.myPredict == as.factor(test_55features[ , 1])) / length(test_55features[ , 1])

### Variable Importance Plot
#varImpPlot(allData.rfModelAll)

##########
### Trial randomForest run - 1000 trees

set.seed(1)
allData.rfModel1k = randomForest(x = as.matrix(BOW_trainCleanedDF[ , -1]), y = as.factor(BOW_trainCleanedDF[ , 1]), mtry = 10, ntree = 1000)

allData.rfModel1k.myPredict = predict(allData.rfModel1k, as.matrix(BOW_testCleanedDF[ , -1]))
allData.rfModel1k.accur = sum(allData.rfModel1k.myPredict == as.factor(BOW_testCleanedDF[ , 1])) / length(BOW_testCleanedDF[ , 1])

### Variable Importance Plot
varImpPlot(allData.rfModel1k)
