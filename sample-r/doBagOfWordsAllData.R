# Do bag-of-words on all data 

#load("data/raw.RData")
labeledRaw.df = raw.data[raw.data$Sentiment != -1, ]

# Clean 
labeledRaw.df$cleanedText = tolower(gsub("[[:punct:]]", "", labeledRaw.df$SentimentText))

set.seed(1)
trainRatio = 0.8
trainIndex = sample(1:nrow(labeledRaw.df), size = nrow(labeledRaw.df)*trainRatio)

trainRaw.df = labeledRaw.df[trainIndex, ]
testRaw.df = labeledRaw.df[-trainIndex, ]

##Construct a corpus using the training data
library(tm)
#trainCorpus = Corpus(VectorSource(trainDF$cleanedText))

##Construct a term frequency matrix
# tf matrix does not use the stop words or numbers
# a list of stop words can be found at 
# http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
stopWords = readLines("http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")

#tf = DocumentTermMatrix(trainCorpus, control = list(stopwords = stopwords("english"), removeNumbers = TRUE))

# Only include words that occur in at least 1% of the data
#tf = removeSparseTerms(tf, 0.99)

# Convert to dense matrix for analysis
#tf = as.matrix(tf)

##Construct a word frequency data frame of the 1% most-used words in the training data
# Sum up the columns to find the occurrences of each word in the corpus
#wordFreq = colSums(tf)
#wordFreqDF = data.frame(word = colnames(tf), freq = wordFreq)
#rownames(wordFreqDF) = NULL

##Write a function to get the word frequency data frame
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

##Get the word frequency data frame for both the training positive and negative Tweets
wordFreqDF.pos = getWordFreqDF(DocumentVec = trainRaw.df[trainRaw.df$Sentiment == 1, "cleanedText"], sparsity = 0.99, stopwords = stopWords)

wordFreqDF.neg = getWordFreqDF(DocumentVec = trainRaw.df[trainRaw.df$Sentiment == 0, "cleanedText"], sparsity = 0.99, stopwords = stopWords)

save(wordFreqDF.pos, "BOW_trainPos_all.RData")
save(wordFreqDF.neg, "BOW_trainNeg_all.RData")

##Merge the two data frames
wordFreqDF.all = merge(wordFreqDF.pos, wordFreqDF.neg, by = "word", all = TRUE)

# Set the column names and set the NAs to 0s.
colnames(wordFreqDF.all) = c("word", "freqPos", "freqNeg")
wordFreqDF.all$freqPos[is.na(wordFreqDF.all$freqPos)] = 0
wordFreqDF.all$freqNeg[is.na(wordFreqDF.all$freqNeg)] = 0

# Compute the difference in postive and negative frequency
wordFreqDF.all$diff = abs(wordFreqDF.all$freqPos - wordFreqDF.all$freqNeg)


### Get word count of top N words: 
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

##Find the word count of the 50 "important" words in the training and testing set.
n = 50
importantWords = wordFreqDF.all$word[order(wordFreqDF.all$diff, decreasing = TRUE)[1:n]]
importantWords = as.character(importantWords)

for (i in 1:n) {
  trainClean.df = getPattern(data = trainRaw.df, sub.index = 1:nrow(trainRaw.df), char.col = 5,
                       pattern = importantWords[i], pat.name = importantWords[i])
  testClean.df = getPattern(data = testRaw.df, sub.index = 1:nrow(testRaw.df), char.col = 5,
                      pattern = importantWords[i], pat.name = importantWords[i])
}

# Clean up 
trainCleanedDF = data.frame(Sentiment = trainClean.df$Sentiment, 
                            trainClean.df[ , seq(from = 7, to = ncol(trainClean.df), by = 2)])
BOW_trainCleanedDF = trainCleanedDF

testCleanedDF = data.frame(Sentiment = testClean.df$Sentiment, 
                           testClean.df[ , seq(from = 7, to = ncol(testClean.df), by = 2)])
BOW_testCleanedDF = testCleanedDF

# Save those two data frames for analysis
save(BOW_trainCleanedDF, file = "data/BOW_trainCleanDF_all.RData")
save(BOW_testCleanedDF, file = "data/BOW_testCleanDF_all.RData")


##########################################################################################
### Trial randomForest run - 100 trees 

set.seed(1)
allData.rfModel100 = randomForest(x = as.matrix(BOW_trainCleanedDF[ , -1]), y = as.factor(BOW_trainCleanedDF[ , 1]), mtry = 10, ntree = 100)

allData.rfModel100.myPredict = predict(allData.rfModel100, as.matrix(BOW_testCleanedDF[ , -1]))
allData.rfModel100.accur = sum(allData.rfModel100.myPredict == as.factor(BOW_testCleanedDF[ , 1])) / length(BOW_testCleanedDF[ , 1])

##########################################################################################
### Trial randomForest run - 1000 trees 

set.seed(1)
allData.rfModel1k = randomForest(x = as.matrix(BOW_trainCleanedDF[ , -1]), y = as.factor(BOW_trainCleanedDF[ , 1]), mtry = 10, ntree = 1000)

allData.rfModel1k.myPredict = predict(allData.rfModel1k, as.matrix(BOW_testCleanedDF[ , -1]))
allData.rfModel1k.accur = sum(allData.rfModel1k.myPredict == as.factor(BOW_testCleanedDF[ , 1])) / length(BOW_testCleanedDF[ , 1])
