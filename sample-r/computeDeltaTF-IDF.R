##Load in a term frequency matrix (with sentiment scores as the first column).
##Compute the delta TF-IDF matrix.

load("data/tf_164words_allRawData.rda")
TFMatrix = tf

posIndex = TFMatrix[, "Sentiment"] == 1
negIndex = TFMatrix[, "Sentiment"] == 0


deltaIDF = log2(length(posIndex) / colSums(sign(TFMatrix[posIndex, -1]))) - 
  log2(length(negIndex) / colSums(sign(TFMatrix[negIndex, -1])))
deltaIDF[is.infinite(deltaIDF)] = 0
deltaIDF[is.nan(deltaIDF)] = 0

deltaTF_IDF = t(apply(TFMatrix[, -1], 1, function(x) x * deltaIDF))
save(deltaTF_IDF, file = "deltaTFIDF_164words_allRawData.rda")

#### 

load("data/deltaTF_IDF_AllFeatures.rda")
load("data/deltaTF_IDF_ImpFeatures.rda")
load("data/April26_wordFreq_relativePos_vs_neg_maxDiff.rda")

relativeSentiDiff <- wordFreq.relative[, 6]

sentiDiff_TF = t(apply(TFMatrix[, 2:119], 1, function(x) x * relativeSentiDiff))
save(sentiDiff_TF, file = "data/relativeSentiDiff_164words.rda")

# negative word (relative freq) has positive value 
# positive word (relative freq) has negative value 


