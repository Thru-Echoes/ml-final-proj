##Load in a term frequency matrix (with sentiment scores as the first column).
##Compute the delta TF-IDF matrix.

load("tf_164words_allRawData.rda")
TFMatrix = tf

posIndex = TFMatrix[, "Sentiment"] == 1
negIndex = TFMatrix[, "Sentiment"] == 0


deltaIDF = log2(length(posIndex) / colSums(sign(TFMatrix[posIndex, -1]))) - 
  log2(length(negIndex) / colSums(sign(TFMatrix[negIndex, -1])))
deltaIDF[is.infinite(deltaIDF)] = 0
deltaIDF[is.nan(deltaIDF)] = 0

deltaTF_IDF = t(apply(TFMatrix[, -1], 1, function(x) x * deltaIDF))
save(deltaTF_IDF, file = "deltaTFIDF_164words_allRawData.rda")
