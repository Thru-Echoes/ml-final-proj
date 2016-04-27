load("data/raw.RData")
# raw.data contains all the data (-1, 0, and 1)
# the unscored ones are labeled -1

load("data/BOW_allWordFreq_all_april24.RData")
# the first column contains the 118 words that appear in at least 1% of the tweets

testAll = raw.data
require(tm)
require(stringr)
# Clean the whole raw dataset, remove punctuation and set everything to lower case.
testAll$cleanedText = tolower(gsub("[[:punct:]]", "", testAll$SentimentText))
save(testAll, file = "April26_rawData_Cleaned.rda")

wordsToUSe = as.character(wordFreqDF.all$word)
#tf1 = t(apply(t(testAll$cleanedText), 2, function(x) str_count(x, wordsToUSe)))
# tf1 counts cant as both can and cant.

wordsToUse.matrix = as.matrix(wordsToUSe)
wordsToUse_WB = apply(wordsToUse.matrix, 1, FUN = function(x) paste("\\b", x, "\\b", sep = ""))

more.words <- c("\\b[a-z]*fuck[a-z]*\\b", "\\bshit\\b", "\\bbullshit\\b", "\\bdamn\\b", "\\bbitch\\b", "\\bcrap\\b", "\\bpiss\\b", "\\bdick\\b", "\\bdarn\\b", "\\bcock\\b", "\\bpussy\\b", "\\basshole\\b", "\\bfag\\b", "\\bfaggit\\b", "\\bfagit\\b", "\\bbastard\\b", "\\bslut\\b", "\\bdouche\\b", "\\bcunt\\b", "\\bl8r\\b", "\\bgr8\\b", "\\bimho\\b", "\\bimo\\b", "\\bily\\b", "\\bsol\\b", "\\bthx\\b", "\\bthnx\\b", "\\brotflmao\\b", "\\bnp\\b", "\\bomg\\b", "\\blmao\\b", "\\btmi\\b", "\\bwtf\\b", "\\bxoxo\\b", "\\bttyl\\b", "\\bjk\\b", "\\brofl\\b", "\\brotfl\\b", "\\bbff\\b", "\\bfyi\\b", "\\bdiy\\b", "\\bidk\\b", "\\bftw\\b", "\\brsvp\\b", "\\btgif\\b", "\\bwth\\b")

finalWordsToUse = c(wordsToUse_WB, more.words)
save(finalWordsToUse, file = "April26_wordsToUse.rda")

tf2 = t(apply(t(testAll$cleanedText), 2, function(x) str_count(x, finalWordsToUse)))
save(tf2, file = "April26_tfMatrix_AllRaw.rda")

idf = log(nrow(tf2) / colSums(sign(tf2)))
idf[is.infinite(idf)] = 0

tf_idf = t(apply(tf2, 1, function(x) x * idf))
save(tf_idf, file = "April26_tf_idfMatrix_AllRaw.rda")
