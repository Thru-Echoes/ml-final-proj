suppressMessages(library(Matrix))
load("TrainTest.RData")
Xsample <- X
ysample <- y
rm(X)
rm(y)
vocab <- scan("vocab.txt", what="character", sep="\n", encoding="latin1")
X <- readMM("X.mtx")
y <- strtoi(scan("y.txt", what="integer", sep="\n", encoding="UTF-8"))
raw.data <- read.csv("MaskedDataRawFixed.csv", header=TRUE)
raw.data$SentimentText <- as.character(raw.data$SentimentText)
tweet.lengths <- nchar(raw.data$SentimentText)
X <- cbind(X, tweet.lengths)
