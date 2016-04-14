#setwd("/Users/Chris/Desktop/Berkeley/Academic Semesters/2016 Spring/Stat154_Machine Learning/Final Project/Data") #Set the directory.
#load("raw.RData") 
load("data/raw.RData")

RawPos.df = raw.data[raw.data$Sentiment == 1, ] 
#Put all the positive tweets into one data frame.

RawNeg.df = raw.data[raw.data$Sentiment == 0, ]
#Put all the negative tweets into one data frame.

RawPosTweet = as.character(RawPos.df[ , 4])
RawNegTweet = as.character(RawNeg.df[ , 4])
#Get the character vector of all the postive tweets.

library(stringr) 
#The function str_extract_all extracts all the matched pattern in a character string.

PosTweet.OneString = paste(RawPosTweet, sep = " ", collapse = "")
NegTweet.OneString = paste(RawNegTweet, sep = " ", collapse = "")
#Collapse the vector RawPosTweet into one string, i.e., a character vector of legnth one. Separate using a space.

#posTest = str_extract_all(string = PosTweet.OneString, pattern = "@[[:alnum:]_]+")
#negTest = str_extract_all(string = NegTweet.OneString, pattern = "@[[:alnum:]_]+")
#The list test contains all the extracted usernames (duplicate possible).

posTest = str_extract_all(string = PosTweet.OneString, pattern = "(\\B@[[:alnum:]_]+)")
negTest = str_extract_all(string = NegTweet.OneString, pattern = "(\\B@[[:alnum:]_]+)")

negTest.vec = negTest[[1]] #Convert test into a character vector.
uniqueNegUsers = unique(negTest.vec) #Find the unique usernames.
nNeg = length(uniqueNegUsers)
uniqueNegOccur = rep(NA, times = nNeg)

posTest.vec = posTest[[1]] #Convert test into a character vector.
uniquePosUsers = unique(posTest.vec) #Find the unique usernames.
nPos = length(uniquePosUsers)
uniquePosOccur = rep(NA, times = nPos)
#The vector UniqueOccurance will contain the frequency for each unique username.

for (i in 1:nNeg) {
  uniqueNegOccur[i] = sum(negTest.vec == uniqueNegUsers[i])
  print(i / nNeg) #Progress: done when it reaches 1.
}

for (i in 1:nPos) {
  uniquePosOccur[i] = sum(posTest.vec == uniquePosUsers[i])
  print(i / nPos) #Progress: done when it reaches 1.
}
