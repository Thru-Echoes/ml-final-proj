# Extract unique usernames and hashtags
#
## 1. Find unique occurrences for positive and negative tweets
###############################################################################################
load("data/raw.RData")
RawNeg.df = raw.data[raw.data$Sentiment == 0, ]
RawPos.df = raw.data[raw.data$Sentiment == 1, ]
RawNegTweet = as.character(RawNeg.df[ , 4])
RawPosTweet = as.character(RawPos.df[ , 4])

extractNeg <- data.frame(RawNegTweet)
# dim = 763500 tweets
allExtractNeg <- extractNeg %>% mutate(atMatches = str_extract_all(RawNegTweet, "(\\B@[[:alnum:]_]+)"), hashMatches = str_extract_all(RawNegTweet, "(\\B#[[:alnum:]_]+)"))

extractPos <- data.frame(RawPosTweet)
# dim = 765127 tweets
allExtractPos <- extractPos %>% mutate(atMatches = str_extract_all(RawPosTweet, "(\\B@[[:alnum:]_]+)"), hashMatches = str_extract_all(RawPosTweet, "(\\B#[[:alnum:]_]+)"))


## 2. See all negative and all positive tweets with columns "atMatches" and "hashMatches"
###############################################################################################
head(allExtractNeg)
head(allExtractPos)

###############################################################################################
###############################################################################################
###############################################################################################

## NOTE!
## unique-neg-occur & unique-pos-occur have issues...
## both appear as 5 column rows but should be single column (like unique user files)

## 3. Have data for unique usernames for positive and negative tweets
## stored in .txt files in data/ directory...
###############################################################################################
#negUser <- read.table(file.choose())
negUser <- read.table("data/unique-neg-users")
negOccur <- read.table("data/unique-neg-occur")
posUser <- read.table("data/unique-pos-users")
posOccur <- read.table("data/unique-pos-occur")

## 4. Check file in R with this:
###############################################################################################
cat(readLines("data/unique-neg-occur"), sep = " ")


## 5. Create data frame to combine negative / positive username + occurrences
###############################################################################################
negDF <- data.frame(user = negUser, occur = negOccur)
posDF <- data.frame(user = posUser, occur = posOccur)

## 6. Order DFs by descending occurrence
###############################################################################################
