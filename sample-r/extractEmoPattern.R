# April 18: use getPattern.R function to extract emoticons from positive and negative tweets
#
## Positive (from online): <code>((:|;|8)+(-)*(\)|D|P|p)+)|((\()+(-)*(:|;)+)</code>
#
## Negative (from online): <code>((:)+(-|')*(\()+)|((\))+(-)*(:|;)+)</code>
###############################################################################################
### Positive Emoticon Patterns:
posPattern1 <- "[:;][-]?[)D]+|[(]+[-]?[:;]"
posPattern2 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]"
posPattern3 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]|[\\^][_\\.\\*-]*[\\^]|[-][_\\.]*[-]"

### Negative Emoticon Patterns:
#negPattern1 <- "[:][-']*[(]+|[)]+[-']*[:]"
negPattern2 <- "[>]*[:8][-']*[(]+|[)]+[-']*[:8][<]*"
#negPattern3 <- "[>]*[:8][-']*[(]+|[)]+[-']*[:8][<]*"

###############################################################################################
# Code
###############################################################################################
### Positive Emoticons + Positive Tweets:

posEmo_posTweets_1 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = posPattern1, pat.name = "positive emoticon")
posEmo_posTweets_2 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = posPattern2, pat.name = "positive emoticon")
posEmo_posTweets_3 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = posPattern3, pat.name = "positive emoticon")

### Negative Emoticons + Positive Tweets:

#negEmo_posTweets_1 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = negPattern1, pat.name = "negative emoticon")
negEmo_posTweets_2 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = negPattern2, pat.name = "negative emoticon")

### Positive Emoticons + Negative Tweets:

posEmo_negTweets_1 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = posPattern1, pat.name = "positive emoticon")
posEmo_negTweets_2 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = posPattern2, pat.name = "positive emoticon")
posEmo_negTweets_3 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = posPattern3, pat.name = "positive emoticon")

### Negative Emoticons + Negative Tweets:

#negEmo_negTweets_1 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = negPattern1, pat.name = "negative emoticon")
negEmo_negTweets_2 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = negPattern2, pat.name = "negative emoticon")

###############################################################################################
# Distributions of Emoticons (see images in plots-images/ directory)
###############################################################################################
## Positive Emoticon x Positive Tweets
#
#### Pattern 1
summary(as.factor(posEmo_posTweets_1[, 6]))

#### Pattern 2
summary(as.factor(posEmo_posTweets_2[, 6]))

#### Pattern 3
summary(as.factor(posEmo_posTweets_3[, 6]))

#############################
## Negative Emoticon x Positive Tweets
#
#### Only used 1 negative emoticon pattern (see above)
summary(as.factor(negEmo_posTweets_2[, 6]))

#############################
## Positive Emoticon x Negative Tweets
#
#### Pattern 1
summary(as.factor(negEmo_posTweets_1[, 6]))

#### Pattern 2
summary(as.factor(negEmo_posTweets_2[, 6]))

#### Pattern 3
summary(as.factor(negEmo_posTweets_3[, 6]))

#############################
## Negative Emoticon x Negative Tweets
#
#### Only used 1 negative emoticon pattern (see above)
summary(as.factor(negEmo_negTweets_2[, 6]))
