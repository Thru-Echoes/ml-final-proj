# April 18: use getPattern.R function to extract emoticons from positive and negative tweets
#
## Positive (from online): <code>((:|;|8)+(-)*(\)|D|P|p)+)|((\()+(-)*(:|;)+)</code>
#
## Negative (from online): <code>((:)+(-|')*(\()+)|((\))+(-)*(:|;)+)</code>
###############################################################################################
### Positive Emoticon Patterns:
posPattern1 <- "[:;][-]?[)D]+|[(]+[-]?[:;]"
posPattern2 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]"
posPattern3 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]|[^][_.\\*-]*[^]|[-][_.]*[-]"

### Negative Emoticon Patterns:
negPattern1 <- ""
negPattern2 <- ""

###############################################################################################
# Code
###############################################################################################
### Positive Emoticons + Positive Tweets:

posEmo_posTweets_1 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = posPattern1, pat.name = "positive emoticon")
posEmo_posTweets_2 <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = posPattern2, pat.name = "positive emoticon")

### Negative Emoticons + Positive Tweets:

### Positive Emoticons + Negative Tweets:

### Negative Emoticons + Negative Tweets:
