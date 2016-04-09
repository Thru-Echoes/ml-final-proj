score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'none') {

    require(plyr)
    require(stringr)

    # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
    # we want a simple array of scores back, so we use "l" + "a" + "ply" <- laply:
    scores <- laply(sentences, function(sentence, pos.words, neg.words) {

        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <- gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence <- tolower(sentence)

        # split into words. str_split is in the stringr package
        word.list <- str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words <- unlist(word.list)

        # compare our words to the dictionaries of positive & negative terms
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)

        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)

        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score <- sum(pos.matches) - sum(neg.matches)

        return(score)

    }, pos.words, neg.words, .progress = .progress)

    scores.df <- data.frame(score = scores, text = sentences)
    return(scores.df)
}

######################################################
##################### Load in data ###################
######################################################
load("data/raw.RData")
fromKaggle <- raw.data[raw.data$SentimentSource == "Kaggle", ]
nKaggle <- nrows(fromKaggle)
# = 1349 total

######################################################
############## Get Sentiment140 (Source) #############
######################################################
s140 <- raw.data[raw.data$SentimentSource == "Sentiment140", ]
summary(as.factor(s140$Sentiment))
s140.neg <- s140[s140$Sentiment == 0, ]
s140.pos <- s140[s140$Sentiment == 1, ]

# How many from s140$Sentiment = -1, 1, 0?
#
# -1 (no score) = 49959
# 0 (negative score) = 762896
# 1 (positive score) = 764423
#
# roughly: number no score = 50k
# roughly: number negative score = 760k
# roughly: number positive score = 760k

######################################################
############### Get random 50k Indices ###############
######################################################
set.seed(1324)
ind50k <- sample(1:762896, 50000, replace = FALSE)
s140.neg50 <- s140.neg[ind50k, ]

indPos50k <- sample(1:764423, 50000, replace = FALSE)
s140.pos50 <- s140.pos[indPos50k, ]

neg50k <- s140.neg50[, 4]
pos50k <- s140.pos50[, 4]

neg50k <- as.character(neg50k)
pos50k <- as.character(pos50k)

######################################################
########### Do Sentiment Analysis ####################
######################################################
#this positive and negative words are related to abortion
pos <- scan('../data/april08-positive-words.txt', what = 'character', comment.char = ';')
neg <- scan('../data/april08-negative-words.txt', what = 'character', comment.char = ';')


# How to call this function (above):
# analysis <- score.sentiment(df$SentimentText, pos, neg, .progress = 'none')
posAnalysis <- score.sentiment(pos50k, pos, neg, .progress = 'none')
table(posAnalysis$score)
mean(posAnalysis$score)
median(posAnalysis$score)
hist(posAnalysis$score)

negAnalysis <- score.sentiment(neg50k, pos, neg, .progress = 'none')
table(negAnalysis$score)
mean(negAnalysis$score)
median(negAnalysis$score)
hist(negAnalysis$score)
