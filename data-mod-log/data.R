suppressMessages(library(dplyr))
suppressMessages(library(stringr))

vocab <- scan("../data/vocab.txt", what="character", sep="\n", encoding="latin1")

load("../data/raw.RData")
raw.data$SentimentText <- as.character(raw.data$SentimentText)

################################## TEST DATA ###################################
test.tweets <- data.frame(tweets=raw.data$SentimentText[1:50000])
test.tweets$tweets <- as.character(test.tweets$tweets)

usernames.pattern <- "(\\B@[[:alnum:]_]+)"
hashtags.pattern <- "(\\B#[[:alnum:]_]+)"

fuck.pattern <- "\\b[a-z]*fuck[a-z]*\\b"

laugh.pattern <- "\\b(?=lol)\\S*(\\S+)(?<=\\blol)\1*\\b"
laugh.pattern <- "\\b(?=haha)\\S*(\\S+)(?<=\\bhaha)\1*\\b"
laugh.pattern <- "\\b(?=omg)\\S*(\\S+)(?<=\\bomg)\1*\\b"

# Test Tweets Username and Hashtag Matches
test.tweets <- test.tweets %>%
  mutate(usernames=str_extract_all(tweets, usernames.pattern),
         hashtags=str_extract_all(tweets, hashtags.pattern),
         laughing=str_extract_all(tweets, laugh.pattern))

test.tweets <- test.tweets %>%
  mutate(fucks=str_extract_all(tweets, fuck.pattern))

head(test.tweets$fucks)

for (i in 307:50000) {
  if (length(test.tweets$fucks[[i]]) != 0) {
    print(test.tweets$tweets[[i]])
    print(test.tweets$fucks[[i]])
    print(i)
    break
  }
}

# Construct a feature that is bad word or no bad word?


more.words <- c("\\b[a-z]*fuck[a-z]*\\b", "\\bshit\\b", "\\bdamn\\b", "\\bbitch\\b", "\\bcrap\\b", "\\bpiss\\b", "\\bdick\\b", "\\bdarn\\b", "\\bcock\\b", "\\bpussy\\b", "\\basshole\\b", "\\bfag\\b", "\\bfaggit\\b", "\\bfagit\\b", "\\bbastard\\b", "\\bslut\\b", "\\bdouche\\b", "\\bcunt\\b", "\\bl8r\\b", "\\bgr8\\b", "\\bimho\\b", "\\bimo\\b", "\\bily\\b", "\\bsol\\b", "\\bthx\\b", "\\bthnx\\b", "\\brotflmao\\b", "\\bnp\\b", "\\bomg\\b", "\\blmao\\b", "\\btmi\\b", "\\bwtf\\b", "\\bxoxo\\b", "\\bttyl\\b", "\\bjk\\b", "\\brofl\\b", "\\brotfl\\b", "\\bbff\\b", "\\bfyi\\b", "\\bdiy\\b", "\\bidk\\b", "\\bftw\\b", "\\brsvp\\b", "\\btgif\\b", "\\bwth\\b")


# Or construct a feature that assigns a bad word score, given that it exists?

# probability that a tweet with a bad word is negative, given that it is a
# certain bad word.

# How do you construct the probability:

# 



str1 <- "fuckkkkk fucking muthafucka"
str2 <- "haha haha hahaha hahaaha"
str3 <- "omgggggg gomg"
str4 <- "Yoooooooo"
sample.df <- data.frame(tweets=c(str1, str2, str3, str4))

sample.df <- sample.df %>%
  mutate(fuck=str_extract_all(tweets, fuck.pattern))


# First Feature
X.test <- data.frame(tweet.lengths=nchar(test.tweets$tweets))
################################################################################

raw.data <- raw.data[50001:nrow(raw.data),]
Index <- 1:nrow(raw.data)
raw.data <- cbind(Index, raw.data)

# Feature #1
tweet.lengths <- nchar(raw.data$SentimentText)
X <- data.frame(tweet.lengths)

neg <- raw.data[raw.data$Sentiment == 0,]
pos <- raw.data[raw.data$Sentiment == 1,]
neg.tweets <- neg[,5]
pos.tweets <- pos[,5]
neg.tweets.df <- neg[,c(1, 5)]
pos.tweets.df <- pos[,c(1, 5)]

usernames.pattern <- "(\\B@[[:alnum:]_]+)"
hashtags.pattern <- "(\\B#[[:alnum:]_]+)"

# Negative Username and Hashtag Matches
neg.tweets.df <- neg.tweets.df %>%
  mutate(usernames=str_extract_all(neg.tweets, usernames.pattern),
         hashtags=str_extract_all(neg.tweets, hashtags.pattern))

# Negative Username Frequencies and Normalized Scores
neg.usernames <- as.data.frame(table(unlist(neg.tweets.df$usernames)))
colnames(neg.usernames)[1] <- "Username"
colnames(neg.usernames)[2] <- "Frequency"
neg.usernames <- neg.usernames %>%
  mutate(Score=Frequency/max(Frequency))

neg.usernames$Username <- as.character(neg.usernames$Username)

nrow(neg.usernames)

# Negative Hashtag Frequencies and Normalized Scores
neg.hashtags <- as.data.frame(table(unlist(neg.tweets.df$hashtags)))
colnames(neg.hashtags)[1] <- "Hashtag"
colnames(neg.hashtags)[2] <- "Frequency"
neg.hashtags <- neg.hashtags %>%
  mutate(Score=Frequency/max(Frequency))

neg.hashtags$Hashtag <- as.character(neg.hashtags$Hashtag)

nrow(neg.hashtags)

# Positive Username and Hashtag Matches
pos.tweets.df <- pos.tweets.df %>%
  mutate(usernames=str_extract_all(pos.tweets, usernames.pattern),
         hashtags=str_extract_all(pos.tweets, hashtags.pattern))

# Positive Username Frequencies and Normalized Scores
pos.usernames <- as.data.frame(table(unlist(pos.tweets.df$usernames)))
colnames(pos.usernames)[1] <- "Username"
colnames(pos.usernames)[2] <- "Frequency"
pos.usernames <- pos.usernames %>%
  mutate(Score=Frequency/max(Frequency))

pos.usernames$Username <- as.character(pos.usernames$Username)

nrow(pos.usernames)

# Positive Hashtag Frequencies and Normalized Scores
pos.hashtags <- as.data.frame(table(unlist(pos.tweets.df$hashtags)))
colnames(pos.hashtags)[1] <- "Hashtag"
colnames(pos.hashtags)[2] <- "Frequency"
pos.hashtags <- pos.hashtags %>%
  mutate(Score=Frequency/max(Frequency))

pos.hashtags$Hashtag <- as.character(pos.hashtags$Hashtag)

nrow(pos.hashtags)

# FEATURE CREATION

head(neg.tweets.df)



neg.usernames.subset <- neg.usernames %>%
  arrange(desc(Frequency)) %>%
  select(Username, Frequency) %>%
  head(50)

pos.usernames.subset <- pos.usernames %>%
  arrange(desc(Frequency)) %>%
  select(Username, Frequency) %>%
  head(50)

neg.hashtags.subset <- neg.hashtags %>%
  arrange(desc(Frequency)) %>%
  select(Hashtag, Frequency) %>%
  head(50)

pos.hashtags.subset <- pos.hashtags %>%
  arrange(desc(Frequency)) %>%
  select(Hashtag, Frequency) %>%
  head(50)

save(list=c("neg.usernames.subset", "pos.usernames.subset", "neg.hashtags.subset", "pos.hashtags.subset"), file="userhashpop.RData")

joined.usernames <- pos.usernames.subset %>%
  full_join(neg.usernames.subset, by=c("Username"="Username"))

joined.usernames <- gsub("^", "\\\\b", joined.usernames$Username)
joined.usernames <- gsub("$", "\\\\b", joined.usernames)

joined.hashtags <- pos.hashtags.subset %>%
  full_join(neg.hashtags.subset, by=c("Hashtag"="Hashtag"))

joined.hashtags <- gsub("^", "\\\\b", joined.hashtags$Hashtag)
joined.hashtags <- gsub("$", "\\\\b", joined.hashtags)

userhash.words <- c(joined.usernames, joined.hashtags)
userhash.words <- unique(tolower(userhash.words))
save(userhash.words, file="userhashwords.RData")

userhash.words2 <- c(joined.usernames$Username, joined.hashtags$Hashtag)
userhash.words2 <- unique(tolower(userhash.words2))
save(userhash.words2, file="userhashwords2.RData")

ex.pattern <- "[^[:alnum:][:space:]!\\?@#,]"
strs <- c("#Hel&lo hue^dh*edj? ebkj.c1'213, !  3%r)fj$3fj.... bkjweb @there.")
new.strs <- gsub(ex.pattern, "", strs)
new.strs
new.new.strs <- gsub(new.patt, "", new.strs)
new.new.strs

head(neg.usernames)
head(pos.usernames)

# Most number of usernames in a negative tweet
max(lengths(neg.tweets.df$usernames))
# Number of negative tweets with more than one username
sum(lengths(neg.tweets.df$usernames) > 1)

# Most number of hashtags in a negative tweet
max(lengths(neg.tweets.df$hashtags))
# Number of negative tweets with more than one hashtag
sum(lengths(neg.tweets.df$hashtags) > 1)

# Most number of usernames in a positive tweet
max(lengths(pos.tweets.df$usernames))
# Number of positive tweets with more than one username
sum(lengths(pos.tweets.df$usernames) > 1)

# Most number of hashtags in a positive tweet
max(lengths(pos.tweets.df$hashtags))
# Number of positive tweets with more than one hashtag
sum(lengths(pos.tweets.df$hashtags) > 1)





################################# TEST SCORES ##################################

# Negative Username Scores for Test Tweets
neg.user.score <- c()
for (usernames in test.tweets$usernames) {
  if (length(usernames) == 0) {
    neg.user.score <- c(neg.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% neg.usernames$Username) {
        sub.scores <- c(sub.scores, neg.usernames$Score[which(usernames[i] == neg.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.user.score <- c(neg.user.score, max(sub.scores))
  } else {
    if (usernames %in% neg.usernames$Username) {
      neg.user.score <- c(neg.user.score, neg.usernames$Score[which(usernames == neg.usernames$Username)])
    } else {
      neg.user.score <- c(neg.user.score, 0)
    }
  }
}
test.tweets <- cbind(test.tweets, neg.user.score)

# Negative Hashtag Scores for Test Tweets
neg.hash.score <- c()
count <- 0
for (hashtags in test.tweets$hashtags) {
  print(count/nrow(test.tweets))
  if (length(hashtags) == 0) {
    neg.hash.score <- c(neg.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% neg.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, neg.hashtags$Score[which(hashtags[i] == neg.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.hash.score <- c(neg.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% neg.hashtags$Hashtag) {
      neg.hash.score <- c(neg.hash.score, neg.hashtags$Score[which(hashtags == neg.hashtags$Hashtag)])
    } else {
      neg.hash.score <- c(neg.hash.score, 0)
    }
  }
  count <- count + 1
}
test.tweets <- cbind(test.tweets, neg.hash.score)

# Positive Username Scores for Test Tweets
pos.user.score <- c()
count <- 0
for (usernames in test.tweets$usernames) {
  print(count/nrow(test.tweets))
  if (length(usernames) == 0) {
    pos.user.score <- c(pos.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% pos.usernames$Username) {
        sub.scores <- c(sub.scores, pos.usernames$Score[which(usernames[i] == pos.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.user.score <- c(pos.user.score, max(sub.scores))
  } else {
    if (usernames %in% pos.usernames$Username) {
      pos.user.score <- c(pos.user.score, pos.usernames$Score[which(usernames == pos.usernames$Username)])
    } else {
      pos.user.score <- c(pos.user.score, 0)
    }
  }
  count <- count + 1
}
test.tweets <- cbind(test.tweets, pos.user.score)

# Positive Hashtag Scores for Test Tweets
pos.hash.score <- c()
count <- 0
for (hashtags in test.tweets$hashtags) {
  print(count/nrow(test.tweets))
  if (length(hashtags) == 0) {
    pos.hash.score <- c(pos.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% pos.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, pos.hashtags$Score[which(hashtags[i] == pos.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.hash.score <- c(pos.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% pos.hashtags$Hashtag) {
      pos.hash.score <- c(pos.hash.score, pos.hashtags$Score[which(hashtags == pos.hashtags$Hashtag)])
    } else {
      pos.hash.score <- c(pos.hash.score, 0)
    }
  }
  count <- count + 1
}
test.tweets <- cbind(test.tweets, pos.hash.score)

# Adding the new features

test.tweets <- subset(test.tweets, select=-c(tweets, usernames, hashtags))
test.tweets <- subset(test.tweets, select=-tweets)

X.test <- cbind(X.test, test.tweets)

save(X.test, file="testMatrix.RData")
save.image()

# STOP STOP STOP

################################################################################




# Negative Username Scores for Negative Tweets
neg.user.score <- c()
for (usernames in neg.tweets.df$usernames) {
  if (length(usernames) == 0) {
    neg.user.score <- c(neg.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% neg.usernames$Username) {
        sub.scores <- c(sub.scores, neg.usernames$Score[which(usernames[i] == neg.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.user.score <- c(neg.user.score, max(sub.scores))
  } else {
    if (usernames %in% neg.usernames$Username) {
      neg.user.score <- c(neg.user.score, neg.usernames$Score[which(usernames == neg.usernames$Username)])
    } else {
      neg.user.score <- c(neg.user.score, 0)
    }
  }
}
neg.tweets.df <- cbind(neg.tweets.df, neg.user.score)

# Negative Hashtag Scores for Negative Tweets
neg.hash.score <- c()
for (hashtags in neg.tweets.df$hashtags) {
  if (length(hashtags) == 0) {
    neg.hash.score <- c(neg.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% neg.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, neg.hashtags$Score[which(hashtags[i] == neg.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.hash.score <- c(neg.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% neg.hashtags$Hashtag) {
      neg.hash.score <- c(neg.hash.score, neg.hashtags$Score[which(hashtags == neg.hashtags$Hashtag)])
    } else {
      neg.hash.score <- c(neg.hash.score, 0)
    }
  }
}
neg.tweets.df <- cbind(neg.tweets.df, neg.hash.score)

# Positive Username Scores for Negative Tweets
pos.user.score <- c()
for (usernames in neg.tweets.df$usernames) {
  if (length(usernames) == 0) {
    pos.user.score <- c(pos.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% pos.usernames$Username) {
        sub.scores <- c(sub.scores, pos.usernames$Score[which(usernames[i] == pos.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.user.score <- c(pos.user.score, max(sub.scores))
  } else {
    if (usernames %in% pos.usernames$Username) {
      pos.user.score <- c(pos.user.score, pos.usernames$Score[which(usernames == pos.usernames$Username)])
    } else {
      pos.user.score <- c(pos.user.score, 0)
    }
  }
}
neg.tweets.df <- cbind(neg.tweets.df, pos.user.score)

# Positive Hashtag Scores for Negative Tweets
pos.hash.score <- c()
for (hashtags in neg.tweets.df$hashtags) {
  if (length(hashtags) == 0) {
    pos.hash.score <- c(pos.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% pos.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, pos.hashtags$Score[which(hashtags[i] == pos.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.hash.score <- c(pos.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% pos.hashtags$Hashtag) {
      pos.hash.score <- c(pos.hash.score, pos.hashtags$Score[which(hashtags == pos.hashtags$Hashtag)])
    } else {
      pos.hash.score <- c(pos.hash.score, 0)
    }
  }
}
neg.tweets.df <- cbind(neg.tweets.df, pos.hash.score)

# Negative Username Scores for Positive Tweets
neg.user.score <- c()
for (usernames in pos.tweets.df$usernames) {
  if (length(usernames) == 0) {
    neg.user.score <- c(neg.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% neg.usernames$Username) {
        sub.scores <- c(sub.scores, neg.usernames$Score[which(usernames[i] == neg.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.user.score <- c(neg.user.score, max(sub.scores))
  } else {
    if (usernames %in% neg.usernames$Username) {
      neg.user.score <- c(neg.user.score, neg.usernames$Score[which(usernames == neg.usernames$Username)])
    } else {
      neg.user.score <- c(neg.user.score, 0)
    }
  }
}
pos.tweets.df <- cbind(pos.tweets.df, neg.user.score)

# Negative Hashtag Scores for Positive Tweets
neg.hash.score <- c()
for (hashtags in pos.tweets.df$hashtags) {
  if (length(hashtags) == 0) {
    neg.hash.score <- c(neg.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% neg.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, neg.hashtags$Score[which(hashtags[i] == neg.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    neg.hash.score <- c(neg.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% neg.hashtags$Hashtag) {
      neg.hash.score <- c(neg.hash.score, neg.hashtags$Score[which(hashtags == neg.hashtags$Hashtag)])
    } else {
      neg.hash.score <- c(neg.hash.score, 0)
    }
  }
}
pos.tweets.df <- cbind(pos.tweets.df, neg.hash.score)

# Positive Username Scores for Positive Tweets
pos.user.score <- c()
for (usernames in pos.tweets.df$usernames) {
  if (length(usernames) == 0) {
    pos.user.score <- c(pos.user.score, 0)
  } else if (length(usernames) > 1) {
    sub.scores <- c()
    for (i in length(usernames)) {
      if (usernames[i] %in% pos.usernames$Username) {
        sub.scores <- c(sub.scores, pos.usernames$Score[which(usernames[i] == pos.usernames$Username)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.user.score <- c(pos.user.score, max(sub.scores))
  } else {
    if (usernames %in% pos.usernames$Username) {
      pos.user.score <- c(pos.user.score, pos.usernames$Score[which(usernames == pos.usernames$Username)])
    } else {
      pos.user.score <- c(pos.user.score, 0)
    }
  }
}
pos.tweets.df <- cbind(pos.tweets.df, pos.user.score)

# Positive Hashtag Scores for Positive Tweets
pos.hash.score <- c()
for (hashtags in pos.tweets.df$hashtags) {
  if (length(hashtags) == 0) {
    pos.hash.score <- c(pos.hash.score, 0)
  } else if (length(hashtags) > 1) {
    sub.scores <- c()
    for (i in length(hashtags)) {
      if (hashtags[i] %in% pos.hashtags$Hashtag) {
        sub.scores <- c(sub.scores, pos.hashtags$Score[which(hashtags[i] == pos.hashtags$Hashtag)])
      } else {
        sub.scores <- c(sub.scores, 0)
      }
    }
    pos.hash.score <- c(pos.hash.score, max(sub.scores))
  } else {
    if (hashtags %in% pos.hashtags$Hashtag) {
      pos.hash.score <- c(pos.hash.score, pos.hashtags$Score[which(hashtags == pos.hashtags$Hashtag)])
    } else {
      pos.hash.score <- c(pos.hash.score, 0)
    }
  }
}
pos.tweets.df <- cbind(pos.tweets.df, pos.hash.score)

unsorted.tweets.df <- rbind(subset(neg.tweets.df,
                                   select=c(Index, neg.user.score,
                                            neg.hash.score, pos.user.score,
                                            pos.hash.score)),
                            subset(pos.tweets.df,
                                   select=c(Index, neg.user.score,
                                            neg.hash.score, pos.user.score,
                                            pos.hash.score)))

sorted.tweets.df <- unsorted.tweets.df %>%
  arrange(Index) %>%
  select(-Index) %>%
  as.data.frame()

X <- cbind(X, sorted.tweets.df)

save(X, file="featureMatrix.RData")
save.image()







# y <- as.factor(raw.data$Sentiment[50001:nrow(raw.data)])
# 
# save(y, file="y.RData")
# save.image()

################################ MODEL BUILDING ################################

suppressMessages(library(xgboost)) # Parallelized Boosting
suppressMessages(library(e1071)) # Naive Bayes, SVM
suppressMessages(library(kernlab)) # Kernelized Methods, KSVM
suppressMessages(library(glmnet)) # LASSO/Elastic-Net GLM
suppressMessages(library(ipred)) # Bagging
suppressMessages(library(randomForest)) # Random Forest
suppressMessages(library(MASS)) # ?
suppressMessages(library(bestglm))
# suppressMessages(library(rpart)) # CART (Decision Trees)

# Naive Bayes Model
# Boosting Model
# Random Forest Model

# Logistic Regression Model (Regularized)
# SVM Model
# KSVM Model

# Loading Data
load("../data/featureMatrix.RData")
load("../data/y.RData")

X <- as.matrix(X)

# Partitioning the data set
n <- nrow(X)
sample.size <- floor(0.8*n)
set.seed(123)
train.sample <- sample(1:n, sample.size)
train <- list(X=X[train.sample,], y=y[train.sample])
test <- list(X=X[-train.sample,], y=y[-train.sample])

# I measured the quality of my classifiers simply by calculating the overall 
# prediction success rate and the success rates for each category of the 
# response variable. I created the following function to do this:
class.test <- function(real, predicted) {
  # Assesses the accuracy of a model's predictions
  ct <- table(real, predicted)
  # [[1]] Percent correct for each category and [[2]] Total percent correct
  return(list(diag(prop.table(ct, 1)), sum(diag(prop.table(ct)))))
}

############################# Logistic Regression ##############################

# lasso.log.model <- cv.glmnet(train$X, train$y,
#                              family="binomial", nfolds=sample.size,
#                              type.measure="class", grouped=FALSE)

# To determine the best variables for logistic regression I used the bestglm 
# function in the bestglm package. The bestglm function performs best subset 
# selection based on a given criteria. The criteria I chose were AIC and BIC. I
# took the best models according to these criteria and compared them.

bglm.train <- cbind(as.data.frame(train$X), y=train$y)

bglm.train <- bglm.train[1:10000,]

log.best.aic <- bestglm(bglm.train, family=binomial, IC="AIC")
log.best.bic <- bestglm(bglm.train, family=binomial, IC="BIC")

mod <- glm(y ~ neg.user.score + neg.hash.score + pos.user.score + pos.hash.score, family=binomial, data=bglm.train)

log.pred <- ifelse(predict(mod, type="response") > 0.5, 1, 0)

class.test(train$y[1:10000], log.pred)

# length(which(X[,1] > 140))
# raw.data[521832,]
