suppressMessages(library(dplyr))
suppressMessages(library(stringr))

vocab <- scan("../data/vocab.txt", what="character", sep="\n", encoding="latin1")
load("../data/raw.RData")

raw.data$SentimentText <- as.character(raw.data$SentimentText)

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
head(neg.usernames)

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
                                   c(Index, neg.user.score, neg.hash.score,
                                     pos.user.score, pos.hash.score)),
                            subset(pos.tweets.df,
                                   c(Index, neg.user.score, neg.hash.score,
                                     pos.user.score, pos.hash.score)))

sorted.tweets.df <- unsorted.tweets.df %>%
  arrange(Index) %>%
  select(-Index) %>%
  as.data.frame()

X <- cbind(X, sorted.tweets.df)

save(X, file = "../data/add-features/featureMatrix.RData")
save.image()
unlink("../data/add-features/featureMatrix.RData")
unlink(".RData")
