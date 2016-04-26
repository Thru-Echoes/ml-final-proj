# Log all data changes

This directory contains data files that we have modified and various attempts of algorithms with results / insights.

Keep track of all changes in this markdown file as shown below:

## Change on April 08:

* have raw.data stored in <code>raw.Rdata</code>
* raw.data has 4 variables: ItemID, Sentiment, SentimentSource, & SentimentText

```
    load("data/raw.RData")
    fromKaggle <- raw.data[raw.data$SentimentSource == "Kaggle", ]
    nKaggle <- nrows(fromKaggle)
    # = 1349 total
```
How many from Kaggle$Sentiment = -1, 1, 0?

-1 (no score) = 41
0 (negative score) = 604
1 (positive score) = 704

##### Looking at Sentiment140:

```
    s140 <- raw.data[raw.data$SentimentSource == "Sentiment140", ]
    summary(as.factor(s140$Sentiment))
    s140.neg <- s140[s140$Sentiment == 0, ]
    s140.pos <- s140[s140$Sentiment == 1, ]
```

How many from s140$Sentiment = -1, 1, 0?

-1 (no score) = 49959
0 (negative score) = 762896
1 (positive score) = 764423

* roughly: number no score = 50k
* roughly: number negative score = 760k
* roughly: number positive score = 760k

##### Get sample 50k from negative and positive

```
    set.seed()
    ind50k <- sample(1:762896, 50000, replace = FALSE)
    s140.neg50 <- s140.neg[ind50k, ]

    indPos50k <- sample(1:764423, 50000, replace = FALSE)
    s140.pos50 <- s140.pos[indPos50k, ]

    neg50k <- s140.neg50[, 4]
    pos50k <- s140.pos50[, 4]

    neg50k <- as.character(neg50k)
    pos50k <- as.character(pos50k)
```

##### Sentiment helper function & stop words

<code>sample-r/sentiment-func.R</code>

Stop words from this package:

```
    library(tm)
    vectorOfStopWords <- stopwords(kind = "en")
```

## Change on April 13:

### Getting unique occurrences & IDs

<strong>These are only for negative tweets!</strong>

```
    load("data/raw.RData")
    RawNeg.df = raw.data[raw.data$Sentiment == 0, ]
    RawNegTweet = as.character(RawNeg.df[ , 4])
    library(stringr)
    NegTweet.OneString = paste(RawNegTweet, sep = " ", collapse = "")
    negTest = str_extract_all(string = NegTweet.OneString, pattern = "@[[:alnum:]_]+")

    negTest.vec = negTest[[1]] #Convert test into a character vector.
    UniqueUsers = unique(negTest.vec) #Find the unique usernames.
    n = length(UniqueUsers)
    UniqueOccurrence = rep(NA, times = n)

    for (i in 1:n) {
        UniqueOccurrence[i] = sum(negTest.vec == UniqueUsers[i])
        print(i / n) #Progress: done when it reaches 1.
    }
```

Make dataframe and check it out!

```
    histDF <- data.frame(occur = UniqueOccurrence, user = UniqueUsers)
```

Some print outs (THESE ARE NEGATIVE SENTIMENTS):

```
    # count = 179k
    occur            user
    1  1478       @tommcfly
    2  1336     @mileycyrus
    3  1251       @ddlovato
    4   505 @DonnieWahlberg
    5   416   @mitchelmusso
    6   379   @jordanknight
```

Check top 1k user references for negative tweets:

```
    subNegUsers <- histDF[1:1000, ]
```

### Extracting @ and #


#### Extract @ (single and multiple users)

```
    # Single @ and double @ per tweet
    singleAt <- "@katyperry does your SKIN look like an old person's "
    doubleAt <- "@SheliaTaylor @DZL.....so how bad do you guys miss me"

    noAt <- "happy B-day dad love you!"
    no2At <- "Jonas Brothers Youtube account is working now"

    # Vector of tweets
    tweetz <- c(singleAt, doubleAt, noAt, no2At)
    naCols <- c(NA, NA, NA, NA)
    sampleDF <- data.frame(tweetz, naCols)
```

Extract single instances of @:

```
    myPattern <- "@([a-zA-Z0-9_]+)"
    extractDF <- extractMatches(data = sampleDF, pattern = myPattern, var = tweetz, isPresent = 1)
```

What if multiple instances of @?

```
    > myMultiPattern <- "(.*)(@[a-zA-Z0-9_]+)"
    > multiDF <- extractMatches(data = sampleDF, pattern = myMultiPattern, var = tweetz)
```

See the extracted:
```
    > multiDF
                                                 tweetz naCols         match1     match2
1  @katyperry does your SKIN look like an old person's      NA                @katyperry
2 @SheliaTaylor @DZL.....so how bad do you guys miss me     NA @SheliaTaylor        @DZL
3                             happy B-day dad love you!     NA           <NA>       <NA>
4         Jonas Brothers Youtube account is working now     NA           <NA>       <NA>
```
#### Create sample data to validate extraction

```
    tripleAt <- "@foo wowie @bar...this is crazyyyy!! @baz"
    emailAt <- "hey this is a tweet stat@berko.gov interesting"

    moreTweetz <- c(singleAt, doubleAt, tripleAt, emailAt, noAt, no2At)
    moreDF <- data.frame(moreTweetz)
```

Now extract:

```
    gsubPattern <- "(.*)(@\\w+)"
    extractBar <- extractMatches(data = moreDF, pattern = gsubPattern, var = moreTweetz)
    extractBar
```

#### Ignore emails (and other word boundries)

<strong>NOTE: this is attempt 1</strong>

```
    tmpP <- "(.*)\\B(@\\w+)"
    extractBar <- extractMatches(data = moreDF, pattern = tmpP, var = moreTweetz)
    extractBar
```

<strong>NOTE: this is attempt 2</strong>

Check for a ton of @:

```
    tonAt <- "@wow1 @wow2 @wow3 @wow4 @wow5 this is wild. @wow6"
    tonTweetz <- c(singleAt, doubleAt, tripleAt, tonAt, emailAt, noAt, no2At)
    tonDF <- data.frame(tonTweetz)

    tmpP <- "([^.!]*)\\B(@[a-zA-Z0-9_]+)"
    extractMulti <- extractMatches(data = tonDF, pattern = tmpP, var = tonTweetz)
```

<strong>NOTE: this is attempt 3</strong>

```
    tmpP <- "(?<=^|(?<=[^a-zA-Z0-9-_\\.]))@([A-Za-z]+[A-Za-z0-9]+)"
    extractMulti <- extractMatches(data = tonDF, pattern = tmpP, var = tonTweetz)
    extractMulti
```

<strong>NOTE: this is attempt 4</strong> - using <code>str_extract_all</code>

```
    # tripleAt (single string)
    #fooExtract <- str_extract_all(string = tripleAt, pattern = "@[[:alnum:]_]+")
    #

    # data frame = tonDF
    extractedDF <- tonDF %>% mutate(matches = str_extract_all(tonTweetz, "(\\B@[[:alnum:]_]+)"))
    extractedDF$matches[[4]][1]
    # get 4th tweet and 1st match in it
```

#### Pull out #

```
    firstHash <- "wow I am a doggie #doglife"
    secondHash <- "ok it is cold outside #winter #cold"
    noHash <- "yeah right it is totally not cold"
    no2Hash <- "ok maybe it is a little bit cold"
    mixTweet <- "@johncena you are the friggin man #wwe"

    fooTweets <- c(firstHash, secondHash, noHash, no2Hash, mixTweet)
    allTweets <- c(firstHash, secondHash, noHash, no2Hash, mixTweet, singleAt, doubleAt, tripleAt, tonAt, noAt, no2At, emailAt)
```

Now pull out # events

```
    # data frame
    hashDF <- data.frame(fooTweets)
    getHashDF <- hashDF %>% mutate(hashMatch = str_extract_all(fooTweets, "(\\B#[[:alnum:]_]+)"))
    #getHashDF$hashMatches[[2]][1]
    # get 2th tweet and 1st match in it
```

#### Pull out @ and #

```
    # tweets = allTweets
    # Make dataframe
    mixDF <- data.frame(allTweets)
    getExtracted <- mixDF %>% mutate(atMatches = str_extract_all(allTweets, "(\\B@[[:alnum:]_]+)"),
                                    hashMatches = str_extract_all(allTweets, "(\\B#[[:alnum:]_]+)"))

    # 5th tweet, 1st @
    getExtracted$atMatches[[5]][1]

    # 5th tweet, 1st #
    getExtracted$hashMatches[[5]][1]
```

## April 14:

#### Pull out @ and # for all neg and all pos

<strong>NOTE: this takes under a minute to run...</strong>

```
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

    head(allExtractNeg)
    head(allExtractPos)
```

Write out files:

```
    # write(allExtractNeg, "data/all-extract-neg")
    # write(allExtractPos, "data/all-extract-pos")
```

<hr>

<strong>And other way to extract just user Occurrence data:</strong>

```
    negTest = str_extract_all(string = NegTweet.OneString, pattern = "(\\B@[[:alnum:]_]+)")
    posTest = str_extract_all(string = PosTweet.OneString, pattern = "(\\B@[[:alnum:]_]+)")

    negTest.vec = negTest[[1]] #Convert test into a character vector.
    posTest.vec = posTest[[1]] #Convert test into a character vector.

    uniqueNegUsers = unique(negTest.vec) #Find the unique usernames.
    uniquePosUsers = unique(posTest.vec) #Find the unique usernames.
    nNeg = length(uniqueNegUsers)
    nPos = length(uniquePosUsers)
    uniqueNegOccurrence = rep(NA, times = nNeg)
    uniquePosOccurrence = rep(NA, times = nPos)

    for (i in 1:nNeg) {
        uniqueNegOccurrence[i] = sum(negTest.vec == uniqueNegUsers[i])
        print(i / nNeg) #Progress: done when it reaches 1.
    }

    for (i in 1:nPos) {
        uniquePosOccurrence[i] = sum(posTest.vec == uniquePosUsers[i])
        print(i / nPos) #Progress: done when it reaches 1.
    }
```

With code above - write out and read in file:

```
    write(uniqueNegUsers, "data/unique-neg-users")
    write(uniqueNegOccurrence, "data/unique-neg-occur")

    write(uniquePosUsers, "data/unique-pos-users")
    write(uniquePosOccurrence, "data/unique-pos-occur")

    # read in
    sampleNegUsers <- read.table(file.choose())
```

## Change on April 18:

Using Chris's <code>getPattern()</code> in <code>sample-r/getPattern.R</code>. A screenshot of the distribution of positive emoticons in all of the positive tweets.

#### Extracting occurrences of positive emoticons in positive tweets

<strong>NOTE: </strong> there are roughly 760k positive tweets.

```
    load("data/raw.RData")
    RawPos.df = raw.data[raw.data$Sentiment == 1, ]
    RawNeg.df = raw.data[raw.data$Sentiment == 0, ]
    posEmo_posTweets <- getPattern(data = RawPos.df, sub.index = 1:nrow(RawPos.df), char.col = 4, pattern = "[:;][-]?[)D]+|[(]+[-]?[:;]", pat.name = "positive emoticon")
    hist(posEmo_posTweets[, 6])
```

Here is the distribution of positive emoticons in the positive tweets:

```
    summary(as.factor(posEmo_posTweets[, 6]))
    #####      0      1      2      3      4      5      6
    ##### 757384   7563    169      8      1      1      1
```

#### Potential positive and negative emoticons

Positive (from online): <code>((:|;|8)+(-)*(\)|D|P|p)+)|((\()+(-)*(:|;)+)</code>

Negative (from online): <code>((:)+(-|')*(\()+)|((\))+(-)*(:|;)+)</code>

<strong>NOTE: see the <code>sample-r/extractEmoPattern.R</code> file / script for information and code to run the following.</strong>

Positive (translated): <code>[:;8][-']?[)DPp]+|[(][-']?[8;:]...</code>

Negative (translated): <code>...</code>

### Positive Emoticon Patterns:

```
    posPattern1 <- "[:;][-]?[)D]+|[(]+[-]?[:;]"
    posPattern2 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]"
    posPattern3 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]|[\\^][_\\.\\*-]*[\\^]|[-][_\\.]*[-]"
```

### Negative Emoticon Patterns:

```
    negPattern1 <- "[:][-]*[(]+|[ ]"
    negPattern2 <- "[>]*[:8][-']*[(]"
    negPattern3 <- ""
```

#### Extracting occurrences of negative emoticons in negative tweets

```
    posEmo_negTweets <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = "[:;][-]?[)D]+|[(]+[-]?[:;]", pat.name = "positive emoticon")
```

#### Extracting negative emoticons in positive tweets

```
    negEmo_posTweets <- ...
    # see sample-r/extractEmoPattern.R file / script for information and code
```

#### Extracting positive emoticons in negative tweets

Using 1st pattern:

```
    posEmo_negTweets_1 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = posPattern1, pat.name = "positive emoticon")
    summary(as.factor(posEmo_negTweets_1[, 6]))
    ####      0      1      2      3      4     10
    #### 758809   4570    103     14      3      1
```

Using 2nd pattern:

```
    # posPattern2 <- "[:;8][-']?[)DPp]+|[(]+[-']?[8:;]"
    posEmo_negTweets_2 <- getPattern(data = RawNeg.df, sub.index = 1:nrow(RawNeg.df), char.col = 4, pattern = posPattern2, pat.name = "positive emoticon")
    summary(as.factor(posEmo_negTweets_2[, 6]))
```

## Modifications from April 20 meeting:

* initialized <code>validityTests.R</code> script with some sample functions and references + notes
* created <strong>BOW_.RDATA</strong> files for *bag-of-words* and 6k - 4k train - test split of sample data

Load BOW RData files and create xTrain, yTrain, xTest, and yTest:

```
    load("data/BOW_trainCleanDF.RData")
    load("data/BOW_testCleanDF.RData")
    trainDF <- BOW_trainCleanedDF
    testDF <- BOW_testCleanedDF
    xTrain <- trainDF[, 2:21]
    xTest <- testDF[, 2:21]

    yTrain <- trainDF[, 1]
    yTest <- testDF[, 1]
```

Run randomForest:

```
    # function from validityTests.R:
    doRF <- function(xTrain, yTrain, xTest, yTest, numTrees) {
        library(randomForest)
        isImportance = TRUE

        # random seed
        set.seed(1892)

        # predict
        rfModel <- randomForest(x = xTrain, y = as.factor(yTrain), ntree = numTrees, do.trace = TRUE, importance = isImportance)
        rfModel.myPred <- predict(rfModel, xTest)

        # correct classification (for binary response)
        rfModel.correctPred <- (rfModel.myPred == yTest)
        rfModel.numCorrect <- length(rfModel.correctPred)

        # misclassification error
        rfModel.misClass <- 1 - sum(rfModel.correctPred) / rfModel.numCorrect

        return(rfModel)
    }

    getYhat <- function(myPred, threshold = 0.5, labels = class.labels) {
        factor(as.numeric(myPred > threshold), levels = 0:1, labels = labels)
    }

    ## Calculate specificity
    getSpec <- function(class, yhat) {
      conf <- table(class, yhat)
      conf[1,1] / sum(conf[1, ])
    }

    ## Calculate sensitivity
    getSens <- function(class, yhat) {
      conf <- table(class, yhat)
      conf[2,2] / sum(conf[2, ])
    }

    ## Calculate accuracy
    getAcc <- function(class, yhat) {
      sum(diag(table(class, yhat))) / length(class)
    }

    ## Calculate summary statistics
    getStats <- function(class, yhat) {
      c(accuracy = getAcc(class, yhat),
        sensitivity = getSens(class, yhat),
        specificity = getSpec(class, yhat))
    }

    neg.label <- 0
    pos.label <- 1
    class.labels <- c(neg.label, pos.label)

    ### Use with randomForest() --> doRF() function from above
    exRF <- doRF(xTrain, yTrain, xTest, yTest, 100)
```

Check out randomForest predictions and summary statistics:

```
    # predictions in exRF.myPred --> .myPred is ~= yHat
    exRF.myYhat <- getYhat(exRF.myPred)

    # get summary statistics
    exRF.summaryStats <- getStats(yTest, exRF.myYhat)
    exRF.summaryStats
```

Get variable importance plot:

```
    varImpPlot(exRF)
```

<hr>

## April 24 meeting:

### Overview

##### We currently have:

1. 50 features from Bag-of-words on all data
2. 4 features from username + hashtag scores for all data

##### To-do for today:

1. Create 54 feature <strong>a. dataframe + b. matrix </strong>
2. Create 1k sample of a. dataframe + b. matrix
3. Run <strong>a. Naive Bayes, b. randomForest(), and c. XGBoost</strong> on 1k sample
4. Setup script for 3a-c runs on all data
5. Setup <strong>word2vec</strong> script
6. (maybe) Setup <strong>emoticon</strong> extraction features script  

<hr>

### Notes on process

* 50 BOW features in <code>data/bow-all-data/</code>
* 4 username / hashtag features in <code>data/featureMatrix.RData</code>

Get 4 username / hashtag features:

```
    load("data/featureMatrix.RData")
    # X = data object
    #
    # has features [tweet.lengths, neg.user.score, neg.hash.score, pos.user.score, pos.hash.score]
    dim(X)
    # 1528627 x 5
    XallScores <- X
```

Get 1k sample df and matrix for testing:

```
    # sample 1k indices
    tmpIndx <- sample(1:nrow(trainRaw.df), size = 1000, replace = FALSE)
    length(unique(tmpIndx))
    # 1000
    train_1k_april24.df <- trainRaw.df[tmpIndx, ]
```

Get username / hashtag trainIndex corresponding to BOW train df:

```
    X.trainRaw.df <- XallScores[(trainIndex), ]
    X.train1k.df <- X.trainRaw.df[tmpIndx, ]
```

Run BOW extraction on 1k sample:

```
    trainCleanedDF_1k = data.frame(Sentiment = train_1k_april24.df$Sentiment,
                           train_1k_april24.df[ , seq(from = 7, to = ncol(train_1k_april24.df), by = 2)])
    BOW_trainCleanedDF_1k = trainCleanedDF_1k
```

Add all features into 1 single 55 + response dataframe (55 because also have tweet length):

```
    total_features_1k <- cbind(BOW_trainCleanedDF_1k, X.train1k.df)
```

Run tests:

```
    # do randomForest...
```

### April 24 - do on all data

```
    # Do doBOW_april24.R
    ...
```

<hr>

## April 25 meeting:

### Overview

Ran 100-tree randomForest, added variable importance plot, put together XGBoost and corresponding parameter search scripts. Running XGBoost parameter search with some subset of data - e.g. 25% of data - run 10k set of random parameters and check for minimum error / AUC / logloss.
