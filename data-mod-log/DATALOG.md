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

### Getting unique occurance & IDs

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
    UniqueOccurance = rep(NA, times = n)

    for (i in 1:n) {
        UniqueOccurance[i] = sum(negTest.vec == UniqueUsers[i])
        print(i / n) #Progress: done when it reaches 1.
    }
```

Make dataframe and check it out!

```
    histDF <- data.frame(occur = UniqueOccurance, user = UniqueUsers)
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

#### Try pulling out @ and # with 1k, 10k, and 100k tweets
