# Log all data changes

This directory contains data files that we have modified and various attempts of algorithms with results / insights.

Keep track of all changes in this markdown file as shown below:

### Change on April 08:

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
