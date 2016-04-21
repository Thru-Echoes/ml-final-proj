# Final Project ML

Final project is a group effort (with Chris and Paul) for a [Kaggle competition](https://inclass.kaggle.com/c/tweetersentiment).

<strong>NOTE:</strong> this is a private competition.

## Dependencies:

```
    install.packages("devtools")
    devtools::install_github("DataComputing/DataComputing")
```

## To-Do & Predictor Ideas:

### April 20 Agenda

* Oliver: create script for testing feature importance and model validity / accuracy / stability (<code>sample-r/validityTests.R</code>)
* Chris: put together standard _bag-of-words_ model with 100k Tweets (maybe 80/20 or 60/40 train-test split)
* Paul: create function(s) to add features we designed earlier to data (i.e. create predictors) 

### April 18

#### Predictor Ideas:

- [ ] Create normalized word count of tweets
- [ ] Create normalized word count of modified tweets (e.g. after extracting only meaningful words)
- [ ] Create normalized character count of tweets
- [ ] Create normalized character count of modified tweets (e.g. after extracting only meaningful words)
- [ ] Create normalized positive occurrences of '@' usernames
- [ ] Create normalized negative occurrences of '@' usernames
- [ ] Create normalized positive occurrences of '#' hashtags
- [ ] Create normalized negative occurrences of '#' hashtags
- [ ] Create normalized positive occurrences of emoticons
- [ ] Create normalized negative occurrences of emoticons
- [ ] Create column that is sentiment value per tweet from Bag-of-words

And more...
- [ ] Linear combination of normalized positive and normalized negative username / hashtag occurrences
- [ ] Create a few special columns for occurrence of most-negative or most-positive usernames / hashtags <strong>(see below)</strong>
- [ ]

For example: top 3 usernames that appear with negative tweets may have thousands of occurrences and the 4th most popular negative username may be only 20 occurrences. We could create 3 special columns, one per top username of negative tweets, and each tweet has a <code>1</code> in that column if that username appears.

<hr>

## Organization:

#### README.md

This file. Deescription of this repository. Contains references used (links) and explainations.

#### description.pdf

Description from class about competition = logistics.

#### data (directory = folder)

Store data given from competition website in this directory (i.e. folder).

#### sample-r

Sample code / script files in R. May contain basic structure of executing / printing / formating / etc various machine learning and data analysis techniques. For example: the <strong>sample-cart.R</strong> file contains general code for basic decision tree (including printing).

This directory also contains the <strong>TrainTest.RData</strong> given from the competition website. This contains a compact dataset that is a subset of the <strong>MaskedData.npz</strong> file. This compact dataset contains only 50,000 observations for training. To use:

```
    load("TrainTest.RData")
    # X, y, & XTest
```

#### data-mod-log

This directory (i.e. folder) contains data files that we have modified. In addition, it contains a markdown file that has a written description of all of the modifications / changes we have made or algorithms we have attempted (with results / insight).

## References:

Here are various references and tutorials for machine learning algorithms, Twitter data analysis in R, and various sentiment analysis in R (and some in Python).

#### Text Analysis Files:

* [Emoticons and smiley faces in various ways](https://gist.github.com/endolith/157796#file-single-character-faces-txt-L98)
* [Emoticon and special character removal](http://stackoverflow.com/questions/12807176/php-writing-a-simple-removeemoji-function)
* [Emoticon and special character removal part. 2](http://stackoverflow.com/questions/12013341/removing-characters-of-a-specific-unicode-range-from-a-string)

#### Twitter R Tutorials:

* [Twitter - R Tutorial 1](http://www.r-bloggers.com/analyze-twitter-data-using-r/)
* [Twitter - R Tutorial 2](https://sivaanalytics.wordpress.com/2013/10/10/sentiment-analysis-on-twitter-data-using-r-part-i/)
* [Twitter - R Tutorial 3](https://www.cdata.com/kb/tech/twitter-odbc-r.rst)
* [Twitter - R Tutorial 4](http://www.r-bloggers.com/getting-started-with-twitter-analysis-in-r/)
* [Twitter - R Tutorial 5](http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment)
* [Twitter - R Tutorial 6](https://silviaplanella.wordpress.com/2014/12/31/sentiment-analysis-twitter-and-r/)
* [Twitter - R - create word cloud + text cleanup](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/)
* [Twitter - R - create word cloud + text cleanup 2](https://sites.google.com/site/miningtwitter/questions/talking-about/given-users)

#### TF-IDF Tutorials:

Term frequency - inverse document frequency. Frequencies of words are offset by their global frequencies. So it is finding the relative frequency of words.

* [Wiki Overview](https://en.wikipedia.org/wiki/Tf%E2%80%93idf)
* [Conceptual Tutorial](http://www.tfidf.com/)
* [Conceptual + Code Tutorial R](http://www.r-bloggers.com/build-a-search-engine-in-20-minutes-or-less/)
* [Step by Step Tutorial on Text Mining in R](http://www.slideshare.net/whitish/textmining-with-r)

#### Bag-of-Words Tutorials:

* [Wiki Overview](https://en.wikipedia.org/wiki/Bag-of-words_model)
* [Great R Tutorial on sentiment analysis in a Kaggle Competition](https://drive.google.com/file/d/0B_sqyEYBKc1wVm4xN0NvQlJlNWc/view)
* [Kaggle Python Tutorial](https://www.kaggle.com/c/word2vec-nlp-tutorial/details/part-1-for-beginners-bag-of-words)
* [Python Tutorial](http://fastml.com/classifying-text-with-bag-of-words-a-tutorial/)

#### Academic Papers / Articles:

* [Sample link to paper](http://)

Explain what this link (above) is here...

*
