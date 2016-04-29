# Final Project ML

Final project is a group effort (with Chris and Paul) for a [Kaggle competition](https://inclass.kaggle.com/c/tweetersentiment).

<strong>NOTE:</strong> this is a private competition.

## 1 Dependencies:

```
    install.packages("devtools")
    devtools::install_github("DataComputing/DataComputing")
```

## 2 COMPLETED April29: Project Overview

<strong>Note: this is a rough outline of our project and similar to report but not meant to be identical / follow exactly.</strong> 

<br>

<strong>This project was completed and turned in on April 29, 2016.</strong>

### 2.1 Overview of Final Steps:

* @Oliver - write up first complete draft of report
* @Chris - check SVM on server
* @Paul - check SVM on server
* @Chris + @Paul - review Oliver's complete draft once ready - edit!

### 2.2 Detailed Goals Now:

    * describe the use of randomForest + XGBoost for feature selection:

        *see the report-notes-overview.Rmd file for detailed explanation*

##### 2.2.2 SVM: Parameter Tuning

    * 5-Fold CV for both linear and Gaussian kernel SVM via:

        *Cost* (C) - from <code>10^-4, 10^-3, ..., 10^4</code>
        *Sigma* (only for Gaussian kernel method) - from <code>10^-3, ..., 10^3</code>

        Using *grid-search* methods through the *Caret* package in *R*, we found that the optimal parameters for linear and Gaussian kernel SVM models were <strong>0.1 for both the Cost and Sigma values</strong>. This conclusion was obtained through independently running both SVM methods as well as using a <code>0.5%, 1%, 5%, and 20%</code> split of the original labeled data for tuning.

##### 2.2.3 XGBoost: Parameter Tuning

    * 5-fold CV for all parameter tuning via randomization

##### 2.2.4 ?: Parameter Tuning

    * dfdf

## 3 Method Review and Critique

### 3.1 Valid Data Cleaning and Feature Extraction?

asdfdsf

### 3.2 Increased Tuning and Selection

We noticed several components of our models and extraction methods involved significant assumptions about data distribution and relationships. For example, our term frequency usage involved the assumption that a single word is a significant unit of sentiment measurement in tweets. This, in turn, ignores the possibility that character or word combinations may have equal or more predictive importance. It also ignores the possibility that characters and words are not significant measurements of sentiment - either due to a lack of pattern signal in the noise or a fundamental insignificance in tweets (as mentioned above).


## 4 Future Work Ideas:

### 4.1 Ensemble Approach

Although computationally expensive, an ensemble approach to predicting sentiment of tweets could prove extremely useful. Unfortunately, we ran out of time to attempt ana ensemble. However, we crafted several approaches to creating an ensemble to potentially increase predictive accuracy, AUC, and decrease the likelihood of overfitting. Here we describe two routes of next-step ensemble models:

##### 4.1.1 SVM + Recursive Trees

This idea attempts to use SVM and recursive tree models in order to perform regression and classification in an extremely robust and non-linear fashion.

##### 4.1.2 Deep XGBoost Model

This idea involves recursively running boosted and bagged tree models - via XGBoost - to generate probability features based on the original data (*i.e. tweets*). We can continue to generate second-order "artifical" feature sets to use as input for another layer of boosted and bagged trees (or forests).

##### 4.1.3 Multi-model Combination

In order to account for the strengths of certain model components and behavior, while minimizing the negative effects of others, we can use a linear or quadratic combination method of predicted features and / or probabilities. For example, we can generate a set of XGBoost probabilities through 10 optimal parameter sets while simultaneously obtaining 10 SVM classification predictions. These 20 model results can be linearly or quadratically combined in order to produce a single, robust prediction. It should be noted that the combination process could use a number of approaches, similar to distance and clustering metrics. *E.g. average probability (mean) per feature, median probability, majority-rule, etc.*


<hr>
<br>
<strong>Below, all old write-up and code, is kept as reference / record</strong>
<br>
<hr>

## 3 To-Do and Predictor Ideas:

#### 3.1 April 20 Agenda

* Oliver: create script for testing feature importance and model validity / accuracy / stability (<code>sample-r/validityTests.R</code>)
* Chris: put together standard _bag-of-words_ model with 100k Tweets (maybe 80/20 or 60/40 train-test split)
* Paul: create function(s) to add features we designed earlier to data (i.e. create predictors)

#### 3.2 April 18

###### 3.2.1 Predictor Ideas:

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

## 4 Organization:

#### 4.1 README.md

This file. Deescription of this repository. Contains references used (links) and explainations.

#### 4.2 description.pdf

Description from class about competition = logistics.

#### 4.3 data (directory = folder)

Store data given from competition website in this directory (i.e. folder).

#### 4.4 sample-r

Sample code / script files in R. May contain basic structure of executing / printing / formating / etc various machine learning and data analysis techniques. For example: the <strong>sample-cart.R</strong> file contains general code for basic decision tree (including printing).

This directory also contains the <strong>TrainTest.RData</strong> given from the competition website. This contains a compact dataset that is a subset of the <strong>MaskedData.npz</strong> file. This compact dataset contains only 50,000 observations for training. To use:

```
    load("TrainTest.RData")
    # X, y, & XTest
```

#### 4.5 data-mod-log

This directory (i.e. folder) contains data files that we have modified. In addition, it contains a markdown file that has a written description of all of the modifications / changes we have made or algorithms we have attempted (with results / insight).

## 5 References:

Here are various references and tutorials for machine learning algorithms, Twitter data analysis in R, and various sentiment analysis in R (and some in Python).

#### 5.1 Text Analysis Files:

* [Emoticons and smiley faces in various ways](https://gist.github.com/endolith/157796#file-single-character-faces-txt-L98)
* [Emoticon and special character removal](http://stackoverflow.com/questions/12807176/php-writing-a-simple-removeemoji-function)
* [Emoticon and special character removal part. 2](http://stackoverflow.com/questions/12013341/removing-characters-of-a-specific-unicode-range-from-a-string)

#### 5.2 Twitter R Tutorials:

* [Twitter - R Tutorial 1](http://www.r-bloggers.com/analyze-twitter-data-using-r/)
* [Twitter - R Tutorial 2](https://sivaanalytics.wordpress.com/2013/10/10/sentiment-analysis-on-twitter-data-using-r-part-i/)
* [Twitter - R Tutorial 3](https://www.cdata.com/kb/tech/twitter-odbc-r.rst)
* [Twitter - R Tutorial 4](http://www.r-bloggers.com/getting-started-with-twitter-analysis-in-r/)
* [Twitter - R Tutorial 5](http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment)
* [Twitter - R Tutorial 6](https://silviaplanella.wordpress.com/2014/12/31/sentiment-analysis-twitter-and-r/)
* [Twitter - R - create word cloud + text cleanup](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/)
* [Twitter - R - create word cloud + text cleanup 2](https://sites.google.com/site/miningtwitter/questions/talking-about/given-users)

#### 5.3 TF-IDF Tutorials:

Term frequency - inverse document frequency. Frequencies of words are offset by their global frequencies. So it is finding the relative frequency of words.

* [Wiki Overview](https://en.wikipedia.org/wiki/Tf%E2%80%93idf)
* [Conceptual Tutorial](http://www.tfidf.com/)
* [Conceptual + Code Tutorial R](http://www.r-bloggers.com/build-a-search-engine-in-20-minutes-or-less/)
* [Step by Step Tutorial on Text Mining in R](http://www.slideshare.net/whitish/textmining-with-r)

#### 5.4 Bag-of-Words Tutorials:

* [Wiki Overview](https://en.wikipedia.org/wiki/Bag-of-words_model)
* [Great R Tutorial on sentiment analysis in a Kaggle Competition](https://drive.google.com/file/d/0B_sqyEYBKc1wVm4xN0NvQlJlNWc/view)
* [Kaggle Python Tutorial](https://www.kaggle.com/c/word2vec-nlp-tutorial/details/part-1-for-beginners-bag-of-words)
* [Python Tutorial](http://fastml.com/classifying-text-with-bag-of-words-a-tutorial/)

#### 5.5 Academic Papers / Articles:

* [Sample link to paper](http://)
