install.packages(c('ROAuth','RCurl'))
install.packages("twitteR")
library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)

# Sentiment Function
score.sentiment <- function(sentences, pos.words, neg.words, .progress = 'none')
{ 
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    },  pos.words, neg.words, .progress = .progress)
    
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
  
}

# New

pos.words = scan('pos-words.txt', what = 'character', comment.char = ';')
neg.words = scan('neg-words.txt', what = 'character', comment.char = ';')
bscore <- score.sentiment(tweet_df$text, pos.words, neg.words, .progress = 'text')
rscore <- score.sentiment(tweet2_df$text, pos.words, neg.words, .progress = 'text')
hist(rscore$score)
hist(bscore$score)

consumerKey <- "4QecxWfZARgqohKa8qAZLhgzh"  
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
auth_URL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "7OcPRv51GKr0Agxh8mjeSNNvypJUy2VEjWFQva4zWc6ttu9gQm"
accessToken <- "3279677640-cLo00plzCkOpQn9WwMdu3QE4BbVY3EbhhLNmqqz"
accessTokenSecret <- "LUiMSz5HOZW72XLo9npT5QXwN9QUjFtrVbxxEzm75FpdQ"

twitCred <- OAuthFactory$new(consumerKey = consumerKey,
                            consumerSecret = consumerSecret,
                            requestURL = reqURL,
                            accessURL = accessURL,
                            authURL = auth_URL)
twitCred$handshake()
setup_twitter_oauth(consumerKey, consumerSecret,accessToken,accessTokenSecret)
tweet1 <- userTimeline("@barcelona", n=100)
tweet2 <- userTimeline("@realmadrid", n=100)
tweet_df <- tbl_df(map_df(tweet1, as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2, as.data.frame))