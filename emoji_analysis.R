library(tidyverse)
library(tidytext)
library(rtweet)
library(igraph)
install.packages("twitteR")
library(twitteR)
app_name = ''
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''
token <- create_token(app = app_name,
                      consumer_key = consumer_key,
                      consumer_secret = consumer_secret,
                      access_token = access_token,
                      access_secret = access_secret)
get_token()
#--------------------------------------------------------------------------------------emoji analysis
meetSA <- userTimeline(user = "SouthAfrica", n = 3200) %>% twListToDF
meetSA

meetSA$text <- iconv(meetSA$text, from = "latin1", to = "ascii", sub = "byte")

emDict_raw <- read.csv2("emojis.csv") %>% 
  select(description = EN, r_encoding = ftu8, unicode)
emDict_raw


matchto <- emDict_raw$r_encoding
description <- emDict_raw$description


emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  
  texts %>% 
    map_df(count_matches, 
           matchto = matchto, 
           description = description, 
           sentiment = sentiment)
  
}

count_matches <- function(string, matchto, description, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) && length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}

rank <- emojis_matching(meetSA$text, as.character(matchto), description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count)) %>%
  arrange(-n)
head(rank,10)

emojitweets <- emojis_matching(meetSA$text, as.character(matchto), description) %>% 
  group_by(text) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  # I add the time created because it makes it easier to look up certain tweets
  merge(meetSA, by = "text") %>% 
  select(text, n, created) %>%
  arrange(-n)
top_5_tweetswithmaxemoji <- head(emojitweets,5)
write.csv(top_5_tweetswithmaxemoji,"E:\\p_backup\\Sem1,2020\\Capstone\\allbloggers\\top5tweetswithmostemoji.csv", row.names = FALSE)

mean(emojitweets$n, na.rm = TRUE)

cleanPosts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

# function that outputs a df of emojis with their top 5 words (by frequency)
wordFreqEmojis <- function(df, text = df$text, description = df$description, top = 5) {
  
  map_df(unique(description), function(x) {
    
    dat <- df %>% 
      filter(description == x)
    
    myCorpus <- Corpus(VectorSource(dat$text)) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(myCorpus)
    # find the sum of words in each Document
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    # collapse matrix by summing over columns
    freq <- colSums(as.matrix(dtm))
    # create sort order (descending)
    ord <- order(freq, decreasing = TRUE)
    
    list(emoji = rep(x, top), 
         words = names(freq[ord][1:top]), 
         frequency = freq[ord][1:top]) 
    
  })
  
}

url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
install.packages("rvest")
library("rvest")
# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")

emojis_merged <- emojis_raw %>%
  merge(emDict_raw, by = "unicode")

new_matchto <- emojis_merged$r_encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

sentiments <- emojis_matching(meetSA$text, as.character(new_matchto), new_description, sentiment) %>%
  mutate(sentiment = count * as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(sentiment_score = sum(sentiment, na.rm = TRUE))
head(sentiments,10)

meetSA_merged <- meetSA %>% 
  select(text, created) %>% 
  merge(sentiments, by = "text", all.x = TRUE)

hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE)
    }
    return(text)
  }
  unname(sapply(text, hg))
}

install.packages("tm")
library("tm")
raw_texts <- emojis_matching(meetSA$text, as.character(matchto), description) %>% 
  select(-sentiment, -count) %>%
  mutate(text = cleanPosts(text)) %>%
  filter(text != "") %>% 
  filter(!is.na(description))
word_emojis <- wordFreqEmojis(raw_texts, raw_texts$text, raw_texts$description) %>% 
  filter(!is.na(words))
word_emojis

emojis_matching(meetSA$text, as.character(matchto), description) %>%
  merge(meetSA %>% select(text, created), by = "text") %>% 
  select(description, created) %>% 
  mutate(weekday = weekdays(created)) %>% 
  select(-created) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n)


