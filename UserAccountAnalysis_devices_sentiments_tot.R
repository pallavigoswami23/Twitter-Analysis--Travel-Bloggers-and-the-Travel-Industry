library(tidyverse)
library(tidytext)
library(rtweet)
library(igraph)
library(twitteR)
install.packages("forestmangr")
library(forestmangr)
library(dplyr)
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

rb <- get_timeline("@SouthAfrica", n= 3200)
rb_tweets_organic <- rb[rb$is_retweet==FALSE, ] 
# Remove replies
rb_tweets_organic <- subset(rb_tweets_organic, is.na(rb_tweets_organic$reply_to_status_id)) 
print(rb_tweets_organic)
rb_tweets_organic <- rb_tweets_organic %>% arrange(-favorite_count)
rb_tweets_organic[1,5]
rb_tweets_organic
rb_tweets_organic <- rb_tweets_organic %>% arrange(-retweet_count)
rb_tweets_organic[1,5]

# Keeping only the retweets
rb_retweets <- rb[rb$is_retweet==TRUE,]
rb_retweets 
# Keeping only the replies
rb_replies <- subset(rb, !is.na(rb$reply_to_status_id))
rb_replies
# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2160, 112, 928)
)

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
# Rounding the data to two decimal points
data <- round_df(data, 2)
# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


colnames(rb)[colnames(rb)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(rb, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from South Africa",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

detach("package:plyr", unload=TRUE) 
library(dplyr) 
rb_app <- rb %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
rb_app <- subset(rb_app, count > 11)

data <- data.frame(
  category=rb_app$source,
  count=rb_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


rb_tweets_organic$text <-  gsub("https\\S*", "", rb_tweets_organic$text)
rb_tweets_organic$text <-  gsub("@\\S*", "", rb_tweets_organic$text) 
rb_tweets_organic$text  <-  gsub("amp", "", rb_tweets_organic$text) 
rb_tweets_organic$text  <-  gsub("[\r\n]", "", rb_tweets_organic$text)
rb_tweets_organic$text  <-  gsub("[[:punct:]]", "", rb_tweets_organic$text)

tweets <-rb_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of SouthAfrica",
       subtitle = "Stop words removed from the list")

rb_tweets_organic$hashtags <- as.character(rb_tweets_organic$hashtags)
rb_tweets_organic$hashtags <- gsub("c\\(", "", rb_tweets_organic$hashtags)
set.seed(2456)
wordcloud(rb_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
set.seed(2456)
wordcloud(rb_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))
install.packages("syuzhet")
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
