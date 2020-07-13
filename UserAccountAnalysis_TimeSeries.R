library(tidyverse)
library(tidytext)
library(rtweet)
library(igraph)
library(twitteR)
app_name = ''
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#the most tweets we can bring back from a user timeline is the most recent 3600...
rb=userTimeline('BudgetTraveller',n=3600)
tw.df=twListToDF(rb)

#Pull out the names of folk who have been "old-fashioned RTd"...
install.packages("stringr")
library(stringr)
library(plyr)
trim <- function (x) sub('@','',x)

tw.df$rt=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
tw.df$rtt=sapply(tw.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')
install.packages("ggplot2")
library(ggplot2)
ggplot(tw.df)+geom_point(aes(x=created,y=screenName))


tw.dfs=subset(tw.df,subset=((Sys.time()-created)<8000))
ggplot(tw.dfs)+geom_point(aes(x=created,y=screenName))

#Order the replyToSN factor levels in the order in which they were first created
tw.dfx=ddply(tw.dfs, .var = "replyToSN", .fun = function(x) {return(subset(x, created %in% min(created),select=c(replyToSN,created)))})
tw.dfxa=arrange(tw.dfx,-desc(created))
tw.dfs$replyToSN=factor(tw.dfs$replyToSN, levels = tw.dfxa$replyToSN)

#and plot the result
ggplot(tw.dfs)+geom_point(aes(x=created,y=replyToSN))

ggplot()+geom_point(data=subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=created,y=replyToSN),col='red') 
+ geom_point(data=subset(tw.dfs,subset=(!is.na(rt))),aes(x=created,y=rt),col='blue')
+ geom_point(data=subset(tw.dfs,subset=(is.na(replyToSN) & is.na(rt))),aes(x=created,y=screenName),col='green')


r_table <- table(tw.dfs$replyToSN)
#..rank them...
r_levels <- names(r_table)[order(-r_table)]
#..and use this ordering to order the factor levels...
tw.dfs$replyToSN <- factor(tw.dfs$replyToSN, levels = r_levels) 

#Then we can plot the chart...
ggplot(subset(tw.dfs,subset=(!is.na(replyToSN))),aes(x=replyToSN)) + geom_bar(aes(y = (..count..)))

head(table(tw.dfs$replyToSN))


topTastic=function(dfc,num=5){
  r_table <- table(dfc)
  r_levels <- names(r_table)[order(-r_table)]
  head(table(factor(dfc, levels = r_levels)),num)
}
#Display the most old-style retweeted folk
topTastic(tw.dfs$rt)
#or the 10 most replied to...
topTastic(tw.dfs$replyToSN,10)


#label a tweet with the month number
tw.dfs$month=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$mon})
#label a tweet with the hour
tw.dfs$hour=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tw.dfs$wday=sapply(tw.dfs$created, function(x) {p=as.POSIXlt(x);p$wday})


ggplot(tw.dfs)+geom_jitter(aes(x=wday,y=hour))
