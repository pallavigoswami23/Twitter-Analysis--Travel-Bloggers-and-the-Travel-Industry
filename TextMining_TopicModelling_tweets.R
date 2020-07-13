library(tidyverse)
library(tidyr)
library(tidytext)
library(lubridate)
library(scales)
library(readr)
library(stringr)
library(stringi)
library(ggplot2)
library(rtweet)
install.packages("tm")
install.packages("tsne")
install.packages("Rtsne")
install.packages("dendextend")
install.packages("factoextra")
install.packages("wordcloud2")
install.packages("widyr")
install.packages("Rmpfr")
install.packages("wordcloud")
install.packages("dplyr")
library(dplyr)
library(wordcloud)
library(Rmpfr)
library(widyr)
library(wordcloud2)
library(factoextra)
library(dendextend)
library(tsne)
library(Rtsne)
library(tm)
install.packages("twitteR")
library(twitteR)
library(igraph)
library(ggraph)
install.packages("topicmodels")
library(topicmodels)
library(reshape2)
install.packages("ggthemes")
library(ggthemes)
library(cluster)

app_name = ''
consumer_key = ''
consumer_secret = ''
access_token = ''
access_secret = ''
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

meetSA <- searchTwitter("#meetsouthafrica",n=3200,lang="en")
meetSA <- twListToDF(meetSA)
setwd("E:/p_backup/Sem1,2020/Capstone/allbloggers")
save(meetSA,file="meetSA.Rdata")
write.csv(meetSA,"meetSA.csv")

text.df <- read.csv("meetSA.csv")
tweets <- data.frame(text=text.df$text)
tweets$text <- as.character(tweets$text)
tweets$text <- gsub('http\\S+\\s*','',tweets$text)
tweets$text <- gsub('\\b+RT','',tweets$text)
tweets$text <- gsub('#\\S+','',tweets$text)
tweets$text <- gsub('@\\S+','',tweets$text)
tweets$text <- gsub('[[:cntrl:]]','',tweets$text)
tweets$text <- gsub('"\\d"','',tweets$text)
tryTolower <- function(x){
  y=NA
  try_error=tryCatch(tolower(x),error=function(e)e)
  if(!inherits(try_error,'error'))
    y=tolower(x)
  return(y)
}
custom.stopwords <- c(stopwords('english'),'south','Africa','meet','South','africa')
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus,content_transformer(tryTolower))
  corpus <- tm_map(corpus,removeWords,custom.stopwords)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removeNumbers)
  return(corpus)
}
corpus <- Corpus(VectorSource(tweets$text))
corpus <- clean.corpus(corpus)
tdm <- TermDocumentMatrix(corpus)                   
tdm

m <- as.matrix(tdm)
term.freq <- rowSums(m)
freq.df <- data.frame(word=names(term.freq),frequency=term.freq)
freq.df
freq.df <- freq.df[order(freq.df[,2],decreasing=T),]
freq.df$word <- factor(freq.df$word,levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,],aes(x=word,y=frequency)) +
  geom_bar(stat="identity",fill="darkred") +
  coord_flip() +
  theme_gdocs()+
  geom_text(aes(label=frequency),color="white",hjust=1.25,size=2.5)+
  ggtitle("Word frequencies(top 20) in tweets of meetsouthafrica")+
  theme(plot.title = element_text(size=10,face="bold"))+
  theme_bw()

associations <- findAssocs(tdm,"canyon",0.25)
associations
associations <- as.data.frame(associations)
associations
associations$terms <- row.names(associations)
associations$terms <- factor(associations$terms,levels=associations$terms)
png("assocs.png", width=1000, height=1000)
ggplot(associations,aes(y=terms))+
  geom_point(aes(x=canyon),canyon=associations,size=4)+
  theme_gdocs()+
  geom_text(aes(x=canyon,label=canyon),color="darkblue",hjust=0.25,size=6)+
  theme(text=element_text(size=15),axis.title.y=element_blank())
dev.off()
tdm
freq <- findFreqTerms(tdm,5)
freq
tsne_out <- Rtsne(m,dims=2,check_duplicates=F)
tsne_out
str(tsne_out)
plot(tsne_out$Y,t="n",main="tsne visualizations of words",cex.main=1)
text(tsne_out$Y,labels=rownames(m),cex=0.35)

X1 <- tsne_out$Y[,1]
X2 <- tsne_out$Y[,2]
df <- data.frame(X1,X2)
df
fviz_nbclust(df,clara, method="silhouette") + theme_classic()

clara.res <- clara(df,5,samples=50,pamLike=TRUE)
fviz_cluster(clara.res,
             palette=c("red","green","blue","purple","yellow"),
             ellipse.type="t",
             geom="point",pointsize=0.5,
             ggtheme=theme_classic())


dtm <- DocumentTermMatrix(corpus)
str(dtm)
tweets_td <- tidy(dtm)
tweets_td
tweets_td %>%
  count(term,sort=TRUE)%>%
  filter(n>15)%>%
  mutate(word=reorder(term,n))%>%
  ggplot(aes(word,n))+
  geom_col(fill="yellow",col="black") +
  xlab(NULL)+
  coord_flip()+
  ggtitle("word with frequencies more than 15 in meetsouthafrica tweets")+
  theme(plot.title=element_text(size=10,face="bold"))+theme_bw()
tweet_words <- tweets_td %>%
  count(term,sort=TRUE)%>%
  filter(n>=10)
tweet_words
wordcloud2(data=tweet_words,size=0.4) 




terms <- tweets_td%>%
  count(term,sort=T)
terms
terms$var <- 1
totalterms <- terms%>%
  group_by(var)%>%
  summarize(total=sum(n))
totalterms
terms_total <- left_join(terms,totalterms)
terms_total
ggplot(terms_total,aes(n/total))+
  geom_histogram(fill="green",color="darkgreen",show.legend=FALSE)+
  xlim(NA,.010)+
  ggtitle("Term frequency distribution in tweets #meetsouthafrica")

freq_by_rank <- terms_total%>%
  mutate(rank=row_number())%>%
  mutate(tfreq=n/total)
freq_by_rank
p<- ggplot(freq_by_rank,aes(x=rank,y=tfreq))+
  geom_line(size=1,col="red",alpha=0.8,show.legend=FALSE)+
  scale_y_log10() +
  ggtitle("Visualization for Zipf's law for tweets in #meetsouthafrica")
p

dtm_tfidf <- DocumentTermMatrix(corpus,control=list(weighting=function(x)weightTfIdf(x,normalize=FALSE)))
str(dtm_tfidf)
inspect(dtm_tfidf)

m2 <- as.matrix(dtm_tfidf)
tsne_out <- Rtsne(m2,dims=2,check_duplicates=F)
X1 <- tsne_out$Y[,1]
X2 <- tsne_out$Y[,2]
df <- data.frame(X1,X2)
fviz_nbclust(df,clara, method="silhouette") + theme_classic()
clara.res <- clara(df,10,samples=50,pamLike=TRUE)
fviz_cluster(clara.res,
             palette=c("red","green","blue","purple","black","yellow","darkblue","darkred","darkgreen"),
             ellipse.type="t",
             geom="point",pointsize=0.5,title="Cluster plot of t-sne coordinates tf-idf",
             ggtheme=theme_classic())




dd <- cbind(df,cluster=clara.res$cluster)
dd$document <- as.numeric(rownames(dd))
dd$document <- as.character(dd$document)
dd$cluster <- as.factor(dd$cluster)
final <- left_join(tweets_td,dd)
final2 <- final %>%
  group_by(cluster)%>%
  count(cluster,term,sort=TRUE)%>%
  ungroup()
totals <- final2 %>%
  group_by(cluster) %>%
  summarize(total=sum(n))
cluster_words <- left_join(final2,totals)
cluster_words

ggplot(cluster_words, aes(n/total,fill=cluster))+
  geom_histogram(show.legend=FALSE) +
  facet_wrap(~cluster,ncol=2,scales="free_y") +
  ggtitle("term frequency distribution in document clusters")

cluster_words <- cluster_words %>%
  bind_tf_idf(term,cluster,n)
cluster_words

png("tfidf.png", width=1000, height=1000)
cluster_words %>%
  select(-total)%>%
  arrange(desc(tf_idf))%>%
  mutate(term=factor(term,levels=rev(unique(term)))) %>%
  group_by(cluster)%>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(term,tf_idf,fill=cluster)) +
  geom_col(show.legend=FALSE) +
  labs(x=NULL,y="tf_idf") +
  facet_wrap(~cluster,ncol=2,scales="free") +
  coord_flip() +
  ggtitle("highest tf_idf in each document cluster")
dev.off()
doctermcl <- final %>%
  select(document,term,cluster)


tweets_bigrams <-  doctermcl %>%
  unnest_tokens(bigram,term,token="ngrams",n=2)
tweets_bigrams

bigrams <- tweets_bigrams %>%
  count(cluster,bigram,sort=TRUE)
bigrams
bigrams_tfidf <- bigrams %>%
  bind_tf_idf(bigram,cluster,n)%>%
  arrange(desc(tf_idf))
bigrams_tfidf
png("tfidf2.png", width=1200, height=1200)
bigrams_tfidf %>%
  mutate(bigram=reorder(bigram,tf_idf))%>%
  group_by(cluster) %>%
  top_n(10,tf_idf) %>%
  ungroup() %>%
  ggplot(aes(bigram,tf_idf,fill=cluster)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~cluster,ncol=2,scales="free") +
  coord_flip() +
  labs(y="tf_idf of bigrams to cluster",x="")
dev.off()  
bigram_graph <- bigrams %>%
  filter(n>=10) %>%
  graph_from_data_frame()
bigram_graph

set.seed(2456)
ggraph(bigram_graph,layout="fr") +
  geom_edge_link(aes(edge_alpha=n,edge_width=n),edge_color="cyan4") +
  geom_node_point(color="purple",size=4) +
  geom_node_text(aes(label=name),repel=TRUE,point.padding=unit(0.2,"lines"))+
  theme_void()+
  ggtitle("bigram network in tweets #meetsouthafrica")

a <- doctermcl%>%
  mutate(cluster=2)
a
word_cor <- doctermcl %>%
  group_by(term) %>%
  pairwise_cor(term,cluster) %>%
  filter(!is.na(correlation))
word_cor  

word_graph <- doctermcl %>%
  group_by(term) %>%
  filter(n() >=10) %>%
  pairwise_cor(term,cluster) %>%
  filter(!is.na(correlation),correlation >=0.50) %>%
  graph_from_data_frame()

word_graph

set.seed(2456)
ggraph(word_graph,layout="fr") +
  geom_edge_link(aes(edge_alpha=correlation),show.legend=FALSE)+
  geom_node_point(color="purple",size=3) +
  geom_node_text(aes(label=name),repel=TRUE,point.padding=unit(0.1,"lines"))+
  theme_void()+
  ggtitle("network of words based on correlation within clusters")

str(dtm)
term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i],dtm$j,mean)*
  log2(tm::nDocs(dtm)/slam::col_sums(dtm>0))
summary(term_tfidf)

dtm <- dtm[,term_tfidf >=0.7285]
summary(slam::col_sums(dtm))
rowTotals <- apply(dtm,1,sum)
empty.rows <- dtm[rowTotals==0,]$dimnames[1][[1]]
corpus <- corpus[-as.numeric(empty.rows)]
dtm <- DocumentTermMatrix(corpus)
str(dtm)

burnin <- 1000
iter <- 1000
keep <- 50
seqk <- seq(2,100,1)
system.time(fitted_many <- lapply(seqk,function(k)topicmodels::LDA(dtm,k=k,
                                                                   method="Gibbs",control=list(burnin=burnin,iter=iter,keep=keep))))
logLiks_many <- lapply(fitted_many,function(L)L@logLiks[-c(1:(burnin/keep))])
harmonicMean <- function(logLiklihoods,precision=2000L){
  Med <- median(logLiklihoods)
  as.double(Med -log(mean(exp(-mpfr(logLiklihoods,prec=precision)+ Med))))
}
hm_many <- sapply(logLiks_many,function(h)harmonicMean(h))
ldaplot <- ggplot(data.frame(seqk,hm_many),aes(x=seqk,y=hm_many)) + geom_path(lwd=1.5)+
  theme(text=element_text(family=NULL),
        axis.title.y=element_text(vjust=1,size=16),
        axis.title.x=element_text(vjust=0.5,size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20))+
  xlab('number of topics')+
  ylab('harmonic mean') +
  ggtitle("detemining the number of topics")
ldaplot
        
k <- seqk[which.max(hm_many)]
k #14

seedNum <- 50
system.time(ldaOut <- topicmodels::LDA(dtm,k=k,method="Gibbs",control=list(burnin=burnin,iter=2000,keep=keep,seed=seedNum)))
str(ldaOut)
ldaOut.terms <- as.matrix(terms(ldaOut,10))
ldaOut.terms[1:10,]

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("topic_model",k,"DocsToTopicsfinal1.csv"))


topics_beta <- tidy(ldaOut,matrix="beta")
top_terms <- topics_beta %>%
  group_by(topic) %>%
  top_n(5,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
top_terms %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic,scales="free") +
  coord_flip() +
  ggtitle("probabilities of top 10 terms per topic")

topics1_5 <- topics_beta %>%
  group_by(term) %>%
  top_n(1,beta) %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  filter(topic<=5) %>%
  acast(term ~ topic,value.var="beta",fill=0) %>%
  comparison.cloud(title.size=1,colors=brewer.pal(5,"Set1"))
topics6_10 <- topics_beta %>%
  group_by(term) %>%
  top_n(1,beta) %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  filter(topic>=6 & topic <=10) %>%
  acast(term ~ topic,value.var="beta",fill=0) %>%
  comparison.cloud(title.size=1,colors=brewer.pal(5,"Set1"))
topics11_14 <- topics_beta %>%
  group_by(term) %>%
  top_n(1,beta) %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  filter(topic>10 & topic <=18) %>%
  acast(term ~ topic,value.var="beta",fill=0) %>%
  comparison.cloud(title.size=1,colors=brewer.pal(5,"Set1"))


termgenerator <- posterior(ldaOut)$terms
termimportance <- apply(termgenerator,1,function(x)x[order(x,decreasing=T)[1:20]])
termimportance.longform <- melt(termimportance,value.name="Probability",varnames=c("TermNumber","Topic"))
ggplot(data=termimportance.longform,
       aes(x=TermNumber,
           y=Probability,
           color=factor(Topic),
           group=Topic)) +
         geom_line() +
         ggtitle("Term distribution across topics")

meetSA.topics <- topicmodels::topics(ldaOut,1)
meetSA.terms <- as.data.frame(topicmodels::terms(ldaOut,30),stringsAsFactors=FALSE)
topicTerms <- tidyr::gather(meetSA.terms,Topic)
topicTerms <- cbind(topicTerms,Rank=rep(1:30))
topTerms <- dplyr::filter(topicTerms,Rank<4)
topTerms <- dplyr::mutate(topTerms,Topic=stringr::word(Topic,2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for(i in 1:18){
  z <- dplyr::filter(topTerms,Topic==i)
  l <- as.data.frame(paste(z[1,2],z[2,2],z[3,2],sep=" "),stringsAsFactors=FALSE)
  topicLabel <- rbind(topicLabel,l)
}
colnames(topicLabel) <- c("Label")
topicLabel

topicsProb <- read.csv("topic_model 18 DocsToTopicsfinal1.csv")
topic2tweets <- which(topicsProb$V1==1)
tweetscorpus <- corpus
topic2TweetsText <- as.list(tweetscorpus[topic2tweets])
sampleTweets <- sample(topic2TweetsText,5)
sampleTweets

topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"topicProbabilities.csv"))


bgg.post <- posterior(ldaOut)
bgg.cor_mat <- cor(t(bgg.post[["terms"]]))
bgg.cor_mat[bgg.cor_mat <.001] <- 0
diag(bgg.cor_mat) <- 0
bgg.graph <- graph.adjacency(bgg.cor_mat,weighted=TRUE,mode="lower")
bgg.graph <- delete.edges(bgg.graph,E(bgg.graph)[weight<.05])
E(bgg.graph)$edge.width <- E(bgg.graph)$weight*50
V(bgg.graph)$size <- colSums(bgg.post[["topics"]])/200
par(mar=c(0,0,3,0))
set.seed(110)
plot.igraph(bgg.graph,edge.width=E(bgg.graph)$edge.width,
            main="Strength between topics based on word probabilities",
            edge.color="orange",
            vertex.color="red",
            vertex.frame.color=NA)

clp <- cluster_label_prop(bgg.graph)
class(clp)
plot(clp,bgg.graph,main="Community detection in topic network")

