install.packages("dplyr")
install.packages("ggpubr")
install.packages("mvShapiroTest")
install.packages("janeaustenr")
install.packages("tidytext")
install.packages("textdata")
library("textdata")
library("tidytext")
library("janeaustenr")
library("mvShapiroTest")
library("ggpubr")
library("dplyr")
caps <- read.csv("allbloggers.csv")
head(caps)
caps$Name <- NULL
caps$Description <- NULL
caps
rownames(caps) <- caps$Screenname
caps$Screenname <- NULL
caps
ggqqplot(caps$status_count)
ggdensity(caps$Friends, 
          main = "Density plot of friends",
          xlab = "friends")

test_cols <- caps[,c("status_count","Friends","Account.age")]
test_cols
res <- cor(test_cols)
print(res)
heatmap.2(x = res, col = col, symm = TRUE,dendrogram='none',Rowv=FALSE,margins=c(10,12), key=TRUE)
ggqqplot(caps$Account.age)
ggdensity(caps$Account.age, fill = "lightgray")
mvShapiro.Test(caps$status_count)
mvShapiro.Test(caps$Friends)
mvShapiro.Test(caps$Account.age)
caps %>% shapiro_test(status_count, Friends,Account.age)
model <- lm(log(Followers)~Average.tweets,data=caps)
summary(model)
model1 <- lm(log(Followers)~status_count+Friends+Account.age + Average.tweets,data=caps)
summary(model1)
png("modelcaps.png", width=500, height=500)
par(mfrow = c(2,2))
plot(model1)
dev.off()


caps_new <- read.csv("allbloggers.csv")
caps_new
desacc = caps_new %>% select(Screenname,Description)
desacc
desacc$stripped_text1 <- gsub("https\\S+","",desacc$Description)
desacc_stem <- desacc %>% select(stripped_text1) %>% unnest_tokens(word,stripped_text1)
head(desacc_stem)
cleaned_desacc <- desacc_stem %>% anti_join(stop_words)
head(cleaned_desacc)
cleaned_desacc %>%
  count(word,sort =TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) +
  geom_col() +
  xlab(NULL)+
  coord_flip() +
  theme_classic() +
  labs(x="count",
       y="unique words",
       title ="unique words found in description of Twitter accounts")
#--------------------------------------------------------
bing_desacc = cleaned_desacc %>% inner_join(get_sentiments("bing")) 
bing_desacc_n= bing_desacc %>%
 count(word,sentiment,sort = TRUE) %>%
 ungroup()
------------------------------------------------------------
bing_desacc_n %>%
  group_by(sentiment)  %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x=word,y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales ="free_y") +
  coord_flip() +
  theme_bw() +
  labs(x=NULL,
       y="contribution to sentiment",
       title ="words used to describe Twitter accounts")
