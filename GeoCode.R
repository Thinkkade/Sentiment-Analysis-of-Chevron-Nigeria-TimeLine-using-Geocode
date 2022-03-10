#Step 1: Load the required packages (including rtweet) in RStudio
library(rtweet)
library(textdata)
library(janeaustenr)
library(reshape2)
library(devtools)
library(widyr)
library(lubridate) # Date & Time
library(ggplot2)# Data Visualisation
library(dplyr) #Data Wrangling
library(tidytext) #Text Mining
library(tm) #Text Mining
library(wordcloud)
library(readr)
library(tidyr) #Tidy Text
library(RColorBrewer) #Data Visualisation
library(stringr) #String Manipulation
library(RSentiment) #Sentiment Analysis
library(cowplot) #Plot Arrange
library(ggthemes) #Data Visualisation
library(knitr)
library(kableExtra)
library(tm)
library(stopwords)
library(remotes)
library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)


#Step 2: Authenticate using your credentials to Twitter's API by creating an access token. Steps on getting Twitter access tokens:
twitter_token <- create_token(app = "Thinkkade", 
                              api_key <- "d7QTHrtPvAZ5o8Q2Bq9lfCXOS",
                              api_secret <- "MKTwYT3Ts7n6mS3StLRhWk3lBB8794Rk0ZwJVGteSM6e4qdi8e",
                              access_token <- "415803231-IGs7RNCfM0td5eVQ8i5aTeK2c1xDL3dvMZHpg9RP",
                              access_token_secret <- "VOsZcyYWV0PzTvZzdawchqxeWX8LiIK86NpAuSV85Xm4w",
                              set_renv = FALSE)

api_key <- "0H4vX3CkqWnoijZ2oisoaa7zo"
api_secret <- "1t0Mcnu8mZX3FrAoygYWYwfNXJOj09DrDGjVjtHuryyCVQNY39"
access_token <- "415803231-zy660bncbZR5RoJSsgnkkGeV4aMILpyJtOEyyupi"
access_token_secret <- "Nn0vYP6hmtcD0oXjzSbvQsFhwdzEyAWDKgT9nckCBXn2p"



#Step 2: Authenticate using your credentials to Twitter's API by creating an access token. Steps on getting Twitter access tokens:
setup_twitter_oauth("0H4vX3CkqWnoijZ2oisoaa7zo",
                    "1t0Mcnu8mZX3FrAoygYWYwfNXJOj09DrDGjVjtHuryyCVQNY39",
                    "415803231-zy660bncbZR5RoJSsgnkkGeV4aMILpyJtOEyyupi",
                    "Nn0vYP6hmtcD0oXjzSbvQsFhwdzEyAWDKgT9nckCBXn2p")

#Step 3: search tweets
chevron <- get_timeline("@chevron", n = 1500, geocode = "9.066667, 7.483333, 200mi", lang = "en")
chevron



chevron <- apply(chevron,2,as.character)
write.csv(chevron, "chevron.csv")



# read in our data
f <- file.choose("chevron.csv")
chevron <- read.csv(f)


#Step 4: Process each set of tweets into tidy text or corpus objects.
tweets.chevron = chevron %>% select(screen_name,text)
tweets.chevron

#Step 5: Use pre-processing text transformations to clean up the tweets; this includes stemming words. An example of stemming is rolling the words "computer", "computational" and "computation" to the root "comput".
head(tweets.chevron$text)
#Remove http elements manually
tweets.chevron$stripped_text1 <- gsub("http\\s+","",tweets.chevron$text)
#vuse the unnest_tokens() function to convert to lowercase,
#remove punctuation, and add id for each tweet
tweets.chevron_stem <- tweets.chevron %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)
head(tweets.chevron_stem)
# remove stop words from your list of words
cleaned_tweets.chevron <- tweets.chevron_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.chevron) 
head(chevron)


#Step6: The top 30 commonly used words in the set of tweets for chevron; this gives an overall picture of what people are most concerned about, and the extent to which they are engaged on the topic.#Top 10 words in #Covid_19 tweets
cleaned_tweets.chevron %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "count",
       y = "Unique words",
       title = "Unique word counts found in #chevronNGtweets")



#Step 7A: Perform sentiment analysis using the Bing lexicon and get_sentiments function from the tidytext package.
#bing sentiment analysis
bing_chevron = cleaned_tweets.chevron %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_chevron


#Step 7B: Then to visually depict the word counts, you can filter and plot the words side-by-side to compare the positive vs negative emotion.
bing_chevron %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing #Chevron",
       y = "contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()




#Step 9: Then to visually depict the word counts, you can filter and plot the words side-by-side to compare the positive vs negative emotion(7B Contd)
bing_chevron %>%
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(30) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  # Use aes() to put words on the x-axis and n on the y-axis
  ggplot(aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  geom_text (aes(label = n, hjust=1), size = 3.5, color = "black")+
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ggtitle("Most common positive and negative words #chevronNGtweets")


#Step 10: Bing overall sentiment.
bing_chevron %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1000)) +
  ggtitle("chevronNGtweets Bing Sentiment") +
  coord_flip()




#Step 11: wordcloud plot the 50 most common words

pal <- brewer.pal(8,"Dark2")

tidy_chevron = cleaned_tweets.chevron %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 10, colors=pal))

#Sentiment word cloud: Classifying the words into different types of emotions also helps us understand how people are feeling towards a subject

#Wordcloud:the most common positive and negative words
tidy_chevron = cleaned_tweets.chevron %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 40)
head(chevron)
# read in our data
f <- file.choose("chevron.csv")
tweets <- read.csv(f)


install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages(wordcloud2)
library(wordcloud2)

install.packages("tm")
library(tm)
#Create a vector containing only the text
text <- data$text
# Create a corpus  
docs <- Corpus(VectorSource(text))

gsub("https\\S*", "", tweets$text) 
gsub("@\\S*", "", tweets$text) 
gsub("amp", "", tweets$text) 
gsub("[\r\n]", "", tweets$text)
gsub("[[:punct:]]", "", data$text)

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

tweets_words <-  tweets %>%
  select(text) %>%
  unnest_tokens(word, text)
words <- tweets_words %>% count(word, sort=TRUE)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,   
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))


cleaned_tweets.chevron<- data.frame(txt = chevron$text,
                                  stringsAsFactors = FALSE)


cleaned_tweets.chevron %>%
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot(x=word, y=n) + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams in African chevron tweets")
