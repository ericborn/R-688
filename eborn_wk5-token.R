# Eric Born
# CS 688
# 06 Oct 2019

# load packages
library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

### Place your own app name and unique codes here!!


# Authenticate via access token
create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token, 
             access_secret = access_secret)

# 1)
# Belize and Bali

# 2)
# Search for tweets containing belize
belize <- search_tweets(
  "#belize", n = 1000, include_rts = FALSE
)
belize_tweets = belize %>% select(screen_name,text,created_at)
belize_tweets

bali <- search_tweets(
  "#bali", n = 1000, include_rts = FALSE
)
bali_tweets = bali %>% select(screen_name,text,created_at)
bali_tweets

# 3)
# strip any text that contains a URL
belize_tweets$stripped_text <- gsub("http\\S+","",belize_tweets$text)
bali_tweets$stripped_text <- gsub("http\\S+","",bali_tweets$text)

# stem words using unnest tokens from tidytext
belize_stem <- belize_tweets %>%
  select(stripped_text) %>%
  tidytext::unnest_tokens(word, stripped_text)

bali_stem <- bali_tweets %>%
  select(stripped_text) %>%
  tidytext::unnest_tokens(word, stripped_text)

# observe that stop words are still present
head(belize_stem)
head(bali_stem)

# remove stop words
cleaned_belize <- belize_stem %>%
  anti_join(tidytext::get_stopwords())

cleaned_bali <- bali_stem %>%
  anti_join(tidytext::get_stopwords())

# observe data
head(cleaned_belize)
head(cleaned_bali)

# 4)
# top 10 words from Belize
cleaned_belize %>%
  count(word, sort = TRUE) %>%
  top_n(10)

# top 10 words from Bali
cleaned_bali %>%
  count(word, sort = TRUE) %>%
  top_n(10)

# 5)
# Find pairs of words
belize_pairs = belize_tweets %>%
  select(stripped_text) %>%
  tidytext::unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)

bali_pairs = bali_tweets %>%
  select(stripped_text) %>%
  tidytext::unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)

# split the pairs of words into two columns
belize_pairs_separate = belize_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")

bali_pairs_separate = bali_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")

# remove stop words from the pairs
belize_pairs_clean <- belize_pairs_separate %>%
  filter(!Word1 %in% tidytext::get_stopwords()) %>%
  filter(!Word2 %in% tidytext::get_stopwords())

bali_pairs_clean <- bali_pairs_separate %>%
  filter(!Word1 %in% tidytext::get_stopwords()) %>%
  filter(!Word2 %in% tidytext::get_stopwords())

# find the top word pairs
belize_pairs_counts <- belize_pairs_clean %>%
  count(Word1, Word2, sort = TRUE)

bali_pairs_counts <- bali_pairs_clean %>%
  count(Word1, Word2, sort = TRUE)

# 6)
# using cleaned tibbles created in step 3
cleaned_belize
cleaned_bali

# joining on the bing lexicon for sentiment analysis
bing_belize = cleaned_belize %>%
  inner_join(tidytext::get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_bali = cleaned_belize %>%
  inner_join(tidytext::get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

head(bing_belize)
head(bing_bali)

# function to calculate sentiment on each word
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    tidytext::unnest_tokens(word,stripped_text) %>% 
    anti_join(tidytext::stop_words) %>%  #remove stop words
    inner_join(tidytext::get_sentiments("bing")) %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

# create a list of all sentiment scores
belize_sent = lapply(belize_tweets$text,function(x){sentiment_bing(x)})
bali_sent = lapply(bali_tweets$text,function(x){sentiment_bing(x)})

# create a tibble holding both countries sentiment scores
country_sentiment = bind_rows(
  tibble(
    country = 'belize',
    score = unlist(map(belize_sent,'score')),
    type = unlist(map(belize_sent,'type'))
  ),
  tibble(
    country = 'bali',
    score = unlist(map(bali_sent,'score')),
    type = unlist(map(bali_sent,'type'))
  )
)

# create a summary of the sentiments per country
# Filter only on words that are in the bing lexicon
country_sentiment %>% filter(type != "Type 1") %>% group_by(country) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )










user1 = lookup_users('hugobowne') #put any user here!

glimpse(user1) # description of your user1. You can do it this way, 

user1 %>% glimpse() # or practice using the pipe operator!

user1$screen_name
user1$name
user1$description
user1$text
user1$user_id

users_test = c('cnnbrk','rstatstweet')

manyusers = lookup_users(users = users_test)


# get the followers' IDs of user1

get_followers(user = user1$user_id, n = 10)

# get the followers' IDs of the second user in the manyusers tibble created.

get_followers(user = manyusers$user_id[[2]], n = 10)

# save followers and input them into the lookup_users() function
# to get information on all of the followers. 

followers1 = get_followers(user = user1$user_id, n = 10)

followers1Info = lookup_users(followers1$user_id)

get_friends(user1$user_id,n = 10)

# get tweets from a particular user

tweetscnn = get_timeline(user = 'cnnbrk', n = 2000) # takes 10-20 seconds to run
ts_plot(tweetscnn, "days") + theme_classic()


tweetsNews = get_timeline(user = c('cnnbrk','foxnewsalert'), n = 2000) # takes 10-20 seconds to run
ts_plot(tweetsNews %>% group_by(screen_name),'days') + theme_classic()

get_my_timeline(n = 5)

### --- Trends (OPTIONAL)

trendlocations = trends_available()
head(trendlocations)

trendlocations %>% group_by(country) %>% 
  summarise(
  Total = n()
  ) %>% 
  arrange(desc(Total)) %>% head(10) %>%
  ggplot(aes(reorder(country, Total), Total, fill = country)) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Number of Trending Locations by Country", x="", 
       subtitle="The US has the largest number of trending locations", 
       caption = "\nSource: Data collected from Twitter's API via rtweet") +
  theme_classic()

# Get the trends for San Francisco

## US locations

usaTrends = trendlocations %>% filter(country == "United States")

head(usaTrends)

sftrends = get_trends("San Francisco")

display.trends = function(trend){
  cat("Name:", trend$trend, "\n url:", trend$url, "\n\n")
}

invisible(lapply(1:nrow(sftrends),function(x){display.trends(sftrends[x,])}))

### --- Network Analysis:
# Diagram the connections between followers/friends and the target account(s)
## First - get friends of multiple accounts
news_frnds <- get_friends(c("cnnbrk", "foxnews", "MSNBC"))
## frequency count of accounts followed by the users queried above
tbl <- table(news_frnds$user_id)
## subset news_frnds data to only those followed by 2 or more (e.g., at least any 2 of the 3 news accounts)
news_frnds1 = news_frnds %>% filter(user_id %in% names(tbl[tbl > 1L]))
## Use the lookup_users() function to get screen name of users.
news_frnds2 = left_join(news_frnds1,distinct(news_frnds1,user_id) %>% 
              mutate(names = lookup_users(user_id)$screen_name), by = 'user_id') %>% 
  select(-user_id)
## convert to matrix
mat <- as.matrix(news_frnds2)
mat
## load igraph library and convert to graph object
library(igraph)
i.mat <- graph_from_edgelist(mat)
# Edges
E(i.mat)
#Vertices
V(i.mat)
#Change color of the vertices of the main accounts
V(i.mat)[c("cnnbrk","MSNBC","foxnews")]$color = 'red'
## plot network
plot(i.mat,layout = layout_nicely, vertex.size=6,
     vertex.label.dist=1, edge.arrow.size=0.5)

### --- Text Mining using TidyText
library(rtweet)
library(tidytext)
library(tidyverse)
library(SnowballC)
source("~/Documents/twitteR.r")
create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token, 
             access_secret = access_secret)

# kaggle = lookup_users('kaggle')

twt.kag = get_timeline(user = 'kaggle',n = 100)

head(twt.kag$text)

# Remove http elements manually
twt.kag$stripped_text <- gsub("http\\S+","",twt.kag$text)
twt.kag$stripped_text <- gsub("[^\u0020-\u007F]+","",twt.kag$stripped_text)
twt.kag$stripped_text <- gsub("'|’","",twt.kag$stripped_text)

head(twt.kag$stripped_text)

# library(tm)
# myCorpus <- VCorpus(VectorSource(twt.kag$stripped_text))
# myCorpus <- 
#  OR 

# use the unnest_tokens() function to convert to lowercase, 
# remove punctuation, and add id for each tweet

twt.kag_clean <- twt.kag %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  mutate(word = wordStem(word))

head(twt.kag_clean)

# plot the top 10 words -- notice any issues?

library(ggplot2)

twt.kag_clean %>% 
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Kaggle tweets")

# Load stop words from the tidytext package
# And remove stop words from your list of words
data("stop_words")
cleaned_twt.kag <- twt.kag_clean %>%
  anti_join(stop_words)

# this is possibly a better way
cleaned_twt.kag <- twt.kag %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words) # %>%
  # mutate(word = wordStem(word))


# Replot with stop words removed.

cleaned_twt.kag %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Kaggle tweets")

### --- Explore words that occur together in pairs!
## note we return back to original text in twt.kag
twt.kag_pairs = twt.kag %>%
  select(stripped_text) %>%
  unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)
head(twt.kag_pairs)
twt.kag_pairs_counts <- twt.kag_pairs %>%
  count(pairs, sort = TRUE)
head(twt.kag_pairs_counts)

## Now if we want to take out the stop words here too, do this:
## By using the separate() function with the option sep =  " ", we are saying to 
## split contents of the column "pairs" whenever there is a space (" ")
## Call the new columns "Word1" and "Word2"

library(tidyr)

twt.kag_pairs_separate = twt.kag_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")
head(twt.kag_pairs_separate)

twt.kag_pairs_clean <- twt.kag_pairs_separate %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)
head(twt.kag_pairs_clean)

# new bigram counts:
twt.kag_pairs_counts <- twt.kag_pairs_clean %>%
  count(Word1, Word2, sort = TRUE)
head(twt.kag_pairs_counts)

### --- Word network (which words associate with which others?)
## now lets plot this

library(igraph)
library(ggraph)

# plot Kaggle tweet word network

twt.kag_pairs_counts %>% 
  filter(n >= 2) %>% #only choose pairs that have more than 2 counts
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Kaggle Tweets",
       subtitle = "Pairs",
       x = "", y = "") +
  theme_bw()

### --- Word Cloud

library(wordcloud)
twt.kag.counts = cleaned_twt.kag %>% count(word)
wordcloud(words = twt.kag.counts$word, freq =twt.kag.counts$n, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
library(stringr)

### --- Word count

remove_reg <- "&amp;|&lt;|&gt;"

tidy_tweets <- twt.kag %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"))

frequency <- tidy_tweets %>% 
  count(word, sort = TRUE) 
frequency
head(frequency)


### --- Sentiment Analysis

library(tidytext)

get_sentiments('nrc')
get_sentiments('bing')

#patriots vs rams

# collect tweets

# these next two commands take a minute
pats = search_tweets(c('patriots'),n = 1000)
rams = search_tweets(c('rams'),n = 1000)

# clean them up!

pats_twts = pats %>% 
  mutate(
  # Remove http elements manually
  stripped_text = gsub("http\\S+","",text)
  ) %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)

rams_twts = rams %>% 
  mutate(
    # Remove http elements manually
    stripped_text = gsub("http\\S+","",text)
  ) %>%  
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)

## bing sentiment analysis

bing_pats = pats_twts %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_rams = rams_twts %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_pats %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing 'patriots'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

bing_rams %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing 'rams'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()


## Calculate score for each tweet


sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
      twt_tbl = tibble(text = twt) %>% 
        mutate(
          # Remove http elements manually
          stripped_text = gsub("http\\S+","",text)
        ) %>% 
      unnest_tokens(word,stripped_text) %>% 
        anti_join(stop_words, by="word") %>%  #remove stop words
      inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
        count(word, sentiment, sort = TRUE) %>% 
        ungroup() %>% 
        ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
        mutate(
          score = case_when(
     sentiment == 'negative'~ n*(-1),
     sentiment == 'positive'~ n*1)
   )
    ## Calculate total score
      sent.score = case_when(
        nrow(twt_tbl)==0~0, # if there are no words, score is 0
        nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
        )
      ## This is to keep track of which tweets contained no words at all from the bing list
      zero.type = case_when(
        nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
        nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
      )
      list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}



sentiment_bing("Hello, how are you, I am good. I hate my shoes, but I love the weather today!")

sentiment_bing("I can't stand how much I'm forced to work, it is so tiring. Help!")

sentiment_bing("Chocolate is the best! I love love love it!")

sentiment_bing("Off to work today")

# Sentiment chart for Pats only
pats_sent = lapply(pats$text,function(x){sentiment_bing(x)}) # -- takes a bit of time
# pats_sent = lapply(pats$text,sentiment_bing)
# pats_sent = lapply(unique(pats$text),sentiment_bing)  # eliminate tweets that say the exact same thing
pats_sentiment = tibble(
  team = 'pats',
  score = unlist(map(pats_sent,'score')),
  type = unlist(map(pats_sent,'type'))
)
ggplot(pats_sentiment,aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6) + theme_bw()
ggplot(pats_sentiment %>% filter(type != "Type 1"),aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6) + theme_bw()

# Compare sentiment for both Pats and Rams searches

# takes a minute or so
pats_sent = lapply(pats$text,function(x){sentiment_bing(x)})
rams_sent = lapply(rams$text,function(x){sentiment_bing(x)})


library(purrr)

nfl_sentiment = bind_rows(
  tibble(
  team = 'pats',
  score = unlist(map(pats_sent,'score')),
  type = unlist(map(pats_sent,'type'))
  ),
  tibble(
    team = 'rams',
    score = unlist(map(rams_sent,'score')),
    type = unlist(map(rams_sent,'type'))
  )
)

ggplot(nfl_sentiment,aes(x=score, fill = team)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~team) + theme_bw()

nfl_sentiment %>% group_by(team) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

## Now lets excluded the Type 1 tweets (tweets with no words in the bing list)

ggplot(nfl_sentiment %>% filter(type != "Type 1"),aes(x=score, fill = team)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~team) + theme_bw()

nfl_sentiment %>% filter(type != "Type 1") %>% group_by(team) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )


### --- REDDIT

library(RedditExtractoR)

subreddit.topics = 'World News'

search.topics = 'Korea'

reddit_links = reddit_urls(
  search_terms = search.topics,
  subreddit = subreddit.topics,
  sort_by = 'new',
  page_threshold = 1
)

glimpse(reddit_links)
reddit_links$title

# Grab Reddit comments

topic_url = reddit_links$URL[3]

reddit_thread = reddit_content(topic_url)
reddit_thread  # check that we got content - most recent post may not have content

reddit_comments =  reddit_thread %>% mutate(
  # Remove http elements manually
  stripped_text = gsub("http\\S+","",comment)
) %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  mutate(word = wordStem(word)) %>%
  anti_join(stop_words)

reddit_comments %>%
    count(word, sort = TRUE) %>%
    top_n(10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme_classic() +
    labs(x = "Count",
         y = "Unique words",
         title = "Unique word counts found in Reddit comments")
  
reddit_comments %>%
    count(word, sort = TRUE) %>%
    top_n(10)
  

bing_reddit = sentiment_bing(reddit_thread$comment)  

reddit_tib = tibble(
  term = 'Korea',
  score = unlist(map(bing_reddit,'score')),
  type = unlist(map(bing_reddit,'sentiment'))
)


ggplot(reddit_tib %>% filter(type != "Type 1"),aes(x=score)) + 
  geom_histogram(bins = 15, alpha = .6) + 
  labs(title = "Sentiment Analysis -  Subreddit: World News",
       subtitle = 'Korea',
                     y = "Count",
                     x = 'Score') + 
  theme_bw()
