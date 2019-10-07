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


# Part B
# 1)