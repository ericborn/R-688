library(tm)
library(tidytext)
library(stringi)
library(WikipediR)


titles <- c("Web_analytics","Text_mining","Integral", "Calculus", 
            "Lists_of_integrals", "Derivative","Alternating_series",
            "Pablo_Picasso","Vincent_van_Gogh","Leo_Tolstoy","Web_crawler")

articles <- lapply(titles,function(i) page_content("en","wikipedia", page_name = i,
                                                   as_wikitext=TRUE)$parse$wikitext)

docs <- VCorpus(VectorSource(articles)) # Get Web Pages' Corpus
remove(articles)

# Text analysis - Preprocessing 
transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
temp <- tm_map(docs, transform.words, "<.+?>", " ")
temp <- tm_map(temp, transform.words, "\t", " ")
temp <- tm_map(temp, content_transformer(tolower)) # Conversion to Lowercase
temp <- tm_map(temp, PlainTextDocument)
temp <- tm_map(temp, stripWhitespace)
temp <- tm_map(temp, removeWords, stopwords("english"))
temp <- tm_map(temp, removePunctuation)
temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
remove(docs)

# Create Dtm 
dtm <- DocumentTermMatrix(temp)
# convert dtm into a tibble
df <- tidy(dtm)

# sort by largest count and limit to rows 1:50
words <- df[order(-df$count)[1:50],][2]
count <- df[order(-df$count)[1:50],][3]

