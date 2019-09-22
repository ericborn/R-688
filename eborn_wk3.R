# Module 3 Code
# install.packages("pdftools")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("tau")
# install.packages("arules")
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(dplyr) # Data preparation and pipes %>%.
library(ggplot2) # Plot word frequencies.
library(scales) # Common data analysis activities.
library(pdftools)

#set the working directory
setwd("c:/Users/TomBrody/Desktop/School/688 Web/wk3/20Newsgroups/20news-bydate-train")

# a)
# sci.space and rec.autos
# load the sci.space training files
sci.train.path <- system.file('train',"sci.space",package="tm")
sci.train.files <- DirSource(sci.train.path)
sci.train.corpus <- Corpus(URISource(sci.train.files$filelist[1:100]), 
                           readerControl=list(reader=readPlain))

# load the sci.space test files
sci.test.path <- system.file('test',"sci.space",package="tm")
sci.test.files <- DirSource(sci.test.path)
sci.test.corpus <- Corpus(URISource(sci.test.files$filelist[1:100]), 
                           readerControl=list(reader=readPlain))

# load the rec.auto training files
rec.train.path <- system.file('train',"rec.autos",package="tm")
rec.train.files <- DirSource(rec.train.path)
rec.train.corpus <- Corpus(URISource(rec.train.files$filelist[1:100]), 
                           readerControl=list(reader=readPlain))

# load the rec.auto test files
rec.test.path <- system.file('test',"rec.autos",package="tm")
rec.test.files <- DirSource(rec.test.path)
rec.test.corpus <- Corpus(URISource(rec.test.files$filelist[1:100]), 
                          readerControl=list(reader=readPlain))

# merge all four collections of documents into a single corpus
full.corpus <- c(sci.train.corpus, rec.train.corpus, sci.test.corpus, rec.test.corpus)

# b)
# lower case
full.corpus.proc <- tm_map(full.corpus, content_transformer(tolower))
# remove stopwords
full.corpus.proc <- tm_map(full.corpus.proc, removeWords, stopwords("english"))
#remove punctuation
full.corpus.proc <- tm_map(full.corpus.proc, removePunctuation)

# Output of the summary line that contains uppercase letters, stop words and punctuation
# "Summary: Dong ....  Dong ....  Do I hear the death-knell of relativity?"
full.corpus[[1]]$content[3]

# Output showing that all three have been properly removed
# "summary dong   dong     hear  deathknell  relativity"
full.corpus.proc[[1]]$content[3]

# c)
# document term matrix
Mac.DTM <- DocumentTermMatrix(Mac.Corpus.Proc, control = list(
  wordLengths=c(3,20),  # words between 3 and 20 characters long
  bounds=list(global=c(20,Inf))  # only include words in DTM if they happen in 20 or more documents
))
inspect(Mac.DTM)


