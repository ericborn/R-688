# Eric Born
# CS688 week 3
# 22 Sept 2019

# install.packages("tm")
library(tm) # Framework for text mining.
library(class) # Using kNN 

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
# words 2 characters and longer
# only include words in DTM if they appear at least 5 times
full.corpus.DTM <- DocumentTermMatrix(full.corpus.proc, control = list(
  wordLengths=c(2,Inf),
  bounds=list(global=c(5,Inf))  
))
inspect(full.corpus.DTM)

full.corpus.DTM[c(1:200),]

# d)
# First 200 documents are training, next 200 are test
train.doc <- full.corpus.DTM[c(1:200),]
test.doc <- full.corpus.DTM[c(201:400),]
Tags <- factor(c(rep("Sci",100), rep("Rec",100)))

test.doc[1,]

# set seed for results repeatability
set.seed(1337)

# run knn with k between 2 and 7
x <- c(2,3,4,5,6,7)

for (i in x){
  prob.test <- knn(train.doc, test.doc, Tags, k = i, prob=TRUE)
  # Display Classification Results
  a <- 1:length(prob.test)
  b <- levels(prob.test)[prob.test]
  c <- attributes(prob.test)$prob
  d <- prob.test==Tags
  
  result <- data.frame(Doc=a, Predict=b,Prob=c, Correct=d)

  print(paste(i, sum(prob.test==Tags)/length(Tags), sep=' '))
}

# Best value for k is 3 or 4 at around 69% accuracy
prob.test <- knn(train.doc, test.doc, Tags, k = 3, prob=TRUE)

# Display Classification Results
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test==Tags

result <- data.frame(Doc=a, Predict=b,Prob=c, Correct=d)

# output only the first and last 6 rows
result[c(1:6, 195:200),]

# e)
# k = 3, 69%
sum(prob.test==Tags)/length(Tags)
