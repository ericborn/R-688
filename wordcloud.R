library(tm)
library(shiny)
library(stringi)
library(tidytext)
library(wordcloud)
library(WikipediR)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)

titles <- c("Web_analytics","Text_mining","Integral", "Calculus", 
            "Lists_of_integrals", "Derivative","Alternating_series",
            "Pablo_Picasso","Vincent_van_Gogh","Leo_Tolstoy","Web_crawler")




# tail(d, 10)
# 
# order(d, decreasing = TRUE)

# # store the count of each word
# freq <- colSums(as.matrix(dtm))
# # create a decending order or the terms
# ord <- order(freq, decreasing = TRUE)

# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2"))
# d$freq

# ui  <- fluidPage(
#   theme = shinytheme('darkly'),
#   titlePanel('Wordcloud Web App'),
#   sidebarLayout(
#     sidebarPanel(
#       #sliderInput('ngramCount', '# of Grams', min = 1, max = 5, value = 2),
#       #hr(),
#       sliderInput('cloudCount', '# of Words', min = 50, max = 400, value = 100)
#     ),
#     mainPanel(
#       plotOutput('wordcloud') 
#     )
#   )
# )

# Define UI for application 
ui  <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  # sidebarLayout(
  #   # Sidebar with a slider and selection inputs
  #   sidebarPanel(h3("Search panel"),
  #                # Where to search 
  #                selectInput("select",
  #                            label = h5("Choose from the following Wiki Pages on"),
  #                            choices = titles,
  #                            selected = titles, multiple = TRUE),
  #                # Start Search
  #                submitButton("Results"),
  #                sliderInput("freq",
  #                            "Minimum Frequency:",
  #                            min = 1,  max = 50, value = 15),
  #                sliderInput("max",
  #                            "Maximum Number of Words:",
  #                            min = 1,  max = 300,  value = 100)
  #   ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plotfdsgv")
    )
  )
#)


server  <- function(input, output) {
  return('im working')
  print('im working')
  output$plot <- renderPlot(function(){
    
    plotWC <- function()
    {
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
      
      # create a matrix from the dtm
      term.matrix <- as.matrix(dtm)
      v <- colSums(as.matrix(dtm)) #sort(rowSums(term.matrix),decreasing=TRUE)
      # create a decending order or the terms
      ord <- order(freq, decreasing = TRUE)
      freq <- colSums(as.matrix(dtm))
      d <- data.frame(name = names(freq[head(ord,n=50)]), value=freq[head(ord,n=50)])
      
      
      wordcloud(words = d$name, freq = d$value, min.freq = 1,
                max.words=50, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    }
    return('I worked')
    plotWC()
  })
}

shinyApp(ui, server)