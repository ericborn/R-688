library(tm)
library(shiny)
library(gridExtra)
library(wordcloud)
library(WikipediR)

#######
# Data collection and processing
#######

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

# create a matrix from the dtm
term.matrix <- as.matrix(dtm)

# find the frequency of each word
freq <- colSums(as.matrix(dtm))

# create a decending order or the terms
ord <- order(freq, decreasing = TRUE)

# creates a dataframe containing only the 50 most frequent words
d <- data.frame(name = names(freq[head(ord,n=50)]), value=freq[head(ord,n=50)])

#######
# Server
#######
server <- function(input, output) {
  # creates the render for the word cloud
  output$distPlot1 <- renderPlot({wordcloud(words = d$name, freq = d$value, min.freq = 1,
                                random.order=FALSE, rot.per=0.35,
                                colors=brewer.pal(8, "Dark2"))})
  # outputs the dataframe containing the 50 words and their counts
  output$summary <- renderPrint({
    d
  })
}

#######
# UI
#######
ui <- fluidPage(
  titlePanel("Word Cloud"),
  sidebarLayout(
    # Sidebar
    sidebarPanel(h3("Search panel"),
                 # Where to search 
                 selectInput("select",
                             label = h5("Choose from the following Wiki Pages on"),
                             choices = titles,
                             selected = titles, multiple = TRUE),
                 # Start Search
                 submitButton("Results")
    ),
    # creates main panel which contains the cloud and the summary information
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(
                   plotOutput("distPlot1"),
                   verbatimTextOutput("summary"))
        )),
      tabPanel("Summary",  verbatimTextOutput("summary1"))
    )
  )
)

#######
# Launch app
#######
shinyApp(ui = ui, server = server)