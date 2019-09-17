# Eric Born
# Part C:

# Import libraries
library(googleAnalyticsR)
library(ggplot2)
library(plotly)

# Setup account auth
ga_auth()
my_accounts <- ga_account_list()
my_id <- my_accounts$viewId[1]

# set date ranges
start_date <- "2019-09-02"
end_date <- "2019-09-16"

# 1)
# Gathers data based upon the devices OS
os <- google_analytics(my_id, date_range=c(start_date, end_date), 
                       metrics = metrics, 
                       dimensions = "operatingSystem")

# Generates a bar plot of the sessions by OS data
plot_ly(data = os, y = ~operatingSystem, x = ~sessions,
        type = 'bar', text = os$sessions, textposition = 'auto') %>%
  layout(title = 'Total sessions by OS',
         yaxis = list(title = 'OS Type'), xaxis = list(title = 'Sessions'))

# 2)
# Gathers data based upon new vs returning visitors
visitor <- google_analytics(my_id, date_range=c(start_date, end_date), 
                            metrics = 'users', 
                            dimensions = 'userType')

# Generates a bar plot of the sessions by visitor type
plot_ly(data = visitor, y = ~userType, x = ~users,
        type = 'bar', text = visitor$users, textposition = 'auto') %>%
  layout(title = 'Total Visitor by type', 
         yaxis = list(title = 'Visitor Type'), xaxis = list(title = 'Totals'))

# 3)
# Gathers data based upon language
language <- google_analytics(my_id, date_range=c(start_date, end_date), 
                            metrics = 'users', 
                            dimensions = 'language')

# Generates a bar plot of the sessions by language
plot_ly(data = language, y = ~language, x = ~users,
        type = 'bar', text = language$users, textposition = 'auto') %>%
  layout(title = 'Total users by language',
         yaxis = list(title = 'Language'), xaxis = list(title = 'Users'))

# 4) 
# setup a vector with metrics to use
mets <- c('users', 'pageviews')

# setup a vector with the dimension types
dims <- c('userType', 'language', 'operatingSystem')

# creates a dataframe from the userType, language and OS dimensions
user.table <- google_analytics(my_id, date_range=c(start_date, end_date), 
                               metrics = mets, 
                               dimensions = dims)
# output the dataframe
user.table

# Part D:

# Version below has no comments and is 16 lines of code

# required library for downloading the stock data
library(quantmod)

# stock.data fucntion
stock.data <- function(ticker) {
  for (i in 1:length(ticker)) {
    cat('\nDiv info for the', toString(ticker[i][[1]]), 'ticker\n')
    ticker.divs <- getDividends(ticker[i][[1]], auto.assign = F)
    ticker.matrix <- as.matrix(ticker.divs)
    ticker.index <- tail(which(diff(ticker.matrix) > 0), 1) + 1
    ticker.divamt <- ticker.matrix[ticker.index]
    ticker.chgdate <- rownames(ticker.matrix)[ticker.index]
    ticker.ntimes <- sum(ticker.matrix == ticker.matrix[ticker.index])
    cat(
      'currentDivAmt', ticker.divamt,
      '\ndateDivAmtLastChanged', ticker.chgdate,
      '\nPayoutsAtCurrDivAmt', ticker.ntimes, '\n'
    )
  }
}

# Full code with comments

# required library for downloading the stock data
library(quantmod)

# Creates the function called stock.data.
# This function takes 1 input which is called ticker.
stock.data <- function(ticker) {
  # since we want to be able to check more than 1 ticker at a time
  # We start with a for loop that iterates from 1 to the length of the
  # ticker input. This allows data collection for as many tickers as we want.
  for(i in 1:length(ticker)){
    # We display the tickers name at the start of the loop
    cat('\nDiv info for the', toString(ticker[i][[1]]),'ticker\n')
    
    # calls the getDividends function from the quantmod library
    # this function downloads the date the dividend changed and the div value.
    # auto.assign=F prevents the function from loading the data to the env
    ticker.divs <- getDividends(ticker[i][[1]], auto.assign=F)
    
    # transforms the data into a matrix
    ticker.matrix <- as.matrix(ticker.divs)

    # The purpose of this is to find the most recent time the dividend changed values.
    # diff returns true if the value after it is different, false if it's the same.
    # which checks if a value is true or false, since we're comparing it >0
    # its only returning values of true, which is a 1.
    # tail with the ,1 retrieves only the very last item from the ticker.matrix object
    # where the which was true. +1 is added at the end to adjust the index position
    # to fall on the most recent position where a change happened.
    ticker.index <- tail(which(diff(ticker.matrix)>0),1)+1

    # Now that the index position has been determined its plugged in as the index
    # to the ticker.matrix object to find the div amount on the latest change.
    ticker.divamt <- ticker.matrix[ticker.index]
    
    # Same as the last, using index to find the row name, which contains the date
    # of the latest change to the div amount
    ticker.chgdate <- rownames(ticker.matrix)[ticker.index]
    
    # calculates the number of times the ticker has had the same dividend amount
    # as the latest value that was found in the ticker.index
    ticker.ntimes <- sum(ticker.matrix==ticker.matrix[ticker.index])
    
    # outputs all of the three values that were just calculated, div amount, last change
    # date and the number of times the ticker has had the same div value.
    cat(
      'currentDivAmt', ticker.divamt,
      '\ndateDivAmtLastChanged', ticker.chgdate,
      '\nPayoutsAtCurrDivAmt', ticker.ntimes, '\n'
    )
  }
}

# builds a list for all three tickers
stock.list <- list('PG', 'msft', 'TGT')

# Runs the list through the function and outputs results
stock.data(stock.list)