# required library for downloading the stock data
library(quantmod)  # stock price functions

# Full code with comments
# Creates the function called stock.data.
# This function takes 1 input which is called ticker
stock.data <- function(ticker) {
  # since we want to be able to check more than 1 ticker at a time
  # We start with a for loop that iterates from 1 to the length of the
  # ticker input.
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


#### Version below has no comments and is 16 lines of code

# required library for downloading the stock data
library(quantmod)  # stock price functions

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