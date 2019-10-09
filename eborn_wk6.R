library(RJSONIO)
library(dplyr)
library(purrr)
library(plotly)

# store the path to the file
webpage <- 'C:/Users/TomBrody/Desktop/School/688 Web/wk6/12months_departures_joiners.json'

# read the json data
data.json <- fromJSON(webpage)

# convert the json to a tibble
nodes.info <- data.json$nodes %>% bind_rows

# a)
# aggregate joining players by month
joining <- aggregate(joining ~ month, data = nodes.info, sum)

# b)
# aggregate departing players by month
departing <- aggregate(departing ~ month, data = nodes.info, sum)

# c)
# merge the joining and departing figures
glitch.players <- merge(joining, departing)

# create a list that contains the proper order for the dates
months <- c('Nov-11', 'Dec-11', 'Jan-12', 'Feb-12', 'Mar-12', 'Apr-12', 'May-12',
            'Jun-12', 'Jul-12', 'Aug-12', 'Sep-12', 'Oct-12', 'Nov-12', 'Dec-12')

# reorder dataframe
glitch.players <- glitch.players[match(months, glitch.players$month),]

# Reset rownames from 1 to n
rownames(glitch.players) <- 1:nrow(glitch.players)

# reset the month factors to be in order
glitch.players$month <- factor(months, levels = months)


# d)
# Plot the joining and departing players
y <- list(title = "Number of players")
x <- list(title = 'Month')
plot_ly(glitch.players, x = ~month, y=~departing, name = 'Departing', type='scatter', mode='line')%>% 
  add_trace(y=~joining, name = 'Joining')%>%
  layout(yaxis = y, title = "Glitch Players by Month", xaxis = x)