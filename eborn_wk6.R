library(RJSONIO)
library(dplyr)
library(purrr)
library(plotly)
library(SportsAnalytics)

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

# e)
# subset of lowest departing player counts
glitch.players.lowest <- glitch.players[4:10,]

# plot of subset
y <- list(title = "Number of players")
x <- list(title = 'Month')
plot_ly(glitch.players.lowest, x = ~month, y=~departing, name = 'Departing', type='scatter', mode='line')%>% 
  layout(yaxis = y, title = "Departing Glitch Players by Month", xaxis = x)


# 2)
# a)
# pull down 2018-19 season stats
NBA.Stats <- fetch_NBAPlayerStatistics(season = "18-19", what = c("",".Home", ".Away"))

# b)
# subset of only players who made greater than 300 shots
fg <- NBA.Stats[NBA.Stats$FieldGoalsMade > 299,]

# create new column of shot percentage
fg$pct <- fg[,7]/fg[,8]

# find the row equal to the max pct row
# highest field goal percentage
fg[fg$pct == max(fg$pct),]

# c)
# top 10 players in terms of total points

sel.cols <- c('name','TotalPoints')

head(NBA.Stats[order(NBA.Stats$TotalPoints, decreasing = TRUE),], n=10)


