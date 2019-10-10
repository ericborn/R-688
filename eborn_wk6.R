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

# d)
# plot top 10 points per team
top10.points <- head(aggregate(TotalPoints ~ Team, data = NBA.Stats, FUN=sum), n=10)

y <- list(title = "Total Points")
x <- list(title = 'Teams')
plot_ly(top10.points, x = ~Team, y=~TotalPoints, name = 'Departing', type='bar')%>% 
  layout(yaxis = y, title = "Total Points per Team", xaxis = x)

# top 10 total minutes played
top10.minutes <- head(aggregate(TotalMinutesPlayed ~ Team, data = NBA.Stats, FUN=sum), n=10)

y <- list(title = "Total Minutes")
x <- list(title = 'Teams')
plot_ly(top10.minutes, x = ~Team, y=~TotalMinutesPlayed, name = 'Departing', type='bar')%>% 
  layout(yaxis = y, title = "Total Points per Team", xaxis = x)

# Box plot
# BOXPLOT top 10 beer styles and their ABV's
# Create dataset that contains the top 10 most reviewed ABV's and abv
top.10.abv <- beer[beer$beer_style %in% beer.top10.style.avg$beer_style, c(4,6)]

# Reset rownames from 1 to n
rownames(top.10.abv) <- 1:nrow(top.10.abv)

# drop empty factor levels
top.10.abv <- droplevels(top.10.abv)

### Lists are out of order, ABV diff being applied to the wrong items
### numbers not needed
# # Store min/max ABV's by style
# top.10.abv.min <- aggregate(beer_abv ~ beer_style, top.10.abv, function(x) min(x))
# top.10.abv.max <- aggregate(beer_abv ~ beer_style, top.10.abv, function(x) max(x))
# 
# # calculate difference between min and max abv by style
# max.min.diff <- top.10.abv.max[2] - top.10.abv.min[2]
# 
# # Bind top 10 styles with min/max difference in ABV
# style.top10 <- cbind(style.top10, max.min.diff)
# 
# # Rename column to abv_diff
# colnames(style.top10)[colnames(style.top10)=="beer_abv"] <- "ABV_diff"


# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Beer Style")
x <- list(title = 'ABV')
abv.box <- plot_ly(top.10.abv, x = ~beer_abv, y = ~beer_style, type = 'box',
                   size = 2)%>% 
  layout(xaxis = x, yaxis = y, title = "ABV distribution of top 10 beer styles")

# Draw plot
abv.box