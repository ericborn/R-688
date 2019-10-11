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
fg[fg$pct == max(fg$pct),c(2,7,8,26)]

# c)
# top 10 players in terms of total points
head(NBA.Stats[order(NBA.Stats$TotalPoints, decreasing = TRUE),c(2,3,21)], n=10)

# d)
# 1)
# plot top 10 points per team
# gather total points per team
top.points <- aggregate(TotalPoints ~ Team, data = NBA.Stats, FUN=sum)

# only select top 10 teams by total points
top10.points <- head(top.points[order(top.points$TotalPoints, decreasing = TRUE),], n=10)

# Reset rownames from 1 to n
rownames(top10.points) <- 1:nrow(top10.points)

# drop empty factor levels
top10.points <- droplevels(top10.points)

# 19 removed since it was NA team with 20 points
all.points <- aggregate(TotalPoints ~ Team, data = NBA.Stats, FUN=sum)[-19,]

# average points across all teams
avg.points <- mean(all.points$TotalPoints)

# creates avg as a new column
# I dont really like the way it looks vs an avg line
#avg.points <- data.frame('Avg', mean(all.points$TotalPoints))
#names(avg.points) <- c('Team', 'TotalPoints')
#top10.points <- rbind(top10.points, avg.points)

y <- list(title = "Total Points")
x <- list(title = 'Teams')
points.plot <- plot_ly(top10.points, x = ~Team, y=~TotalPoints, name = 'Total Points', type='bar')%>% 
  add_trace(y =avg.points, name = 'League Avg Points', type = 'scatter', mode = 'lines')%>%
  layout(yaxis = y, title = "Top 10 Total Points per Team", xaxis = x)

# draw plot
points.plot

# 2)
# top 10 total minutes played
top.minutes <- aggregate(TotalMinutesPlayed ~ Team, data = NBA.Stats, FUN=sum)

# only select top 10 teams by total points
top10.minutes <- head(top.minutes[order(top.minutes$TotalMinutesPlayed, decreasing = TRUE),], n=10)

# Reset rownames from 1 to n
rownames(top10.minutes) <- 1:nrow(top10.minutes)

# drop empty factor levels
top10.minutes <- droplevels(top10.minutes)

y <- list(title = "Total Minutes")
x <- list(title = 'Teams')
minutes.plot <- plot_ly(top10.minutes, x = ~Team, y=~TotalMinutesPlayed, type='bar')%>% 
  layout(yaxis = y, title = "Total Minutes per Team", xaxis = x)

# draw plot
minutes.plot

# 3)
# Box plot
# store just the top 10 scoring team names
teams <- as.vector(top10.points$Team)

# pull players team and total points from top 10
top10.full <- NBA.Stats[with(NBA.Stats,Team %in% teams),c(3,21)]

# Reset rownames from 1 to n
rownames(top10.full) <- 1:nrow(top10.full)

# drop empty factor levels
top10.full <- droplevels(top10.full)

# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Total Points")
x <- list(title = 'Team')
top.box <- plot_ly(top10.full, x = ~Team, y = ~TotalPoints, type = 'box', size = 2)%>% 
  layout(xaxis = x, yaxis = y, title = "Point distribution of top 10 teams")

# draw plot
top.box

# 4)
# top 10 scorers
# number of FieldGoalsMade, ThreesMade, FreeThrowsMade

# players
players <- head(NBA.Stats[order(NBA.Stats$TotalPoints, decreasing = TRUE),c(2)], n=10)

# FieldGoalsMade, ThreesMade, FreeThrowsMade
player.points <- NBA.Stats[NBA.Stats$Name %in% players, c(2,7,9,11)]

# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Total Points")
x <- list(title = 'Player')
players.plot <- plot_ly(player.points, x = ~Name, y = ~FieldGoalsMade, type = 'bar', name = 'Field Goals')%>% 
  add_trace(y = ~ThreesMade, name =  'Threes' )%>%
  add_trace(y = ~FreeThrowsMade, name = 'Free Throws' )%>%
  layout(xaxis = x, yaxis = y, title = "Point Distribution of the Top 10 Players", barmode = 'stack')

# draw plot
players.plot
