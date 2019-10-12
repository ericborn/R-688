library('XML')
library('rvest')
library('purrr')
library('plotly')
library('stringi')
library('SportsAnalytics')

# a)
# 2003-2004

# b)
# pull down 2003-04 season stats
NBA.Stats <- fetch_NBAPlayerStatistics(season = "03-04", what = c("",".Home", ".Away"))

# c)
wolves <- NBA.Stats[NBA.Stats$Team == 'MIN',]

# Highest Total Points
wolves[wolves$TotalPoints == max(wolves$TotalPoints),c(2, 21)]

# Highest Blocks
wolves[wolves$Blocks == max(wolves$Blocks),c(2, 18)]

# Highest Rebounds
wolves[wolves$TotalRebounds == max(wolves$TotalRebounds),c(2, 14)]

# d)
# top 5 teams for the 03-04 season
season.url <- 'https://www.landofbasketball.com/yearbyyear/2003_2004_standings.htm'

# read the html
webpage <- read_html(season.url)

# create var to hold wins/losses for each conference
mid.wins <- c(0)
mid.loss <- c(0)
pac.wins <- c(0)
pac.loss <- c(0)
atl.wins <- c(0)
atl.loss <- c(0)
cen.wins <- c(0)
cen.loss <- c(0)

# midwest div 7 teams
# loop through the columns grabbing each win and loss
for (k in 2:8){
  mid.wins[[k]] <- webpage %>% html_nodes("table") %>% .[4] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:8){
  mid.loss[[k]] <- webpage %>% html_nodes("table") %>% .[4] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[4] %>% html_text()
}

# pac div 7 teams
# loop through the columns grabbing each win and loss
for (k in 2:8){
  pac.wins[[k]] <- webpage %>% html_nodes("table") %>% .[5] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:8){
  pac.loss[[k]] <- webpage %>% html_nodes("table") %>% .[5] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[4] %>% html_text()
}

# atl div 7 teams
# loop through the columns grabbing each win and loss
for (k in 2:8){
  atl.wins[[k]] <- webpage %>% html_nodes("table") %>% .[4] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:8){
  atl.loss[[k]] <- webpage %>% html_nodes("table") %>% .[4] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[4] %>% html_text()
}

# cen div 8 teams
# loop through the columns grabbing each win and loss
for (k in 2:9){
  cen.wins[[k]] <- webpage %>% html_nodes("table") %>% .[5] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:9){
  cen.loss[[k]] <- webpage %>% html_nodes("table") %>% .[5] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[4] %>% html_text()
}

# drop the 1st index which is an NA
mid.wins <- mid.wins[-1]
mid.loss <- mid.loss[-1]
pac.wins <- pac.wins[-1]
pac.loss <- pac.loss[-1]
atl.wins <- atl.wins[-1]
atl.loss <- atl.loss[-1]
cen.wins <- cen.wins[-1]
cen.loss <- cen.loss[-1]

# team names by division
western <- webpage %>% html_nodes("table") %>% .[1] %>% html_nodes("a") %>% html_text()
eastern <- webpage %>% html_nodes("table") %>% .[2] %>% html_nodes("a") %>% html_text()
midwest <- webpage %>% html_nodes("table") %>% .[4] %>% html_nodes("a") %>% html_text()
pacific <- webpage %>% html_nodes("table") %>% .[5] %>% html_nodes("a") %>% html_text()
atlantic <- webpage %>% html_nodes("table") %>% .[6] %>% html_nodes("a") %>% html_text()
central <- webpage %>% html_nodes("table") %>% .[7] %>% html_nodes("a") %>% html_text()
teams <- c(Midwest, Pacific, Atlantic, Central)

teams.df = data.frame(Team=teams, Conference=c(rep('Western',length(western)),
                                              rep('Eastern',length(eastern))),
                      Division=c(rep('Midwest', length(midwest)),
                                 rep('Pacific', length(pacific)),
                                 rep('Atlantic', length(atlantic)),
                                 rep('Central', length(central))
                                 ),Win=c(west.wins, ),Loss='')

teams.df$Division = 1

cbind(teams.df,Team=teams)

teams.df[teams.df$Team %in% western,] = 1


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

# 5)
# score breakdown per top 10 teams
# players
teams <- head(NBA.Stats[order(NBA.Stats$TotalPoints, decreasing = TRUE),c(3)], n=10)

# FieldGoalsMade, ThreesMade, FreeThrowsMade
team.points <- NBA.Stats[NBA.Stats$Team %in% teams, c(3,7,9,11)]

# Reset rownames from 1 to n
rownames(team.points) <- 1:nrow(team.points)

# drop empty factor levels
team.points <- droplevels(team.points)

# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Total Points")
x <- list(title = 'Team')
team.point.plot <- plot_ly(team.points, x = ~Team, y = ~FieldGoalsMade, type = 'bar', name = 'Field Goals')%>% 
  add_trace(y = ~ThreesMade, name =  'Threes' )%>%
  add_trace(y = ~FreeThrowsMade, name = 'Free Throws' )%>%
  layout(xaxis = x, yaxis = y, title = "Point Distribution of the Top 10 Teams", barmode = 'stack')

# draw plot
team.point.plot