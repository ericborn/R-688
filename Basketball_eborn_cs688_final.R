library('XML')
library('rvest')
library('purrr')
library('plotly')
library('stringi')
library('googleVis')
library('SportsAnalytics')

# a)
# 2003-2004

# b)
# pull down 2003-04 season stats
NBA.Stats <- fetch_NBAPlayerStatistics(season = "03-04", what = c("",".Home", ".Away"))

# c)
wolves <- NBA.Stats[NBA.Stats$Team == 'MIN',]

# TODO
# !!! cONVERT TO PLOT_LY TABLE
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
  atl.wins[[k]] <- webpage %>% html_nodes("table") %>% .[6] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:8){
  atl.loss[[k]] <- webpage %>% html_nodes("table") %>% .[6] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[4] %>% html_text()
}

# cen div 8 teams
# loop through the columns grabbing each win and loss
for (k in 2:9){
  cen.wins[[k]] <- webpage %>% html_nodes("table") %>% .[7] %>% 
    html_nodes("tr") %>% .[k] %>% html_nodes("td") %>% .[3] %>% html_text()
}

for (k in 2:9){
  cen.loss[[k]] <- webpage %>% html_nodes("table") %>% .[7] %>% 
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

# team names by conference and division
western <- webpage %>% html_nodes("table") %>% .[1] %>% html_nodes("a") %>% html_text()
eastern <- webpage %>% html_nodes("table") %>% .[2] %>% html_nodes("a") %>% html_text()
midwest <- webpage %>% html_nodes("table") %>% .[4] %>% html_nodes("a") %>% html_text()
pacific <- webpage %>% html_nodes("table") %>% .[5] %>% html_nodes("a") %>% html_text()
atlantic <- webpage %>% html_nodes("table") %>% .[6] %>% html_nodes("a") %>% html_text()
central <- webpage %>% html_nodes("table") %>% .[7] %>% html_nodes("a") %>% html_text()
teams <- c(midwest, pacific, atlantic, central)

# Create dataframe from win/loss data
teams.df = data.frame(Team=teams, 
                      Conference=c(rep('Western',length(western)),
                                   rep('Eastern',length(eastern))),
                      Division=c(rep('Midwest', length(midwest)),
                                 rep('Pacific', length(pacific)),
                                 rep('Atlantic', length(atlantic)),
                                 rep('Central', length(central))),
                      Win=c(mid.wins, pac.wins, atl.wins, cen.wins),
                      Loss=c(mid.loss,pac.loss,atl.loss,cen.loss))

# TODO
# CONVERT TO TABLE
# output top 5 teams by total wins
head(teams.df[order(teams.df$Win, decreasing = TRUE),], n=5)

# e)
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

y <- list(title = "Total Points")
x <- list(title = 'Teams')
points.plot <- plot_ly(top10.points, x = ~Team, y= ~TotalPoints,
                       type='bar', color = ~Team)%>% 
  add_trace(y = avg.points, name = 'League Avg Points', type = 'scatter', 
            mode = 'lines', color = I('black'))%>%
  layout(yaxis = y, title = "Top 10 Total Points per Team", xaxis = x)

# draw plot
points.plot

# # 2)
# scorers breakdown for the wolves
# number of FieldGoalsMade, ThreesMade, FreeThrowsMade
wolves.points <- wolves[c(2,7,9,11)]

# Create boxplot based on top 10 beer styles with ABV information
y <- list(title = "Total Points")
x <- list(title = 'Players')
wolves.plot <- plot_ly(wolves.points, x = ~Name, y = ~FieldGoalsMade, type = 'bar', name = 'Field Goals')%>% 
  add_trace(y = ~ThreesMade, name =  'Threes' )%>%
  add_trace(y = ~FreeThrowsMade, name = 'Free Throws' )%>%
  layout(xaxis = x, yaxis = y, title = "Point Distribution of the Timber Wolves", barmode = 'stack')

# draw plot
wolves.plot

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
top.box <- plot_ly(top10.full, x = ~Team, y = ~TotalPoints, type = 'box', size = 2,
                   color = ~Team)%>% 
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

# f)
# champ names

champ.url <- 'https://www.landofbasketball.com/championships/year_by_year.htm'

# read the html
champ.page <- read_html(champ.url)

# initalize vector
champ.names <- c(0)

# loop through page to get the last 20 NBA champs
for (k in 2:21){
  champ.names[[k]] <- champ.page %>% html_nodes("table") %>% .[1] %>% html_nodes("tr") %>% .[k] %>% 
    html_nodes("a") %>% .[2] %>% html_text()
}

# drop index 1
champ.names <- champ.names[-1]

# Makes champion names unique
champ.names <- unique(champ.names)

# url for NBA team names and coordinates
city.url <- 'https://en.wikipedia.org/wiki/National_Basketball_Association'

# read html
city.page <- read_html(city.url)

# initalize empty list
champ.city <- list()

# champ.city <- c(city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[4] %>% 
#     html_nodes("td") %>% .[1] %>% html_text(),
#   # coords
#   city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[4] %>% 
#     html_nodes("td") %>% .[5] %>% html_nodes("span") %>% .[11] %>% html_text())

# grabs team and coords from first table
k = 3
# 4-18
for (i in 1:15){
                         # team name
  champ.city[i] <- paste(city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[k] %>% 
                         html_nodes("td") %>% .[1] %>% html_text(),
                         # coords
                         city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[k] %>% 
                         html_nodes("td") %>% .[5] %>% html_nodes("span") %>% .[11] %>% html_text())
  k <- k + 1

}  

# grabs team and coords from second table
j = 19
# 19-34
for (i in 16:30){
                         # team name
  champ.city[i] <- paste(city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[j] %>% 
                          html_nodes("td") %>% .[1] %>% html_text(),
                         # coords
                         city.page %>% html_nodes("table") %>% .[3] %>% html_nodes("tr") %>% .[j] %>% 
                          html_nodes("td") %>% .[5] %>% html_nodes("span") %>% .[11] %>% html_text())
  j <- j + 1                         
}

# split strings on \n
champ.split <- sapply(champ.city, function(x) strsplit(x, "\n"))

# initalize empty lists
names <- list()
coords <- list()

# creates separate lists from team names and coordinates
for (i in 1:length(champ.split)){
  names[i] <- champ.split[[i]][1]
  coords[i] <- champ.split[[i]][2]
}

# replace semi colon with colon
coords <- stri_replace_first_charclass(coords, "[;]", ":")

# remove leading space and space after colon
coords <- gsub(" ", "", coords)

# flatten list of names
names <- unlist(names)

# static set of winners and coords
# toronto      '43.643333:79.379167'
# golden state '37.768056:122.3875'
# cleveland    '41.496389:81.688056'
# san antonio  '29.426944:98.4375'
# miami        '25.781389:80.188056'
# dallas       '32.790556:96.810278'
# lakers       '34.043056:118.267222'
# boston       '42.366303:71.062228'
# detroit      '42.696944:83.245556'

# coords <- c('43.643333:-79.379167', '37.768056:-122.3875', '41.496389:-81.688056',
#             '29.426944:-98.4375', '25.781389:-80.188056', '32.790556:-96.810278',
#             '34.043056:-118.267222', '42.366303:-71.062228', '42.696944:-83.245556')


# all teams and their coordinates as separate columns in a df
full.df <- data.frame(team = names, coords = coords)

# creates a df for just the champion teams
map.df <- data.frame(LatLong = full.df[full.df$team %in% champ.names,][2], 
                     Tip = full.df[full.df$team %in% champ.names,][1])

# setup map
champMap <- gvisMap(map.df,
                    locationvar = 'coords',
                    tipvar = 'team',
                    options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
# draw map
plot(champMap)
