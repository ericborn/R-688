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

# d)
plot_ly(glitch.players, x = ~month, y=~departing, type='scatter', mode='line')%>% 
  add_trace(y=~joining)
%>%
  add_trace()
  
  layout(yaxis = y, title = "Top 10 most reviewed beers by name", xaxis = x)