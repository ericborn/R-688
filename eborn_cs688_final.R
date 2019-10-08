# Eric Born
# CS 688 Final Project
# Web scraping NFL stats

#install.packages('rvest')
library('rvest')

# scrape player name, team name, position, offensive stats: yards, touch downs
# defensive stats: interceptions, passes defended, fumbles, sacks

# find main page that contains all teams for 2018 season
teamsUrl <- 'https://www.pro-football-reference.com/years/2018/'

# read the html
webpage <- read_html(teamsUrl)

# one table for AFC one for NFC
<table class="sortable stats_table now_sortable" id="AFC" data-cols-to-freeze="1">
<table class="sortable stats_table now_sortable" id="NFC" data-cols-to-freeze="1">

  

  
# create link for each team
#sample team link
https://www.pro-football-reference.com/teams/nwe/2018.htm

# create link for each player
# table class with id="passing", then tbody and count <tr data-row=,
# then look for data-stat="player" and retrieve csk= for the players name.
# iterate through all rows grabbing their name
<table class="sortable stats_table now_sortable" id="passing" data-cols-to-freeze="2">
<tbody>
# count rows
<tr data-row="0">.</tr>
# for i in rows save csk= value
<td class="left " data-append-csv="BradTo00" data-stat="player" csk="Brady,Tom">

# rushing/receiving
<div id="all_rushing_and_receiving" class="table_wrapper table_controls">


  
# sample player, defaults to current season
https://www.pro-football-reference.com/players/M/MichSo00.htm

# move to 2018 season
https://www.pro-football-reference.com/players/M/MichSo00/gamelog/2018/

# name
<h1 itemprop="name">Kyle Van Noy</h1>

# team name
<a href="/teams/nwe/2018.htm">New England Patriots</a>
  
#yards = yards from scrimmage
<tr id="rushing_and_receiving.2018" class="full_table" data-row="0">
<td class="right " data-stat="yds_from_scrimmage">981</td>
  
# yards passing
# if <h2>Passing</h2> exists
<tr id="passing.2018" class="full_table" data-row="0">
<td class="right " data-stat="pass_yds">4355</td>