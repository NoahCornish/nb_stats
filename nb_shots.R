library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)
library(googlesheets4)

# store season ID for paste into url
season_id <- 70

# OHL team_IDs
ohl_teams <- tibble(
  team_id = c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,34),
  team_name = c("HAM", "KGN", "OSH", "OTT", "PBO", "BAR", "ER", "GUE",
                "KIT", "OS", "SBY", "FLNT", "LDN", "SAR", "SOO", "WSR",
                "MISS", "NB", "NIAG", "SAG")
)

# create an empty place to add each game data
output <- NULL

# iterative process to read each team id from ohl_teams data frame and then run code
for (i in 1:nrow(ohl_teams)) {
  team_ID <- (ohl_teams[i, 1])
  
  # Automate the link based on team_ID
  str1 <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id="
  str2 <- "&team_id="
  str3 <- "&league_code=&fmt=json"
  gameID_url <- paste0(str1,season_id,str2,team_ID,str3)
  
  # Import gameID data from JSON
  # use jsonlite::fromJSON to handle NULL values
  gameID_data <- jsonlite::fromJSON(gameID_url, simplifyDataFrame = TRUE)
  
  # Pull out the data frame for gameID
  game_data <- gameID_data[["SiteKit"]][["Schedule"]]
  # Pull out the data frame for team_id
  team_id <- gameID_data[["SiteKit"]][["Parameters"]][["team_id"]]
  # pull out if game started
  game_start <- gameID_data[["SiteKit"]][["Schedule"]][["started"]]
  game_start <- as.data.frame(game_start)
  # create a data frame with team_id
  team <- data.frame(team_id)
  
  # clean data
  games <- game_data %>%
    mutate(team_id = as.numeric(team_id)) %>%
    mutate(team = case_when(
      team_id == 1 ~ "HAM",
      team_id == 2 ~ "KGN",
      team_id == 4 ~ "OSH",
      team_id == 5 ~ "OTT",
      team_id == 6 ~ "PBO",
      team_id == 7 ~ "BAR",
      team_id == 8 ~ "ER",
      team_id == 9 ~ "GUE",
      team_id == 10 ~ "KIT",
      team_id == 11 ~ "OS",
      team_id == 12 ~ "SBY",
      team_id == 13 ~ "FLNT",
      team_id == 14 ~ "LDN",
      team_id == 15 ~ "SAR",
      team_id == 16 ~ "SOO",
      team_id == 17 ~ "WSR",
      team_id == 18 ~ "MISS",
      team_id == 19 ~ "NB",
      team_id == 20 ~ "NIAG",
      team_id == 34 ~ "SAG",
      TRUE ~ "NULL")) %>% 
    select(team_id, team, ohl_game_id = "game_id", date_played,
           home = "home_team_code", home_score = "home_goal_count",
           visitor = "visiting_team_code", visitor_score = "visiting_goal_count") %>% 
    mutate(ohl_game_id = as.numeric(ohl_game_id),
           date_played = as.Date(date_played),
           home_score = as.numeric(home_score),
           visitor_score = as.numeric(visitor_score)) %>% 
    bind_cols(game_start)
  
  output <- rbind(output, (games))
  
}

# clean up the output script for what we need
# added TRUE statement producing "Not played" so
# other scripts can filter out these games for data
games_info <- output %>%
  #filter(home_score != "0" | visitor_score != "0") %>%
  arrange(ohl_game_id) %>%
  group_by(ohl_game_id) %>%
  slice(1) %>%
  select(-c(team_id, team)) %>%
  arrange(ohl_game_id) %>%
  mutate(winner = case_when(
    home_score > visitor_score ~ "home",
    home_score < visitor_score ~ "visitor",
    TRUE ~ "Not played"
  )) %>% 
  filter(home == "NB" | visitor == "NB") %>% 
  arrange(date_played)

df <- games_info 
  


# pull all ohl games from github
#df <- read.csv("https://raw.githubusercontent.com/turkjr19/nb_shots/main/ohl_2021_2022_regSeasonGameIDs.csv")

# pull ohl roster names from github
#rosters <- read.csv("https://raw.githubusercontent.com/turkjr19/OHL_scrape/main/data/rosters_Jan05.csv")
rosters <- read.csv("rosters_updated.csv")

# get google sheet and establish where to append too later in code
ss <- gs4_get('https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing')

# read sheet from googlesheet (nb_shots shared in claircornish google drive)
df2 <- read_sheet("https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing")




# adjust date to use a filter for scraping
#scrape_dates <- today()

# create dataframe that we will use to iterate through to pull json data from events
gameIDs <- df %>% 
  filter(game_start == 1) %>% 
  mutate(opponent = case_when(
    home == "NB" ~ visitor,
    TRUE ~ home
  )) %>% 
    select(-c(game_start))

# previous games with shot data
df2_previous <- df2 %>% 
  select(ohl_game_id) %>% 
  unique()

# update gameIDs dataframe with only games that need shots scraped
gameIDs <- gameIDs %>% 
  anti_join(df2_previous, by = "ohl_game_id")
  #filter(ohl_game_id == 25077)

# logic to check if there were games yesterday
# if there were no games stop and print
# if there were games continue on with lineups and events
if(nrow(gameIDs) == 0){
  stop("all games have been scraped - nothing to scrape")
}

## now scrape events
# get the number of games to scrape
pb_count <- nrow(gameIDs)

#set random system sleep variable so we don't overload ohl server
tmsleep <- sample(5:10,1)

# set progress bar so we have an idea how long it will take
pb <- txtProgressBar(min = 0, max = pb_count, style = 3)

# create empty dataframe to store data
output <- NULL

# iterative process to read each game id from all games data frame and then run code
for (i in 1:nrow(gameIDs)) {
  game_ID <- (gameIDs[i, 1])
  ohl_game_id <- as.numeric(gameIDs[i,1])
  Sys.sleep(tmsleep)
  shots <- NULL
  
  str1 <- "https://cluster.leaguestat.com/feed/index.php?feed=gc&key=2976319eb44abe94&client_code=ohl&game_id="
  str2 <- "&lang_code=en&fmt=json&tab=pxpverbose"
  game_url <- paste0(str1,game_ID,str2)
  url <- game_url
  
  # Import pxp data from JSON
  # use jsonlite::fromJSON to handle NULL values
  json_data <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  
  # create a tiblle
  events <- as_tibble(json_data[["GC"]][["Pxpverbose"]]) %>% 
    mutate(ohl_game_id = ohl_game_id) %>%
    select(ohl_game_id, everything())
  
  # get shot data
  shots <- events %>% 
    filter(event == "shot") %>% 
    select(ohl_game_id, event, time, s, period_id, team_id, x_location, y_location,
           shot_player_id = "player_id", home, shot_type, shot_type_description,
           shot_quality_description)
  
  output <- bind_rows(shots, output)
  
  setTxtProgressBar(pb, i)
  
}


# ***** cleaning shot data and pulling out what we need *****
y <- output %>% 
  #filter(ohl_game_id == x) %>% 
  filter(team_id == 19) %>% # filter only North Bay shots
  select(ohl_game_id, team_id, period_id, x_location, y_location,
         shot_player_id, shot_quality_description) %>% 
  mutate(shot_id = row_number()) # need this to use for joining later

# replacing shot_player_id with player names
z <- y %>% 
  select(shot_id, shot_player_id) %>% 
  mutate(player_id = as.integer(shot_player_id)) %>% 
  left_join(rosters, by = 'player_id') %>% 
  select(shot_id, full_name)

# pull out opponent to join with shot data
opponent <- gameIDs %>% 
  select(ohl_game_id, date_played, opponent)
  
# create columns so that shots will appear properly on the plot
viz_df <- z %>% 
  left_join(y, by = "shot_id") %>% 
  select(ohl_game_id, shot_id, x_location, y_location, full_name,
         period = "period_id",
         shot_quality_description) %>% 
  mutate(x.plot = x_location,
         y.plot = y_location*-1) %>%
  mutate(x.plot = x.plot-300,
         y.plot = y.plot+150) %>% 
  mutate(x.plot = x.plot*-1,
         y.plot = y.plot*-1) %>%
  mutate(x.plot = x.plot/2.93) %>% 
  mutate(y.plot = case_when(
    y.plot >0 ~ y.plot/3,
    y.plot <0 ~ y.plot/3,
    TRUE ~ (as.numeric(.$y.plot)),
  )) %>% 
  mutate(result = case_when(
    shot_quality_description == "Quality goal" ~ "goal",
    shot_quality_description == "Non quality goal" ~ "goal",
    TRUE ~ "shot"
  )) %>% 
  left_join(opponent, by = "ohl_game_id") %>% 
  select(ohl_game_id, date_played, opponent,
         shot_id:result) %>%
  mutate(y.plot = case_when(
    y.plot > 42 ~ 41.9,
    y.plot < -42 ~ -41.9,
    TRUE ~ (as.numeric(.$y.plot))
  )) %>%
  mutate(TEAM = "NB") %>% 
  #filter(full_name == "Noah Cornish") %>% #get shots for a desired player
  arrange(date_played)

# append to google sheet
sheet_append(ss, data = viz_df)


############################