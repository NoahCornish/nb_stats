library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)
library(googlesheets4)

# pull all ohl games from github
df <- read.csv("https://raw.githubusercontent.com/turkjr19/nb_shots/main/ohl_2021_2022_regSeasonGameIDs.csv")

# pull ohl roster names from github
rosters <- read.csv("https://raw.githubusercontent.com/turkjr19/OHL_scrape/main/data/rosters_Dec24.csv")

# get google sheet and establish where to append too later in code
ss <- gs4_get('https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing')

# read sheet from googlesheet (nb_shots shared in claircornish google drive)
df2 <- read_sheet("https://docs.google.com/spreadsheets/d/1aueTPPjV3axvQU9Eu7T-LLbzW2CASZKy5iI7vuNrJK8/edit?usp=sharing")

# adjust date to use a filter for scraping
scrape_dates <- today()

# create dataframe that we will use to iterate through to pull json data from events
gameIDs <- df %>% 
  filter(date_played <= scrape_dates) %>% 
  filter(home == "NB" | visitor == "NB") %>% 
  rename(ohl_game_id = "game_id") %>% 
  mutate(opponent = case_when(
    home == "NB" ~ visitor,
    TRUE ~ home
  ))

# previous games with shot data
df2_previous <- df2 %>% 
  select(ohl_game_id) %>% 
  unique()

# update gameIDs dataframe with only games that need shots scraped
gameIDs <- gameIDs %>% 
  anti_join(df2_previous, by = "ohl_game_id")

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
  arrange(date_played)

# append to google sheet
sheet_append(ss, data = viz_df)

