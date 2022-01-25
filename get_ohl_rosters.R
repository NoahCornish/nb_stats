library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)

# store season ID for paste into url
season_id <- 70

# OHL team_IDs
id <- c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,34)
ohl_teams <- data.frame(id)

# create an empty place to add each player bio data
output <- NULL

# iterative process to read each team id from ohl_teams data frame and then run code
for (i in 1:nrow(ohl_teams)) {
  team_ID <- (ohl_teams[i, 1])
  
  # Automate the link based on team_ID
  str1 <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=roster&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id="
  str2 <- "&team_id="
  str3 <- "&fmt=json"
  url <- paste0(str1,season_id,str2,team_ID,str3)
  
  # Import roster data from JSON
  # use jsonlite::fromJSON to handle NULL values
  json_data <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  
  # create a tibble of only the number of players to loop through
  # the last list item is the coaches/managers so we need to remove that with code below
  list <- json_data[["SiteKit"]][["Roster"]]
  list <- head(list,-1)
  
  for (i in list){
    # had to create a tibble and pull from each list item because some players
    # had a draftteam list that was empty and it would not allow me to just create a tibble
    # right off the start
    player_bios <- tibble(
      player_id = as.numeric((i)[["player_id"]]),
      full_name = (i)[["name"]],
      last_name = (i)[["last_name"]],
      first_name = (i)[["first_name"]],
      pos = (i)[["position"]],
      shoots = (i)[["shoots"]],
      height = as.numeric((i)[["height"]]),
      weight = as.numeric((i)[["weight"]]),
      birthdate = as.Date((i)[["birthdate"]]),
      latest_team_id = as.numeric((i)[["latest_team_id"]]),
      team_name = (i)[["team_name"]],
      division = (i)[["division"]],
      jersey_number = as.numeric((i)[["tp_jersey_number"]]),
      rookie = as.numeric((i)[["rookie"]])
    )
    
    output <- bind_rows(output, player_bios) %>% 
      unique()
  }
  
}  

write_csv(output,
          file = "rosters_updated.csv")
