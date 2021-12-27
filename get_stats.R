library(tidyverse)
library(janitor)
library(lubridate)
library(RJSONIO)
library(jsonlite)

# link
url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=statviewtype&type=topscorers&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&league_code=&season_id=70&first=0&limit=500&sort=active&stat=all&order_direction="

# use jsonlite::fromJSON to handle NULL values
json_data <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

# create data frame
df <- json_data[["SiteKit"]][["Statviewtype"]] %>%
  select(rank, player_id:num_teams) %>% 
  select(-c(birthtown, birthprov, birthcntry,
            loose_ball_recoveries, caused_turnovers, turnovers,
            phonetic_name, last_years_club, suspension_games_remaining,
            suspension_indefinite)) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  mutate(across(active:age, ~as.numeric(.))) %>% 
  mutate(across(rookie:jersey_number, ~as.numeric(.))) %>% 
  mutate(team_id = as.numeric(team_id)) %>% 
  mutate(across(games_played:faceoff_pct, ~as.numeric(.))) %>%
  mutate(across(shots_on:num_teams, ~as.numeric(.))) %>% 
  mutate(birthdate_year = stringr::str_split(birthdate_year,
                                             "\\'", simplify = TRUE, n = 2)[,2]) %>% 
  mutate(birthdate_year = as.numeric(birthdate_year)) %>% 
  mutate(birthdate_year = 2000 + birthdate_year)

# create data frame with columns required for tableau viz
df2 <- df %>% 
  select(Name = "name",
         Pos = "position",
         Team = "team_name",
         GP = "games_played",
         G = "goals",
         A = "assists",
         PTS = "points",
         `Pts/G` = "points_per_game",
         PPG = "power_play_goals",
         PPA = "power_play_assists",
         GWG = "game_winning_goals")

# save file to csv
write_csv(df2,
          file = "Ohl_stats_2021_2022.csv")
