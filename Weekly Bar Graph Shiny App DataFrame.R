library(tidyverse)
library(nflreadr)
library(httr)
library(jsonlite)

options(scipen = 9999)

#source("G:/My Drive/Giants/Code/ftn_functions.R")

source("ftn_functions.R")


SEASON <- 2023
key <- "bd99fcb1-bc62-4655-822c-9eb02d09d420"


ftn_charts <- ftn_game_data(type = "charts", year = SEASON)

ftn_plays <- ftn_game_data(type = "plays", year = SEASON)


url <- paste0("https://data.ftndata.com/games/", SEASON)
req <- GET(url, add_headers(Authorization = key))
ftn_games <- as.data.frame(fromJSON(rawToChar(req$content)))


  
  scores <- load_schedules(season = SEASON) %>%
    select(week, home_team, home_score, away_team, away_score)

  
  teams <- load_teams() %>%
    select(team_abbr, team_name, team_color, team_logo_espn)

  concept <- ftn_charts %>%
      left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
      left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
      filter(!is.na(concept), concept != "", !is.na(off.x), !is.na(def.x)) %>%
      mutate(off.x = case_when(
        off.x == "CLV" ~ "CLE",
        off.x == "BLT" ~ "BAL",
        off.x == "ARZ" ~ "ARI",
        off.x == "HST" ~ "HOU",
        TRUE ~ off.x
      )) %>%
      mutate(def.x = case_when(
        def.x == "CLV" ~ "CLE",
        def.x == "BLT" ~ "BAL",
        def.x == "ARZ" ~ "ARI",
        def.x == "HST" ~ "HOU",
        TRUE ~ def.x
      )) %>%
    mutate(concept = case_when(
      concept == "Inside Zone" ~ "Inside Zone",  
      concept == "Outside Zone" ~ "Outside Zone",  
      concept == "Man/Duo" ~ "Man/Duo",  
      concept == "Power" ~ "Power",  
      concept == "Counter" ~ "Counter",              
      concept == "Trap" ~ "Trap",  
      concept == "Draw" ~ "Draw",  
      TRUE ~ "Other"
    )) %>%
    group_by(seas, week, off.x) %>%
    mutate(snaps = n()) %>%
    ungroup() %>%
    group_by(seas, off.x, def.x, week, concept) %>%
    summarize(plays = n(), rate = plays/last(snaps), .groups = "drop") 
  

  
  saveRDS(concept, "Weekly_Bar_Graph_data.rds")
