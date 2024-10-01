library(tidyverse)
library(nflreadr)
library(httr)
library(jsonlite)

options(scipen = 9999)

#source("G:/My Drive/Giants/Code/ftn_functions.R")

source("ftn_functions.R")


SEASON <- 2024
key <- Sys.getenv("API_KEY")  # Use the API key from the environment variable
#key <- "bd99fcb1-bc62-4655-822c-9eb02d09d420"


ftn_charts <- ftn_game_data(type = "charts", year = SEASON) %>%
  mutate(off = case_when(
    off == "CLV" ~ "CLE",
    off == "BLT" ~ "BAL",
    off == "ARZ" ~ "ARI",
    off == "HST" ~ "HOU",
    TRUE ~ off
  )) %>%
  mutate(def = case_when(
    def == "CLV" ~ "CLE",
    def == "BLT" ~ "BAL",
    def == "ARZ" ~ "ARI",
    def == "HST" ~ "HOU",
    TRUE ~ def
  )) 

ftn_plays <- ftn_game_data(type = "plays", year = SEASON)

#ftn_participation <- ftn_game_data(type = "participation", year = SEASON) 

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
  filter(!is.na(concept), concept != "", concept != "Kneel", !is.na(off.x), !is.na(def.x)) %>%
  mutate(concept = case_when(
    concept == "Inside Zone" ~ "Inside Zone",  
    concept == "Outside Zone" ~ "Outside Zone",  
    concept == "Man/Duo" ~ "Man/Duo",  
    concept == "Power" ~ "Power",  
    concept == "Counter" ~ "Counter",              
    concept == "Trap" ~ "Trap",  
    concept == "Draw" ~ "Draw",  
    concept == "Fullback Run" ~ "WR/FB Run",  
    concept == "QB Sneak" ~ "QB Sneak",
    concept == "Trick/WR Run" ~ "WR/FB Run",  
    TRUE ~ "Other"
  )) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps = n()) %>%
  ungroup() %>%
  group_by(seas, team = off.x, opp = def.x, week, category = concept) %>%
  summarize(plays = n(),
            rate = plays/last(snaps), .groups = "drop") 

shell <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(!is.na(shell), shell != "", !is.na(off.x), !is.na(def.x)) %>%
  mutate(shell = case_when(
    shell == 0 ~ "Cover 0",  
    shell == 1 ~ "Cover 1",  
    shell == 2 ~ "Cover 2",  
    shell == 3 ~ "Cover 3",   
    shell == 4 ~ "Cover 4",            
    shell == 6 ~ "Cover 6",  
    shell == 9 ~ "Cover 9",  
    shell == "2M" ~ "Cover 2 Man",  
    TRUE ~ "Other"
  )) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_shell = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = shell) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_shell), .groups = "drop") 

air_yards <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), !is.na(air_yards), pass == 1) %>%
  mutate(air_yards_bracket = case_when(
    air_yards <= 0 ~ "Behind LOS",  
    air_yards >= 1 & air_yards <= 5 ~ "1-5",  
    air_yards >= 6 & air_yards <= 10 ~ "6-10",  
    air_yards >= 11 & air_yards <= 15 ~ "11-15",   
    air_yards >= 16 & air_yards <= 20 ~ "16-20",            
    air_yards >= 21 ~ "21+")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_air = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = air_yards_bracket) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_air))

route <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(!is.na(rte), rte != "", !is.na(off.x), !is.na(def.x)) %>%
  mutate(rte = case_when(
    rte == "0 - Screen" ~ "Screen",  
    rte == "1 - Slant" ~ "Slant",  
    rte == "2 - Quick Out" ~ "Quick Out",  
    rte == "3 - Hitch/Curl" ~ "Hitch/Curl",   
    rte == "4 - Deep Out" ~ "Deep Out",            
    rte == "5 - In/Dig" ~ "In/Dig",  
    rte == "6 - Corner" ~ "Corner",  
    rte == "7 - Post" ~ "Post",
    rte == "8 - Shallow Cross/Drag" ~ "Shallow Cross",            
    rte == "9 - Go" ~ "Go",  
    rte == "10 - Swing" ~ "Swing",  
    rte == "11 - Texas/Angle" ~ "Texas/Angle",
    rte == "12 - Wheel" ~ "Wheel")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_rte = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = rte) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_rte), .groups = "drop") 

ttp <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(!is.na(ttp), ttp != "", !is.na(off.x), !is.na(def.x)) %>%
  mutate(ttp_bracket = case_when(
    ttp < 1 ~ "Less than 1s",  
    ttp >= 1 & ttp <= 1.5 ~ "1s-1.5s",  
    ttp > 1.5 & ttp <= 2 ~ "1.6s-2s",  
    ttp > 2 & ttp <= 3 ~ "2.1s-3s",   
    ttp > 3 & ttp <= 4 ~ "3.1s-4s",            
    ttp > 4. ~ "Longer than 4s")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_ttp = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = ttp_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_ttp), .groups = "drop") 

saveRDS(concept, "Weekly_Bar_Graph_data_concept.rds")

saveRDS(shell, "Weekly_Bar_Graph_data_shell.rds")

saveRDS(air_yards, "Weekly_Bar_Graph_data_air_yards.rds")

saveRDS(route, "Weekly_Bar_Graph_data_route.rds")

saveRDS(ttp, "Weekly_Bar_Graph_data_ttp.rds")            
