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
  group_by(seas, team = off.x, opp = def.x, week, category = rte) %>%
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
  group_by(seas, team = off.x, opp = def.x, week, category = ttp_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_ttp), .groups = "drop") 

ttpr <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(!is.na(ttpr), ttpr != "", !is.na(off.x), !is.na(def.x)) %>%
  mutate(ttpr_bracket = case_when(
    ttpr < 1.5 ~ "Less than 1.5s",  
    ttpr >= 1.5 & ttpr <= 2.0 ~ "1.5s-2.0s",  
    ttpr > 2 & ttpr <= 2.5 ~ "2.1s-2.5s",  
    ttpr > 2.5 & ttpr <= 3 ~ "2.5s-3s",   
    ttpr > 3 & ttpr <= 4 ~ "3.1s-4s",            
    ttpr > 4. ~ "Longer than 4s")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_ttpr = n()) %>%
  ungroup() %>%
  group_by(seas, team = off.x, opp = def.x, week, category = ttpr_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_ttpr), .groups = "drop") 

downs <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), pass + rush == 1) %>%
  mutate(down = case_when(
    down == 1 ~ "1st",  
    down == 2 ~ "2nd",    
    down == 3 ~ "3rd",  
    down == 4 ~ "4th")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_down = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = down) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_down))

ydstogo <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), pass + rush == 1) %>%
  mutate(ydstogo_bracket = case_when(
    ydstogo <= 3 ~ "1-3 yards",  
    ydstogo >= 4 & ydstogo <= 7 ~ "4-7 yards",  
    ydstogo >= 8 & ydstogo <= 10 ~ "8-10 yards",           
    ydstogo >= 11 ~ "11+ yards")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_ydstogo = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = ydstogo) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_ydstogo))

run_gap <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), rush == 1) %>%
  mutate(run_gap_bracket = case_when(
    run_location == "left" & run_gap == "end" ~ "left end",  
    run_location == "left" & run_gap == "tackle" ~ "left tackle",  
    run_location == "left" & run_gap == "guard" ~ "left guard",         
    run_location == "right" & run_gap == "end" ~ "right end",  
    run_location == "right" & run_gap == "tackle" ~ "right tackle",  
    run_location == "right" & run_gap == "guard" ~ "right guard",      
    TRUE ~ "middle")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_run_gap = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = run_gap_bracket) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_run_gap))

play_action <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(type.x == "PASS", !is.na(off.x), !is.na(def.x)) %>%
  mutate(play_action_bracket = case_when(
    pap == 1 ~ "Yes",
    TRUE ~ "No")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_pass = n()) %>%
  ungroup() %>%
  group_by(seas, team = off.x, opp = def.x, week, category = play_action_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_pass), .groups = "drop") 

pressure_allowed <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(type.x == "PASS", !is.na(off.x), !is.na(def.x)) %>%
  mutate(pressure_bracket = case_when(
    qbp == 1 ~ "Yes",
    TRUE ~ "No")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_pass = n()) %>%
  ungroup() %>%
  group_by(seas, team = off.x, opp = def.x, week, category = pressure_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_pass), .groups = "drop") 

pass_rushers <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(type.x == "PASS", !is.na(off.x), !is.na(def.x)) %>%
  mutate(pass_rushers_bracket = case_when(
    pru <= 3 ~ "3 or Fewer",  
    pru == 4 ~ "4",  
    pru == 5 ~ "5", 
    pru == 6 ~ "6",          
    pru >= 7 ~ "7 or More")) %>%
  group_by(seas, week, def.x) %>%
  mutate(snaps_pass = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = pass_rushers_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_pass), .groups = "drop") 

men_in_box <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(type.x == "PASS" | type.x == "RUSH", !is.na(off.x), !is.na(def.x)) %>%
  mutate(men_in_box_bracket = case_when(
    box <= 5 ~ "5 or Fewer",  
    box == 6 ~ "6",  
    box == 7 ~ "7", 
    box == 8 ~ "8",          
    box >= 9 ~ "9 or More")) %>%
  group_by(seas, week, def.x) %>%
  mutate(snaps_pass = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = men_in_box_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_pass), .groups = "drop") 

qb_pos <- ftn_charts %>%
  left_join(ftn_plays, by = c("gid", "pid"), relationship = "many-to-many") %>%
  left_join(ftn_games, by = "gid", relationship = "many-to-many") %>%
  filter(type.x == "PASS" | type.x == "RUSH", !is.na(off.x), !is.na(def.x)) %>%
  mutate(qb_pos_bracket = case_when(
    qb_pos == "U" ~ "Under Center",  
    qb_pos == "S" ~ "Shotgun",  
    qb_pos == "P" ~ "Pistol")) %>%
  group_by(seas, week, off.x) %>%
  mutate(snaps_play = n()) %>%
  ungroup() %>%
  group_by(seas, team = def.x, opp = off.x, week, category = qb_pos_bracket) %>%
  summarize(plays = n(),
            rate = plays/last(snaps_play), .groups = "drop") 

wp <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), pass + rush == 1) %>%
  mutate(wp_bracket = case_when(
    wp < 0.2 ~ "Less than 20%",  
    wp >= 0.2 & wp < 0.4 ~ "20%-40%",  
    wp >= 0.4 & wp < 0.6 ~ "40%-60%",           
    wp >= 0.6 & wp < 0.8 ~ "60%-80%",  
    wp >= 0.8 ~ "More than 80%")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_play = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = wp_bracket) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_play))

yards_gained <- load_pbp(SEASON) %>%
  filter(!is.na(epa), !is.na(down), pass + rush == 1) %>%
  mutate(yards_gained_bracket = case_when(
    yards_gained < 0 ~ "Loss of Yards",  
    yards_gained >= 0 & yards_gained <= 3 ~ "0-3 yards",  
    yards_gained > 3 & yards_gained <= 8 ~ "4-8 yards",           
    yards_gained > 8 & yards_gained <= 12 ~ "9-12 yards",  
    yards_gained > 12 & yards_gained <= 19 ~ "13-19 yards",            
    yards_gained > 19 ~ "20+ yards")) %>%
  group_by(season, posteam, week) %>%
  mutate(snaps_play = n()) %>%
  group_by(seas = season, team = posteam, opp = defteam, week, category = yards_gained_bracket) %>%
  reframe(plays = n(),
          rate = plays/last(snaps_play))

saveRDS(concept, "Weekly_Bar_Graph_data_Run_Concepts.rds")

saveRDS(shell, "Weekly_Bar_Graph_data_Shell_Coverages.rds")

saveRDS(air_yards, "Weekly_Bar_Graph_data_Air_Yard_Buckets.rds")

saveRDS(route, "Weekly_Bar_Graph_data_Targeted_Routes.rds")

saveRDS(ttp, "Weekly_Bar_Graph_data_Time_to_Pass.rds")            

saveRDS(ttpr, "Weekly_Bar_Graph_data_Time_to_Pressure.rds")   

saveRDS(downs, "Weekly_Bar_Graph_data_Downs.rds")   

saveRDS(ydstogo, "Weekly_Bar_Graph_data_Yards_to_Go.rds")   

saveRDS(run_gap, "Weekly_Bar_Graph_data_Run_Gap.rds")   

saveRDS(play_action, "Weekly_Bar_Graph_data_Play_Action.rds")   

saveRDS(pressure_allowed, "Weekly_Bar_Graph_data_Pressure_Allowed.rds")   

saveRDS(pass_rushers, "Weekly_Bar_Graph_data_Pass_Rushers.rds") 

saveRDS(men_in_box, "Weekly_Bar_Graph_data_Men_in_Box.rds") 

saveRDS(qb_pos, "Weekly_Bar_Graph_data_QB_Position.rds") 

saveRDS(wp, "Weekly_Bar_Graph_data_Win_Probability.rds") 

saveRDS(yards_gained, "Weekly_Bar_Graph_data_Yards Gained.rds") 
