ftn_game_data <- function(type = "charts", 
                          year = nflreadr::get_current_season()){
  
  ### creating empty data frame to store information
  api_data <- data.frame()
  
  ### base URL and row count limit information
  url_base <- paste0("https://data.ftndata.com/games/", year, "/", type)
  
  # charts, participation, plays
  count <- 1000
  start <- 0
  
  ### adding authorization code into header
  auth_header <- "bd99fcb1-bc62-4655-822c-9eb02d09d420"
  
  ### creating function to gather all 2023 data
  while (TRUE) {
    
    url <- paste0(url_base, "?count=", count, "&start=", start)
    
    response <- GET(url, add_headers(
      'Accept' = 'application/json',
      'Authorization' = auth_header))
    
    ### check if the request was successful
    if (status_code(response) != 200) {
      print(paste0("Request failed with status ", status_code(response)))
      break
    }
    
    ### parsing and converting JSON into a data frame
    extracted_data <- content(response, "text", encoding = "UTF-8")
    df <- fromJSON(extracted_data, flatten = TRUE)
    
    ### check if data is empty, and break the loop if it is
    if (nrow(df) == 0) {
      break
    }
    
    ### adding collected data to the master data frame
    api_data <- rbind(api_data, df)
    
    ### check if type is one of charts, participation, or plays, 
    ### and break the loop if it is
    if (!type %in% c("charts", "participation", "plays")) {
      break
    }
    
    ### update the start parameter for the next loop
    start <- start + count
    
    ### showing progress of loop
    print(paste("Fetched", nrow(api_data), "rows so far."))
  }
  
  ### showing completion of process
  print(paste0("Collection of FTN ", type,  " data is complete."))
  
    write_csv(api_data, paste0("FTN_", type, "_", year, ".csv"))
}
  
}




ftn_other_data <- function(type = "players"){
  
  ### adding authorization code into header
  auth_header <- "bd99fcb1-bc62-4655-822c-9eb02d09d420"
  
  ### URL
  url <- paste0("https://data.ftndata.com/", type)
  
  response <- GET(url, add_headers(
    'Accept' = 'application/json',
    'Authorization' = auth_header))
  
  ### check if the request was successful
  if (status_code(response) != 200) {
    print(paste0("Request failed with status ", status_code(response)))
  }
  
  ### parsing and converting JSON into a data frame
  extracted_data <- content(response, "text", encoding = "UTF-8")
  api_data <- fromJSON(extracted_data, flatten = TRUE)
  
  ### showing completion of process
  print(paste0("Collection of FTN ", type,  " data is complete."))
  
    write_csv(api_data, paste0("FTN_", type, "_", year, ".csv"))
}
  
}
