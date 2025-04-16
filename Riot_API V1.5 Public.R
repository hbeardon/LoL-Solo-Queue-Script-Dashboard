## Install and Load Packages
library(httr)
library(jsonlite)
library(dplyr)

## Define Token & Riot ID
token <- "Your API Key here"
gameName <- "Your LoL In-Game Name"
tagLine <- "Your LoL tagline"

## Build URL to get puuid
base_url <- "https://europe.api.riotgames.com/riot/account/v1/accounts/by-riot-id/"
full_url <- paste0(base_url, gameName, "/", tagLine, "?api_key=", token)

## Make API Call to get puuid
response <- GET(full_url)

## Check Response Status and Extract puuid
if (status_code(response) == 200) {
  data <- content(response, as = "parsed")
  puuid <- data$puuid
  
  ## Match parameters
  queueid <- "420"
  type <- "ranked"
  indexstart <- 0
  count <- 100
  match_ids <- c()
  
  repeat {
    match_url <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/", puuid, 
                        "/ids?queue=", queueid, "&type=", type, 
                        "&start=", indexstart, "&count=", count, "&api_key=", token)
    
    match_response <- GET(match_url, add_headers("X-Riot-Token" = token))
    
    if (status_code(match_response) == 200) {
      batch_ids <- content(match_response, as = "parsed")
      if (length(batch_ids) == 0) {
        print("All match IDs retrieved.")
        break
      }
      match_ids <- c(match_ids, batch_ids)
      indexstart <- indexstart + count
    } else {
      print("Failed to retrieve match IDs for current batch.")
      break
    }
  }
  
  ## Fetch match details
  all_matches <- lapply(match_ids, function(match_id) {
    Sys.sleep(1.2)
    match_full_url <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/", match_id)
    match_detail_response <- GET(match_full_url, add_headers("X-Riot-Token" = token))
    
    if (status_code(match_detail_response) == 200) {
      match_detail <- content(match_detail_response, as = "parsed")
      participants <- match_detail$info$participants
      filtered_participants <- lapply(participants, function(participant) {
        participant <- participant[setdiff(names(participant), c("perks", "challenges", "missions"))]
        participant$matchId <- match_id
        return(participant)
      })
      return(filtered_participants)
    } else {
      return(NULL)
    }
  })
  
  ## Clean up
  all_matches <- all_matches[!sapply(all_matches, is.null)]
  
  all_columns <- unique(unlist(lapply(all_matches, function(match) {
    unique(unlist(lapply(match, names)))
  })))
  
  all_participants <- lapply(all_matches, function(match) {
    lapply(match, function(participant) {
      participant[setdiff(all_columns, names(participant))] <- NA
      participant <- participant[all_columns]
      participant
    })
  })
  
  all_participants_flat <- do.call("rbind", lapply(all_participants, function(x) do.call("rbind", x)))
  all_participants_df <- as.data.frame(all_participants_flat, stringsAsFactors = FALSE)
  
  ## Convert list columns to JSON if needed
  all_participants_df <- as.data.frame(lapply(all_participants_df, function(column) {
    if (is.list(column)) {
      sapply(column, toJSON, auto_unbox = TRUE)
    } else {
      column
    }
  }), stringsAsFactors = FALSE)
  
  ## Clean summoner names
  all_participants_df$summonerName <- as.character(all_participants_df$summonerName)
  all_participants_df$riotIdGameName <- as.character(all_participants_df$riotIdGameName)
  
  all_participants_df$summonerName[!grepl("[A-Za-z0-9]", all_participants_df$summonerName)] <- 
    all_participants_df$riotIdGameName[!grepl("[A-Za-z0-9]", all_participants_df$summonerName)]
  
  ## === RANK + TIER LOOKUP SECTION ===
  
  ## Get unique summonerId/summonerName combos
  unique_players <- all_participants_df %>%
    select(summonerId, summonerName) %>%
    distinct() %>%
    group_by(summonerId) %>%
    slice(1) %>%
    ungroup()
  
  ## Create empty rank data frame
  summoner_ranks <- data.frame(summonerId = character(), tier = character(), rank = character(), stringsAsFactors = FALSE)
  
  ## Loop to fetch rank by summonerId
  for (i in 1:nrow(unique_players)) {
    Sys.sleep(1.2)
    id <- unique_players$summonerId[i]
    
    rank_url <- paste0("https://euw1.api.riotgames.com/lol/league/v4/entries/by-summoner/", id, "?api_key=", token)
    rank_response <- GET(rank_url)
    
    if (status_code(rank_response) == 200) {
      rank_data <- content(rank_response, as = "parsed")
      soloq_entry <- Filter(function(entry) entry$queueType == "RANKED_SOLO_5x5", rank_data)
      
      if (length(soloq_entry) > 0) {
        summoner_ranks <- rbind(summoner_ranks, data.frame(
          summonerId = id,
          tier = soloq_entry[[1]]$tier,
          rank = soloq_entry[[1]]$rank,
          stringsAsFactors = FALSE
        ))
      } else {
        summoner_ranks <- rbind(summoner_ranks, data.frame(
          summonerId = id,
          tier = NA,
          rank = NA,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      message(paste("Failed to retrieve rank for ID:", id))
    }
  }
  
  ## Remove accidental duplicates, just in case
  summoner_ranks <- summoner_ranks %>%
    group_by(summonerId) %>%
    slice(1) %>%
    ungroup()
  
  ## Merge rank info into participants df
  all_participants_df <- left_join(all_participants_df, summoner_ranks, by = "summonerId")
  
  ## Save to CSV
  write.csv(all_participants_df, file = "soloq_match_data_with_rank.csv", row.names = FALSE, quote = FALSE)
  print("Data saved to soloq_match_data_with_rank.csv")
  
} else {
  print("Failed to retrieve puuid")
}
