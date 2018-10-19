#' Scrape lineups for ESPN Game ID
#'
#' This function scrapes lineup for a given game on espn
#'
#' @param game_id espn id
#'
#' @return list with the two line ups plus subs info
#'
scrape_lineup <- function(game_id) {
  if (length(game_id) > 1) { stop("game_id should be length one!") }
  team_info <- list(); teams <- NULL; n_players <- 11
  game_id   <- as.character(game_id)

  # Compose the url
  url <- paste0("http://www.espn.com/soccer/lineups?gameId=", game_id)

  # All the substitution pattern are "#'-#, but extra time has pattern "90+#'-#
  regular_time_pattern <- expand.grid(1:120, 1:99) %>%
    apply(1, function(x) paste0(x[1], "'-", x[2], ""))
  extra_time_pattern1 <- expand.grid(paste0("45'\\+", 1:9, "'-"), 1:99) %>%
    apply(1, function(x) paste0(x[1], x[2]))
  extra_time_pattern2 <- expand.grid(paste0("90'\\+", 1:9, "'-"), 1:99) %>%
    apply(1, function(x) paste0(x[1], x[2]))
  extra_time_pattern3 <- expand.grid(paste0("120'\\+", 1:9, "'-"), 1:99) %>%
    apply(1, function(x) paste0(x[1], x[2]))
  substitution_pattern <- c(regular_time_pattern, extra_time_pattern1,
                            extra_time_pattern2, extra_time_pattern3) %>%
    stringr::str_replace_all(" ", "")

  # For each of the two teams
  for (team_j in 1:2) {

    url_nodes <- url %>% xml2::read_html() %>% rvest::html_nodes("table")

    if (length(url_nodes) < 2) {
      cat("Game info", game_id, "- there is no line-up available for this game! \n")
      return(NULL)
    }

    # Extract the line up from the espn url
    team_lineup <- url_nodes[[team_j]] %>%
      rvest::html_text() %>%
      stringr::str_replace_all(pattern = "\n", replacement = "-") %>%
      stringr::str_replace_all(pattern = "\t", replacement = "")

    team <- stringr::str_extract(team_lineup, pattern = "(?<=--).*(?=No.-Name)") %>%
      stringr::str_replace_all(pattern = "-", replacement = "")
    teams <- c(teams, team)
    team_lineup <- sub('.*\\.-Name', '', team_lineup)


    # PLAYER EXTRACTION DIFFERS BETWEEN GAMES
    # First remove buzzwords that confuse player name number pattern
    buzz_words <- c("Saves", "Goals", "Shots on Target", "Shots", "Fouls Committed",
                    "Fouls Against", "Assists", "Discipline", "Yellow", "Red", "Offsides")
    for(kk in 1:length(buzz_words))
      team_lineup <- stringr::str_replace_all(team_lineup, buzz_words[kk], "")

    # Split players
    player_by_player_data <- strsplit(team_lineup, "-------------------") %>%
       unlist(use.names = FALSE)

    # There is no boxscore stats for some player, need to re-split
    # according to ad different pattern
    length_roster <- length(player_by_player_data)
    if(length_roster < n_players) {
      player_by_player_data <- split_lineup_when_no_stats(team_lineup)
    }


    # This shouldn't be a problem if we eliminate games not played yet
    # if(!(stringr::str_detect(team_lineup, "Shots") %>% any))
    #     player_by_player_data <- set_player_by_player_data_other_pattern(team_lineup)
    line_up <- time_of_sub_in <- time_of_sub_out <- c(); count_sub <- 0
    for(ii in 1:n_players){


      # Extract player name
      line_up[ii] <- get_player_name(player_by_player_data[ii])
      time_of_sub_in[ii]  <- "start"
      time_of_sub_out[ii] <- "end"

      # If the player was sub out
      is_player_sub_out <- stringr::str_detect(player_by_player_data[ii],
                                               substitution_pattern)

      # At the beginning the player sub out is the ii-th of the starting lineup
      jj <- ii

      # Until we get sub out info we stay on the same line. A player could sub
      # in and be sub out
      while(is_player_sub_out %>% any()){

        count_sub <- count_sub + 1
        wwhich <- which(is_player_sub_out == TRUE)

        # There could be multiple strings that match, the longest are the exact match
        l <- sapply(as.list(wwhich), function(x)
          stringr::str_length(substitution_pattern[x]))
        wwhich <- wwhich[which(l == max(l))]

        # if there are multiple subs length wwwhich > 1, you need to pick
        # the right pattern, which is the first that occurs
        where_all_subs_start <- sapply(stringr::str_locate_all(pattern = substitution_pattern[wwhich],
                                                               player_by_player_data[ii]),
                                       function(x) x[,"start"])
        which_first <- which(where_all_subs_start == min(where_all_subs_start))
        wwhich <- wwhich[which_first]

        # Get the time of substitution
        time_of_sub_out[jj] <- stringr::str_extract(substitution_pattern[wwhich],
                                                    "\\-*\\d+\\.*\\d*")
        time_of_sub_in[n_players + count_sub]  <- time_of_sub_out[jj]
        time_of_sub_out[n_players + count_sub] <- "end"

        # Remove everything before the pattern, so that only the sub pattern remains
        where_sub_pattern <- stringr::str_locate(pattern = substitution_pattern[wwhich],
                                                 player_by_player_data[ii])
        substitution_string <- substring(player_by_player_data[ii],
                                         where_sub_pattern[2])
        # Compute the player name
        line_up[n_players + count_sub] <- get_player_name(substitution_string)

        # Update the player string
        player_by_player_data[ii] <- substitution_string
        is_player_sub_out <- stringr::str_detect(player_by_player_data[ii],
                                                 substitution_pattern)
        # Update the sub: now the possible sub out guy is the one just sub-in
        jj <- n_players + count_sub
      }
    }
    # Fill out the time of sub and provide the list
    team_info[[team_j]] <- data.frame(lineup = line_up, time_of_sub_in = time_of_sub_in,
                                      time_of_sub_out = time_of_sub_out,
                                      team = rep(ifelse(team_j == 1, "home", "away"),
                                                 length(line_up)))

  }

  cat("Game info", game_id, "-", teams[1], "-", teams[2], "\n")
  team_info <- Reduce(rbind, team_info)
  return(team_info)
}


#' Split the lineup when no stats are available for some players
#'
#' This function splits the lineup info when the stats are missing for some players
#' @param team_lineup team lineup extracted by espn removed of buzzwords and extra
#' spaces
split_lineup_when_no_stats <- function(team_lineup){

  n_players <- 11

  # First eliminate multiple space pattern
  team_lineup <- stringr::str_replace_all(team_lineup, "-\\s-", "")

  # Find the new splitting point
  new_splitting_point <- c(gregexpr("-[0-9]+----[A-Z]", team_lineup) %>%
                             unlist(use.names=FALSE))

  # Split the line up based on the Number ----- Name pattern
  player_by_player_data <- c()
  for(ii in 1:n_players) {
    player_by_player_data[ii] <- substring(team_lineup, new_splitting_point[ii] + 1,
                                           new_splitting_point[ii + 1])
  }
  return(player_by_player_data)
}


#' Extract the player name given a string with player data info
#'
#' This function extracts the player name given a string with player data info
#' @param player_by_player_string string with a single player info
get_player_name <- function(player_by_player_string){

  where_name_starts <- gregexpr("[A-Z]", player_by_player_string)[[1]][1]
  player_name <- substring(player_by_player_string, where_name_starts)
  where_name_ends <- gregexpr("-", player_name)[[1]][1]
  player_name <- substring(player_name, 1, where_name_ends - 1)

  return(player_name)
}
