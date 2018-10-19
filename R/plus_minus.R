#' Accounting for red cards
#'
#' This function adds the info about red card to the game_lineup
#' @param game_commentary from fcscrapR
#' @param game_lineup computed from the package
#' @param type_card is the type of card to consider. It has to be red,
#' left it flexible for testing purposes
add_red_card_info <- function(game_commentary, game_lineup, type_card = "red"){

  wwhich_card <- which(game_commentary$card_type == type_card)

  # Transform the line-up start and end data accounting for commentary info
  # into numerical
  game_lineup$time_of_sub_in <- game_lineup$time_of_sub_in %>% as.character()
  game_lineup$time_of_sub_in[game_lineup$time_of_sub_in == "start"] <- 0

  game_lineup$time_of_sub_out <- game_lineup$time_of_sub_out %>% as.character()
  game_lineup$time_of_sub_out[game_lineup$time_of_sub_out == "end"] <-
    max(game_commentary$match_time_numeric, na.rm = TRUE)

  game_lineup$time_of_sub_in  <- game_lineup$time_of_sub_in %>% as.numeric
  game_lineup$time_of_sub_out <- game_lineup$time_of_sub_out %>% as.numeric


  # If there is at least one red card select the players
  if(length(wwhich_card) > 0){
    players_card <- game_commentary$card_player[wwhich_card]
    minutes_card <- game_commentary$match_time_numeric[wwhich_card]
    # game_lineup$time_of_sub_out <- game_lineup$time_of_sub_out %>% as.character()

    for(ii in 1:length(players_card)){
      wwhich <- which(game_lineup$lineup == players_card[ii])
      game_lineup$time_of_sub_out[wwhich] <- minutes_card[ii] # %>% as.character()
    }
  }
  return(game_lineup)
}

#' Creating segmentation
#'
#' This function creates segmentation every time a the configuration
#' on the pitch changes
#' @param game_commentary from fcscrapR
#' @param game_lineup computed from the package
create_segmentation <- function(game_lineup){

  # Get the order line up by time_of_sub_out
  ordered_lineup <- game_lineup[order(game_lineup$time_of_sub_out %>% as.character() %>%
                                        as.numeric(), decreasing = FALSE),]

  # Create the segmentation matrix
  segments <- c(0, ordered_lineup$time_of_sub_out %>% unique) %>% as.numeric()
  segment_matrix <- matrix(NA, ncol = 2, nrow = length(segments) - 1)
  for(j in 1:(length(segments) - 1))
    segment_matrix[j,] <- c(segments[j], segments[j + 1])

  return(segment_matrix)
}

#' Creating the shot attempt by segmentation for both teams
#'
#' This function creates the shot attempt matrix by segmentation for both teams
#' @param game_commentary from fcscrapR
#' @param segmentation_matrix a matrix with each row a segment, each coloumn
#' the start and the end of the segment
get_shot_attempt_by_segment <- function(game_commentary, segmentation_matrix){

  shot_locations_levels <- c("outside_the_box", "the_centre_of_the_box",
                             "right_shot_very_close_range", "the_right_side_of_the_box",
                             "the_left_side_of_the_box", "the_left_side_of_the_six_yard_box",
                             "the_right_side_of_the_six_yard_box", "right_shot_a_difficult_angle_and_long_range",
                             "very_close_range", "left_shot_long_range", "left_shot_very_close_range",
                             "left_shot_a_difficult_angle_and_long_range",
                             "right_shot_long_range", "with_an_attempt_very_close_range")
  shot_locations_levels_commentary <- stringr::str_replace_all(shot_locations_levels,
                                                               "_", " ")

  # Creating all the columns
  other_cols <- expand.grid(c("shot_saved", "shot_missed", "shot_blocked",
                              shot_locations_levels,
                              "shots_on_goal", "goal", "corner",
                              "offside", "foul", "free_kick"), c("home_", "away_")) %>%
    apply(1, function(x) paste0(x[2], x[1], collapse = " "))


  stat_by_segment <-  matrix(0, ncol = length(other_cols) + 2,
                            nrow = nrow(segmentation_matrix)) %>% as.data.frame()
  colnames(stat_by_segment) <- c("ts", "tf", other_cols)

  # Define team one and team two
  home_team <- game_commentary$team_one %>% unique
  away_team <- game_commentary$team_two %>% unique

  # Initialize stat_by_sement
  stat_by_segment[,1:2] <- segmentation_matrix[,1:2]


  # Loop through all the segments
  for(t in 1:nrow(segmentation_matrix)){

    if(stat_by_segment$tf[t] > stat_by_segment$ts[t]){

      # Find which index in the commentary is contained in the time segment
      wwhich <- which((game_commentary$match_time_numeric <
                         stat_by_segment$tf[t]) &
                        (game_commentary$match_time_numeric >=
                           stat_by_segment$ts[t]))
    } else{
      wwhich <- which(game_commentary$match_time_numeric == stat_by_segment$tf[t])
    }

    # If there is any shot in the interval tf - ts
    if(length(wwhich) > 0){

      # Check if there is shootout, it needs to be deleted or it alters the
      # scoring rate
      commentary_t <- game_commentary$commentary[wwhich]
      wwhich_shootout <- which(startsWith(commentary_t, "Penalty Shootout begins"))
      if((wwhich_shootout %>% length) > 0)
        wwhich <- wwhich[1:(wwhich_shootout - 1)]

      # Build the segment matrix for each column -------------------------------
      # + Shot type
      shot_table <- table(factor(game_commentary$shot_result[wwhich],
                                 levels = c("blocked", "goal", "missed", "saved")),
                          factor(game_commentary$shot_by_team[wwhich], levels = c(home_team, away_team)))

      for(word in c("blocked", "missed", "saved")) {

        stat_by_segment[t, grep(word, colnames(stat_by_segment))] <-
          shot_table[grep(word, rownames(shot_table)),]
      }
      # Need to be separate or it overwrites shots_on_goal
      stat_by_segment[t, c("home_goal", "away_goal")] <- shot_table["goal",]

      # Create shots on goal
      stat_by_segment$home_shots_on_goal[t] <- stat_by_segment$home_shot_saved[t] +
        stat_by_segment$home_goal[t]
      stat_by_segment$away_shots_on_goal[t] <- stat_by_segment$away_shot_saved[t] +
        stat_by_segment$away_goal[t]

      # Shot Location
      shot_table <- table(factor(game_commentary$shot_where[wwhich],
                                 levels = shot_locations_levels_commentary),
            factor(game_commentary$shot_by_team[wwhich], levels = c(home_team, away_team)))

      for(word in shot_locations_levels) {
        stat_by_segment[t, grep(word, colnames(stat_by_segment))] <-
          shot_table[grep(stringr::str_replace_all(word, "_", " "), rownames(shot_table)),]
      }

      # + Corner, Offside, Foul, Free kick
      stat_by_segment[t, c("home_corner", "away_corner")] <-
        table(factor(game_commentary$corner_team[wwhich], levels = c(home_team, away_team)))
      stat_by_segment[t, c("home_offside", "away_offside")] <-
        table(factor(game_commentary$offside_team[wwhich], levels = c(home_team, away_team)))
      stat_by_segment[t, c("home_foul", "away_foul")] <-
        table(factor(game_commentary$foul_by_team[wwhich], levels = c(home_team, away_team)))

      free_kick_table <- table(factor(game_commentary$free_kick_where[wwhich],
                                      levels = c("attacking half", "defensive half")),
                               factor(game_commentary$free_kick_team[wwhich], levels = c(home_team, away_team)))
      stat_by_segment[t, c("home_free_kick", "away_free_kick")] <- free_kick_table

    }
  }

  return(stat_by_segment)
}






#' Real Plus Minus matrix
#'
#' This function creates the design matrix for a given league, given the lineups
#' and the segmentation matrix
#' @param lineups all lineups by league stacked in a matrix
#' @param segments all segments by league stacked in a matrix
create_design_matrix <- function(lineups, segments){

  if(is.null(lineups$espn_id)) stop("The column with espn_id is missing")
  lineups <- lineups[!is.na(lineups$espn_id), ]

  players   <- gsub(" ", "_", unique(as.character(lineups$lineup)))
  n_players <- length(unique(as.character(lineups$lineup)))

  # Create the new segment matrix with a column for player
  new_segments  <- cbind(segments, matrix(0, nrow(segments), n_players))
  names(new_segments)[(ncol(segments) + 1) : ncol(new_segments)] <- players

  for (i in 1:nrow(lineups)){
    player   <- gsub(" ", "_", as.character(lineups$lineup)[i])
    game     <- lineups$espn_id[i]
    time_in  <- lineups$time_of_sub_in[i]
    time_out <- lineups$time_of_sub_out[i]

    # Espn has some error and empty names. If that is the case skip
    if(stringr::str_length(player) > 1) {
      new_segments[(new_segments$espn_id == game) & (new_segments$ts >= time_in) &
                     (new_segments$tf <= time_out), player] =
        ifelse(lineups$team[i] == "home", 1, -1)
    }
  }
  return(new_segments)
}


#' Compute the real-plus minus
#'
#' This function computes real plus minus and threshold by minutes player
#' if requested
#' @param design_matrix 0, -1, 1 matrix with info about shots and goals
#' @param threshold for minutes, by default 0
#' @param minutes dataframe with minutes info by player
#' @param response character to chose as response, either goal, or shots
#' @param type can be offensive, defensive or both which takes the differnce
#' in the response
get_rpm <- function(design_matrix, threshold = 0, minutes, response = "goal", type = "both") {

  if(!any(stringr::str_detect(colnames(design_matrix), response)))
    stop(paste("The string ", response, " deoesn't exist in the design matrix.
               You might have mispelled the world, or used the plural."))

  if(!(type %in% c("both", "offense", "defense")))
    stop("type can be one of these three: both, offense, defense.")

  # Identify where the players info is located in the matrix
  response_home <- paste0("home_", response)
  response_away <- paste0("away_", response)
  wwhich_design <- grep("[A-Z]", colnames(design_matrix))

  # Determine y based on type of rpm
  if(type == "both")
    y <- as.numeric(design_matrix[, response_home] - design_matrix[, response_away])
  if(type == "offense")
    y <- as.numeric(design_matrix[, response_home])
  if(type == "defense")
    y <- as.numeric(design_matrix[, response_away])

  x <- as.matrix(design_matrix[,wwhich_design])
  weights <- 1 / (design_matrix[, "tf"] - design_matrix[, "ts"])

  # Ridge regression
  model_cv <- glmnet::cv.glmnet(x = x, y = y, weights = weights, alpha = 0)
  model <- glmnet::glmnet(x = x, y = y, weights = weights, alpha = 0,
                          lambda = model_cv$lambda.min)
  coefs <- model$beta

  # Build the dataframe with coeffs
  df <- data.frame(betas = coefs@x, names = coefs@Dimnames[[1]])

  # Subselect based on threshold
  more_than_thresh <- minutes[which(minutes$tot_mins > threshold), "name_underscore"]
  thresh_rows <- which(df$names %in% as.character(more_than_thresh))

  df_thresh <- df[thresh_rows, ]
  df_tresh_sorted <- df_thresh[order(df_thresh$betas, decreasing=TRUE), ]

  return(df_tresh_sorted)
}
