#' Block comparison
#'
#' This function shows  the espn name with one or more fifa candidates and ask the
#' user to decide which one is the most plausible match. The user can input
#' the row number of the selected match, or 'y' if the only row is shown and the
#' user believes that is a match, 0 if the user doesn't see any plausible match
#' @param records table of records with espn, fifa name and some additional info
#' @return  the user answer
block_comparison <- function(records) {

  cat("Choose the row you think it's a match. Enter 0 if you think there is no match. \n")
  cat("If there is a single row shown, you can also enter 'y' for yes. \n")

  print(records)
  user_answer <- readline("Which row is a match? ")

  return(user_answer)
}

#' Active comparison between records
#'
#' This function shows one at the time all the comparisons,
#' for all unique espn players in data
#' @param data dataframe with espn names and fifa names and possibly additional
#' information
#' @return vector with all answers
active_comparison <- function(data) {

  unique_players <- data$espn_name %>% unique
  matches <- c()

  for (ii in 1:length(unique_players)) {
    wwhich <- which(data$espn_name == unique_players[ii])

    # Check that the input is correct
    not_valid <- TRUE
    while (not_valid) {
      result <- block_comparison(data[wwhich, ])

      if (length(wwhich)==1 & result=='y') result <- rownames(data[wwhich, ])
      if (result %in% c(0, rownames(data[wwhich, ]))) {
        not_valid <- FALSE
      } else {
        cat("ATTENTION: The row number you enter isn't valid, please check your answer again. \n")
      }
    }
    matches <- c(matches, result)
  }

  return(matches)
}


#' Insert fifa name
#'
#' This function allows the user to insert the fifa name, given the espn name
#' and some other info
#' @param record is the espn name with possibly additional info on the player
#' @return the answer of the user
insert_fifa_name <- function(record) {

  cat("Insert the fifa name for the record shown below or NA if uknown.\n \n")

  print(record)

  user_name <- readline("Your answer: ")
  return(user_name)
}

#' Active insertion of fifa names
#'
#' This function allow the user to insert the fifa names for all unique espn players
#' in data
#' @param data dataframe with espn names and possibly additional
#' information
#' @return vector with all answers. Either NA or the answer of the user
active_insert <- function(data) {

  if(!("espn_name" %in% names(data))) stop("espn_name must be in the data")

  unique_players <- data$espn_name %>% unique
  matches <- c()

  for (ii in 1:length(unique_players)) {
    wwhich <- which(data$espn_name == unique_players[ii])
    result <- insert_fifa_name(data[wwhich, ])

    if (result == 'na' | result == 'Na' | result == 'nA') result <- 'NA'
    matches <- c(matches, result)

  }

  return(matches)
}
