#' Scrape sofifa tables
#'
#' @param url url for the sofifa table
#'
scrape_sofifa_table <- function(url) {
  xml_url <- xml2::read_html(url)
  tables <- rvest::html_nodes(xml_url, "table")
  sofifa_table <- rvest::html_table( tables[1][[1]] )

  links <- rvest::html_nodes(x = tables, css = "td") %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
  titles <- rvest::html_nodes(x = tables, css = "td") %>% rvest::html_nodes("a") %>% rvest::html_attr("title")
  link_title_df <- data.frame(link=as.character(links), title=as.character(titles))

  return(list(sofifa_table=sofifa_table, link_title_df=link_title_df))
}

#' Clean raw sofifa table
#'
#' @param tab data-frame of sofifa table extracted from \code{scrape_sofifa_table}
#'
clean_sofifa_table <- function(tab) {
  sofifa_table <- tab$sofifa_table[2:nrow(tab$sofifa_table), c(2, 3, 4, 5, 6, 8, 9)]
  names(sofifa_table) <- c("name", "age", "overall", "potential", "team", "value", "wage")

  # Extract full player names from the links
  player_name_rows <- grep(pattern = "/player/", tab$link_title_df$link)
  player_names <- as.character(tab$link_title_df$title[player_name_rows])
  stopifnot(length(player_names) == nrow(sofifa_table))
  sofifa_table$full_name <- player_names

  # Extract player coutry from links
  player_country_rows <- player_name_rows - 1
  # ERROR IN THIS LINE IN RUSSIA: grep(pattern = "/players\\?na=", x = tab$link_title_df$link)
  player_country <- as.character(tab$link_title_df$title[player_country_rows])
  stopifnot(length(player_country) == nrow(sofifa_table))
  sofifa_table$country <- player_country

  # Split the first column into "name" and "position"
  name_pos_split <-stringr::str_split_fixed(string = sofifa_table$name, pattern = "\t", n=2)
  sofifa_table$name <- name_pos_split[, 1]
  pos <- name_pos_split[, 2]
  pos <- stringr::str_replace_all(string = pos, pattern = "\t", replacement = "")
  sofifa_table$position <- pos

  # Parse Team
  team_split <- stringr::str_split_fixed(sofifa_table$team, pattern = "\t", n=2)
  sofifa_table$team <- team_split[, 1]

  return(sofifa_table)
}
