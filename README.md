# PlusMinusData

# Introduction
Welcome to PlusMinusData!

This package will allow you to download play by play data from espn.com and sofifa.com and perform active record linkage. 

## Donwload the package and other dipendencies
You can get started by downloading the package and some dependencies

```{r, eval = FALSE}
devtools::install_github("ryurko/fcscrapR")
devtools::install_github("fmatano/PlusMinusData")
library(magrittr)
library(fcscrapR)
library(PlusMinusData)
```

## Donwload play-by-play data, for a given game
Here is an example on how to download play-by-play data for a given game

We first choose league and season
```{r}
league_selected <- "english premier league"
years <- 2017
```

You can select a date of interest and scrape all the games that occurred that day

```{r, eval = FALSE}
date_selected <- as.Date("2017-10-14")
espn_games <- fcscrapR::scrape_scoreboard_ids(scoreboard_name=league_selected, game_date=date_selected)
```
You'll notice that there are games that don't belong to the league selected. This happens because espn always displays your current day's games on their page. You need to make sure to select the one game or all the games you are actually interested in, or remember to delete all the current day's games. 

Let's for instance download play-by-play data for the match match Liverpool - Manchester United. 

We start by extracting the game lineup as follows 

```{r, eval = FALSE}
game_id <- "480831"
lineup <- scrape_lineup(game_id=game_id)
```
Then we add minutes, events and red-card info from the commentary data

```{r, eval = FALSE}
game_commentary <- fcscrapR::scrape_commentary(game_id=game_id)
lineup <- add_red_card_info(game_commentary=game_commentary, game_lineup=lineup)

lineup
```
Finally we compute the segments of the game and combine all the events that happened in that segment for home and away team respectively
```{r, eval = FALSE}
segmentation_mat <- create_segmentation(game_lineup=lineup)
segmentation_mat <- get_shot_attempt_by_segment(game_commentary=game_commentary, segmentation_matrix=segmentation_mat)

```


The design matrix can be now created by input lineup and segmentation matrix. Notice that this can handle a dataframe with lineups and segmentation for a series of games
```{r, eval = FALSE}
lineup$espn_id <- game_id
segmentation_mat$espn_id <- game_id
design_matrix <- create_design_matrix(lineups=lineup, segments=segmentation_mat)

```
The design matrix contains all the information for the given match.
