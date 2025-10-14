yf_gamekeys <- function()  {
  result <- yahoofantasyr::yf_get(paste0("games;game_codes=nfl"))
  data <- result$fantasy_content$games

  temp_list <- list()

  for (season in names(data)) {
    if (season == "count") {

    } else {
      season_df <- map(data[[season]]$game[[1]], as.tibble) |>
        purrr::list_cbind()

      temp_list[[season]] <- season_df
    }
  }

  df <- dplyr::bind_rows(temp_list)
}


yf_game <- function(game_key) {
  result <- yahoofantasyr::yf_get(paste0("game/", game_key))
}


yf_game_leagues <- function(game_key) {
  result <- yahoofantasyr::yf_get(paste0("game/", game_key, "/leagues"))
}


#### User Endpoints
yf_user_games <- function() {
  result <- yahoofantasyr::yf_get("users;use_login=1/games")
}
