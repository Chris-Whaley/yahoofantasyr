#' Get Game Keys for All Available Fantasy Seasons
#'
#' @returns Data frame of all game metadata
#' @export
#'
#' @examples all_nfl_seasons <- yf_gamekeys()
yf_gamekeys <- function() {
  result <- yahoofantasyr::yf_get(paste0("games;game_codes=nfl"))
  data <- result$fantasy_content$games

  temp_list <- list()

  for (season in names(data)) {
    if (season != "count") {
      season_df <- purrr::lmap(data[[season]]$game[[1]], tibble::as_tibble) |>
        purrr::list_cbind()

      temp_list[[season]] <- season_df
    }
  }

  df <- dplyr::bind_rows(temp_list)
  # df <- bindRows(temp_list)

  return(df)
}


#' Get the Game Key for the Current Fantasy Season
#'
#' @returns Game Key id for current season
#' @export
#'
#' @examples current_seasons_gamekey <- yf_gamekey_current_season()
yf_gamekey_current_season <- function() {
  result <- yahoofantasyr::yf_get(paste0("game/nfl"))
  data <- result$fantasy_content$game[[1]]

  df <- tibble::as_tibble(data)
  message(paste("Returning game key for", df$season, df$code, "season."))
  return(df$game_key)
}


#' Get Specific Game Data for Select Seasons using Game Key
#'
#' @param game_key Identifier for a fantasy sport's season
#'
#' @returns Data frame of metadata for input game keys
#' @export
#'
#' @examples nfl_461 <- yf_game_from_key("461")
yf_game_from_key <- function(game_key) {
  result <- yahoofantasyr::yf_gamekeys()
  df <- result |>
    dplyr::filter(game_key %in% .env$game_key)
  # df <- result[result$game_key %in% game_key, , drop = FALSE]


  return(df)
}


#' Get Specific Game Data for Select Seasons using Year
#'
#' @param year Season year for fantasy sport
#'
#' @returns Data frame of metadata for input years
#' @export
#'
#' @examples nfl_2025 <- yf_game_from_year(2025)
yf_game_from_year <- function(year) {
  result <- yahoofantasyr::yf_gamekeys()
  df <- result |>
    dplyr::filter(season %in% year)
  # df <- result[result$season %in% year, , drop = FALSE]
  return(df)
}
