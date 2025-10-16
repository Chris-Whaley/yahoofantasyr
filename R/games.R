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

  # df <- dplyr::bind_rows(temp_list)
  # df <- do.call(rbind, temp_list)
  # rownames(df) <- NULL
  # return(df)

  # 1. Find all unique column names
  all_cols <- unique(unlist(lapply(temp_list, names)))

  # 2. Add missing columns to each data frame
  df_list <- lapply(temp_list, function(df) {
    missing <- setdiff(all_cols, names(df))
    if (length(missing) > 0) df[missing] <- NA
    df[all_cols]  # reorder columns consistently
  })

  # 3. Combine using rbind
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL  # optional, reset rownames

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
  result <- yf_gamekeys()
  # df <- result |>
  #   dplyr::filter(rlang::.data$game_key %in% rlang::.env$game_key)
  df <- result[result$game_key %in% game_key, , drop = FALSE]


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
  result <- yf_gamekeys()
  # df <- result |>
  #   dplyr::filter(rlang::.data$season %in% rlang::.env$year)
  df <- result[result$season %in% year, , drop = FALSE]
  return(df)
}




#' Get All Leagues You Participate In
#'
#' @returns Data frame with each row a league
#' @export
#'
#' @examples my_leagues <- yf_user_leagues()
yf_user_leagues <- function() {
  result <- yahoofantasyr::yf_get("users;use_login=1/games;game_keys=nfl/leagues")
  data <- find_list_with_name(result, "leagues")

  temp_list <- list()
  for (league in names(data)) {
    if (league != "count") {
      league_df <- purrr::lmap(data[[league]]$league[[1]], tibble::as_tibble) |>
        purrr::list_cbind()

      temp_list[[league]] <- league_df
    }
  }

  # df <- dplyr::bind_rows(temp_list)
  df <- do.call(rbind, temp_list)
  return(df)

}
