yf_player <- function(player_key) {
  result <- yahoofantasyr::yf_get(paste0("player/", player_key))
  data <- result$fantasy_content$player[[1]]

  vars <- c(
    "player_key", "player_id", "full", "editorial_team_full_name",
    "editorial_team_abbr", "week", "display_position", "url"
  )

  df <- map(vars, ~ find_variable_in_nested_list(data, .x)) |>
    set_names(vars) |>
    as.tibble()

  return(df)
}

# TODO: figure out code for combos (all season, one week current season, one week prev season)
yf_player_stats <- function(player_key, type = "week", week = NULL, season = NULL) {
  # type: season, week
  # week: week of season
  # season: defaults to latest/current season
  path <- paste0("player/", player_key, "/stats;type=", type)
  if (!is.null(season)) path <- paste0(path, ";season=", season)
  if (!is.null(week)) path <- paste0(path, ";week=", week)

  result <- yahoofantasyr::yf_get(path)
  data <- result$fantasy_content$player
}

yf_player_draft_analysis <- function(player_key) {
  result <- yahoofantasyr::yf_get(paste0("player/", player_key, "/draft_analysis"))
  data <- result$fantasy_content$player[[2]]$draft_analysis
  df <- data |>
    purrr::map(as.tibble) |>
    purrr::list_cbind() |>
    dplyr::mutate(player_key = player_key) |>
    dplyr::relocate(player_key)
}
