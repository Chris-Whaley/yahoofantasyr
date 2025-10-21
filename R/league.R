#' Get League Metadata
#'
#' @param league_key Identifier for league
#'
#' @returns Dataframe of league metadata
#' @export
#'
#' @examples league <- yf_league("461.l.442310")
yf_league <- function(league_key) {
  if (missing(league_key)) {
    league_key <- get_league_key()
  }
  result <- yahoofantasyr::yf_get(paste0("league/", league_key))
  data <- result$fantasy_content$league[[1]]
  # df <- purrr::lmap(data, tibble::as_tibble) |>
  #   purrr::list_cbind()

  df <- clean_yahoo_list(data)

  return(df)
}


#' Get All Leagues You Participate In
#'
#' @returns Data frame with a league in each row
#' @export
#'
#' @examples my_leagues <- yf_user_leagues()
yf_user_leagues <- function() {
  result <- yahoofantasyr::yf_get("users;use_login=1/games;is_available=1/leagues")
  data <- find_lists_with_name(result, "league")

  temp_list <- list()
  # for (league in seq_along(data)) {
  #   if (league != "count") {
  #     league_df <- purrr::lmap(data[[league]][[1]], tibble::as_tibble) |>
  #       purrr::list_cbind()
  #
  #     temp_list[[league]] <- league_df
  #   }
  # }

  for (index in seq_along(data)) {
    temp_list[[index]] <- clean_yahoo_list(data[[index]][[1]])
  }


  df <- dplyr::bind_rows(temp_list)
  # df <- yahoofantasyr::bindRows(temp_list)
  return(df)
}

#' Get League Settings
#'
#' @param league_key Identifier for league
#' @param settings_type Category of settings
#'
#' @returns Dataframe of league settings
#' @export
#'
#' @examples league_roster_settings <- yf_league_settings("461.l.442310")
yf_league_settings <- function(league_key, settings_type = "overview") {
  if (missing(league_key)) {
    league_key <- get_league_key()
  }
  result <- yahoofantasyr::yf_get(paste0("league/", league_key, "/settings"))
  data <- result$fantasy_content$league[[2]]

  # All Settings
  settings <- data$settings[[1]]

  # Overview
  overview_settings <- purrr::keep(settings, ~ !is.list(.x)) |>
    purrr::map_chr(~ .x[[1]] %||% NA_character_) |>
    tibble::enframe(name = "setting", value = "value")

  # Extract roster positions
  roster_positions <- settings$roster_positions |>
    purrr::map("roster_position") |>
    purrr::map(~ tibble::as_tibble(purrr::map(.x, ~ .x[[1]])))
  roster_positions <- bindRows(roster_positions)
  roster_positions$count <- as.numeric(roster_positions$count)

  # Extract stat categories
  stat_categories <- settings$stat_categories$stats |>
    purrr::map("stat") |>
    purrr::map(~ tibble::as_tibble(purrr::map(.x, ~ if (is.list(.x)) .x[[1]] else .x))) |>
    purrr::list_rbind()

  # Extract stat groups
  stat_groups <- settings$stat_categories$groups |>
    purrr::map("group") |>
    purrr::map(~ tibble::as_tibble(purrr::map(.x, ~ .x[[1]]))) |>
    purrr::list_rbind()

  # Extract stat modifiers
  stat_modifiers <- settings$stat_modifiers$stats |>
    purrr::map("stat") |>
    purrr::map(function(s) {
      bonuses <- s$bonuses |>
        purrr::map("bonus") |>
        purrr::map(~ tibble::as_tibble(purrr::map(.x, ~ .x[[1]])), .id = "bonus_id") |>
        purrr::list_rbind() |>
        dplyr::mutate(stat_id = s$stat_id[[1]]) |>
        dplyr::select(stat_id, everything())

      tibble::tibble(
        stat_id = s$stat_id[[1]],
        value = s$value[[1]],
        bonuses = list(bonuses)
      )
    }) |>
    purrr::list_rbind()

  # Break out the bonus per stat id
  bonus_table <- suppressMessages(
    stat_modifiers |>
      dplyr::select(stat_id, bonuses) |>
      tidyr::unnest(bonuses, names_repair = "unique")
  )

  bonus_table <- bonus_table[, -1]
  names(bonus_table)[names(bonus_table) == "stat_id...2"] <- "stat_id"

  # Output choice
  df <- switch(settings_type,
    "overview" = overview_settings,
    "roster" = roster_positions,
    "categories" = stat_categories,
    "groups" = stat_groups,
    "modifiers" = stat_modifiers,
    "bonuses" = bonus_table,
    stop("Invalid choice")
  )

  return(df)
}

yf_league_standings <- function(league_key) {
  result <- yahoofantasyr::yf_get(paste0("league/", league_key, "/standings"))
  data <- result$fantasy_content$league

  week <- data[[1]]$matchup_week
  teams <- data[[2]]$standings[[1]]$teams

  temp_list <- list()
  vars_meta <- c(
    "team_key", "name", "nickname", "faab_balance", "number_of_moves",
    "number_of_trades", "auction_budget_total", "auction_budget_spent"
  )

  vars_scores <- c(
    "rank", "wins", "losses", "ties", "percentage",
    "points_for", "points_against", "type", "value"
  )

  for (team in names(teams)) {
    if (team != "count") {
      team_df <- teams[[team]]$team

      # have to split up variable lookup. it was picking up the first (i.e. incorrect) "value".
      # not the "value" of the streak
      # [[1]] : team metadata (name, team key)
      # [[3]] : standings data (points for/against, rank, streaks, wins/loses)
      temp_df_meta <- purrr::map(vars_meta, ~ yahoofantasyr::find_variable_in_nested_list(team_df[[1]], .x)) |>
        rlang::set_names(vars_meta) |>
        tibble::as_tibble() |>
        dplyr::mutate(across(!c("team_key", "name", "nickname"), ~ purrr::map_dbl(.x, as.numeric)))
      temp_df_scores <- purrr::map(vars_scores, ~ yahoofantasyr::find_variable_in_nested_list(team_df[[3]], .x)) |>
        rlang::set_names(vars_scores) |>
        tibble::as_tibble() |>
        dplyr::mutate(across(!c("type"), ~ purrr::map_dbl(.x, as.numeric)))
      temp_df <- dplyr::bind_cols(temp_df_meta, temp_df_scores)

      temp_list[[team]] <- temp_df
    }
  }

  df <- dplyr::bind_rows(purrr::map(temp_list, tibble::as_tibble))

  return(df)
}


yf_league_scoreboard <- function(league_key, week = NULL) {
  path <- paste0("league/", league_key, "/scoreboard")
  if (!is.null(week)) path <- paste0(path, ";week=", week)
  result <- yahoofantasyr::yf_get(path)
  data <- result$fantasy_content$league[[2]]$scoreboard[["0"]]$matchups

  temp_list <- list()
  for (matchup in names(data)) {
    if (matchup == "count") {} else {
      matchup_df <- data[[matchup]]

      # Skip NULL or empty teams
      teams_list <- matchup_df$matchup[["0"]]$teams

      temp_df <- extract_matchup_teams(teams_list)
      temp_df$matchup_number <- matchup
      temp_list[[matchup]] <- temp_df
    }
  }

  df <- bind_rows(map(temp_list, as_tibble))
  df <- df |>
    mutate(week = week) |>
    relocate(week)

  return(df)
}

yf_league_teams <- function(league_key) {
  result <- yahoofantasyr::yf_get(paste0("league/", league_key, "/teams"))
  data <- result$fantasy_content$league[[2]]$teams

  vars <- c("team_key", "team_id", "name", "nickname")
  temp_list <- list()
  for (team in names(data)) {
    if (team == "count") {} else {
      temp_df <- purrr::map(vars, ~ yahoofantasyr::find_variable_in_nested_list(data[[team]], .x)) %>%
        set_names(vars) |>
        tibble::as_tibble()
      temp_list[[team]] <- temp_df
    }
  }

  teams_df <- yahoofantasyr::bindRows(temp_list)

  return(teams_df)
}

yf_league_players <- function(league_key) {
  yahoofantasyr::yf_get(paste0("league/", league_key, "/players"))
}

yf_league_transactions <- function(league_key) {
  yahoofantasyr::yf_get(paste0("league/", league_key, "/transactions"))
}

yf_league_draftresults <- function(league_key) {
  result <- yahoofantasyr::yf_get(paste0("league/", league_key, "/draftresults"))
  data <- result$fantasy_content$league[[2]]$draft_results

  number_of_picks <- data$count

  column_names <- c("pick", "round", "cost", "team_key", "player_key")
  df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(df) <- column_names

  for (pick in 1:number_of_picks) {
    temp_df <- map(dt[[pick]]$draft_result, tibble::as_tibble) |> list_cbind()

    df <- rbind(df, temp_df)
  }
}

# Return stored league key or error if not set
get_league_key <- function() {
  if (exists("user_league_key", envir = the)) {
    get("user_league_key", envir = the)
  } else {
    stop("No league key currently saved. Please input one using 'set_league_key(key)'.")
  }
}

# Allow manual setting of a key
set_league_key <- function(key) {
  assign("user_league_key", key, envir = the)
  invisible(TRUE)
}


# Helpers ------------------------------------------------------------
# helper function to pull matchup team meta data (minus player scores)
extract_matchup_teams <- function(matchup) {
  get_team_info <- function(team) {
    list(
      team_key = find_variable_in_nested_list(team, "team_key"),
      team_id = find_variable_in_nested_list(team, "team_id"),
      name = find_variable_in_nested_list(team, "name"),
      manager = find_variable_in_nested_list(team, "nickname"),
      score = team[[2]]$team_points$total,
      projected_score = team[[2]]$team_projected_points$total,
      win_probability = team[[2]]$win_probability
    )
  }

  home_team <- get_team_info(matchup[["0"]]$team)
  away_team <- get_team_info(matchup[["1"]]$team)

  home_team <- as.data.frame(home_team)
  away_team <- as.data.frame(away_team)

  df <- yahoofantasyr::bindRows(home_team, away_team)

  return(df)
}
