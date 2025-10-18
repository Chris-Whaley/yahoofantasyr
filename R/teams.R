yf_team <- function(team_key) {
  result <- yahoofantasyr::yf_get(paste0("team/", team_key))
  data <- result$fantasy_content$team[[1]]

  vars <- c(
    "team_key", "team_id", "name", "nickname",
    "previous_season_team_rank", "faab_balance", "number_of_moves",
    "number_of_trades", "auction_budget_total", "auction_budget_spent"
  )

  df <- map(vars, ~ find_variable_in_nested_list(data, .x)) %>%
    set_names(vars) |>
    tibble::as.tibble()

  return(df)
}


yf_team_roster <- function(team_key, week = NULL) {
  path <- paste0("team/", team_key, "/roster")
  if (!is.null(week)) path <- paste0(path, ";week=", week)
  result <- yf_get(path)
  data <- result$fantasy_content$team[[2]]$roster$`0`$players

  number_of_players <- data[[2]]$roster$`0`$players$count

  temp_list <- list()

  for (player in 1:number_of_players) {
    input_player <- data[[player]]$player[[1]][[1]]$player_key
    temp_list[[player]] <- yahoofantasyr::yf_player(input_player)
  }

  df <- yahoofantasyr::bindRows(map(temp_list, as.tibble))

  return(df)
}


yf_team_matchups <- function(team_key, week = NULL) {
  path <- paste0("team/", team_key, "/matchups")
  result <- yahoofantasyr::yf_get(path)
  data <- result$fantasy_content$team

  # Pull and clean to get week numbers
  weeks <- names(data[[2]]$matchups)
  weeks <- weeks[weeks != "count"]
  weeks <- as.numeric(weeks) + 1 # week 1 equals "0" in original vector
  if (!is.null(week)) weeks <- weeks[weeks == week]


  vars_team <- c("team_key", "name", "nickname")


  # [[1]]: metadata for team_key
  team_meta <- yahoofantasyr::yf_team(team_key)

  # [[2]]$matchups: each week's matchup
  # [[2]]$matchups[[1]]$matchup
  temp_df_meta <- map(vars_meta, ~ find_variable_in_nested_list(data[[2]]$matchups[[1]]$matchup, .x)) %>%
    set_names(vars_meta) |>
    tibble::as.tibble()

  all_weeks <- data[[2]]$matchups
  all_matchups <- list()

  ## OUTER LOOP: go through each week
  for (week in weeks) {
    ## INNER LOOP: each team in a matchup for a week
    scores_df <- all_weeks[[week]]$matchup[["0"]]$teams
    scores_list <- list()
    for (team in names(scores_df)) {
      if (team == "count") {} else {
        temp_df_week <- map(vars_team, ~ find_variable_in_nested_list(scores_df[[team]][[1]], .x)) %>%
          set_names(vars_team) |>
          tibble::as.tibble()

        temp_df_week$week <- week
        temp_df_week$team_points <- find_lists_with_name(scores_df[[team]], "team_points")$total
        temp_df_week$team_projected_points <- find_lists_with_name(scores_df[[team]], "team_projected_points")$total
        temp_df_week$win_probability <- find_variable_in_nested_list(scores_df[[team]], "win_probability")

        # put each team into an element in a list
        scores_list[[team]] <- temp_df_week
      }
    }
    # convert each week's matchup into a dataframe. add to master list of all weeks
    # TODO one row per matchup, distinguish column names for each team
    weekly_scores_df <- yahoofantasyr::bindRows(scores_list)
    all_matchups[[week]] <- weekly_scores_df
  }

  all <- yahoofantasyr::bindRows(all_matchups)
  all <- all |>
    dplyr::relocate(week)

  return(all)
}
