the <- new.env(parent = emptyenv())
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to yahoofantasyr! Checking authentication...")
  # internal state for storing user data


  # Example: Automatically check refresh token
  token <- yahoofantasyr::refresh_token()
  if (!is.na(token)) {
    packageStartupMessage("✔ Yahoo token loaded successfully.")
  } else {
    packageStartupMessage("⚠ No Yahoo token found. Run `yf_auth()` to authenticate.")
  }

  # Capture League Key. Users League > Any League > Error
  # TODO: use start_date and end_date to classify preseason, active, offseason
  all_leagues <- yahoofantasyr::yf_user_leagues()
  the$all_leagues <- all_leagues |>
    dplyr::select(league_key, name, game_code, season)

  # set up metadata for api functions (e.g., default columns found in all leagues)
  the$team_meta <- load_sport_meta()$default
  the$team_columns <- the$team_meta$team_columns

  df_text <- paste(utils::capture.output(print(data.frame(the$all_leagues), row.names = FALSE)), collapse = "\n")
  packageStartupMessage(
    cli::rule(center = "Welcome to yahoofantasyr"),
    "\n",
    cli::col_grey("Available fantasy leagues:\n"),
    df_text
  )

  # Ask user for league key
  entering_league <- get_league_choice()

  if (entering_league) {
    user_league_key <- input_league_key()
  } else {
    user_league_key <- NULL
  }

  # is it one of their leagues? if not, is it a query-able league? if not, we can't find the league
  if (is.null(user_league_key)) {
    packageStartupMessage(paste(
      "League Key not entered. Please unattach and reattach package to try again,",
      "or manually enter a league_key for functions requiring one."
    ))
    return()
  } else if (is_user_league(user_league_key) | is_available_league(user_league_key)) {
    set_league_key(user_league_key)
    # assign("user_league_key", user_league_key, envir = the)
    # user_league <- yahoofantasyr::yf_league(user_league_key) |>
    #   dplyr::select(league_key, name, game_code, season)
    # user_league_name <- user_league$name
    # user_league_game_code <- user_league$game_code
    # user_league_season <- user_league$season
    # assign("user_league_name", user_league_name, envir = the)
    # assign("user_league_game_code", user_league_game_code, envir = the)
    # assign("user_league_season", user_league_season, envir = the)
    # packageStartupMessage(paste("You have entered the league key for", get("user_league_name", envir = the)))
  } else {
    packageStartupMessage("League not found. Please unattach and reattach package to try again.")
  }





}


# Helpers ------------------------------------------------------------
get_league_choice <- function(prompt = "Do you wish to enter your league? (Yes/No)", max_attempts = 2) {
  for (i in seq_len(max_attempts)) {
    choice <- trimws(tolower(readline(prompt)))
    if (choice == "yes") {
      return(TRUE)
    } else if (choice == "no") {
      return(FALSE)
    }
  }
  return(FALSE)
}

input_league_key <- function(prompt = "Enter your Yahoo 'league_key': ", max_attempts = 2) {
  for (i in seq_len(max_attempts)) {
    league <- trimws(readline(prompt))
    if (nzchar(league)) {
      packageStartupMessage("League ID entered: ", league)
      return(league)
    }
    packageStartupMessage("No input detected. Try again.")
  }
  packageStartupMessage("No league ID entered.")
  packageStartupMessage(paste(
    "Please note that you will need to manually enter a league key for",
    "functions requiring a 'league_key'."
  ))
  return(NULL)
}

is_user_league <- function(input_league_key) {
  user_league_name <- the$all_leagues |>
    dplyr::filter(league_key == input_league_key) |>
    dplyr::pull(name)

  is_users <- dplyr::if_else(length(user_league_name) > 0,
    TRUE,
    FALSE
  )
  return(is_users)
}

is_available_league <- function(input_league_key) {
  result <- tryCatch(
    {
      yahoofantasyr::yf_league(input_league_key)
      TRUE # success → return TRUE
    },
    error = function(e) {
      FALSE # failure → return FALSE
    }
  )
  return(result)
}


#' Internal: Load Sport Metadata from YAML
#' @keywords internal
load_sport_meta <- function() {
  yaml::read_yaml(fs::path_package("tools", "sports.yml", package = "yahoofantasyr"))
}


#' Internal: Retrieve sport metadata for a league key
#' @keywords internal
# get_sport_meta <- function(game_code) {
#   meta <- load_sport_meta()[[game_code]]
#   if (is.null(meta)) stop("Unsupported or unknown sport: ", sport)
#   return(meta)
# }

setup_league_meta <- function(league_key, settings_type) {
  if (missing(league_key)) {
    league_key <- get_league_key()
  }

  overview <- yf_league_settings(league_key, settings_type = "overview")
  is_auction <- overview |>
    dplyr::filter(setting == "is_auction_draft") |>
    dplyr::summarise(value = ifelse(as.numeric(value) == 1, TRUE, FALSE)) |>
    dplyr::pull(value)

  is_faab <- overview |>
    dplyr::filter(setting == "uses_faab") |>
    dplyr::summarise(value = ifelse(as.numeric(value) == 1, TRUE, FALSE)) |>
    dplyr::pull(value)

  is_rolling_waivers <- overview |>
    dplyr::filter(setting == "waiver_type") |>
    dplyr::summarise(value = ifelse(value == "R", TRUE, FALSE)) |>
    dplyr::pull(value)

  return_settings_type <- switch(settings_type,
    "auction" = is_auction,
    "faab" = is_faab,
    "rolling_waivers" = is_rolling_waivers
  )

  return(return_settings_type)
}


# Allow manual setting of a key
set_league_key <- function(league_key) {

  invisible(TRUE)

  user_league <- yahoofantasyr::yf_league(league_key) |>
    dplyr::select(league_key, name, game_code, season)
  # user_league_name <- user_league$name
  # user_league_game_code <- user_league$game_code
  # user_league_season <- user_league$season
  assign("user_league_key", league_key, envir = the)
  # assign("user_league_name", user_league_name, envir = the)
  # assign("user_league_game_code", user_league_game_code, envir = the)
  # assign("user_league_season", user_league_season, envir = the)
  assign("user_league_name", user_league$name, envir = the)
  assign("user_league_game_code", user_league$game_code, envir = the)
  assign("user_league_season", user_league$season, envir = the)
  packageStartupMessage(paste("You have entered the league key for", get("user_league_name", envir = the)))

  # is it an auction draft
  if (setup_league_meta(the$user_league_key, "auction")) {
    auction_columns <- load_sport_meta()$auction
    the$team_columns <- c(the$team_meta$team_columns, auction_columns$auction_team_columns)
  }

  # does the league use faab?
  if (setup_league_meta(the$user_league_key, "faab")) {
    faab_columns <- load_sport_meta()$faab
    the$team_columns <- c(the$team_columns, faab_columns$faab_team_columns)
  }
}
