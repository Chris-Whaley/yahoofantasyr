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
  # packageStartupMessage("Printing your leagues to console")
  # TODO: use start_date and end_date to classify preseason, active, offseason
  all_leagues <- yahoofantasyr::yf_user_leagues()
  the$all_leagues <- all_leagues |>
    dplyr::select(league_key, name, game_code, season)
  # print(the$all_leagues)
  df_text <- paste(capture.output(print(data.frame(the$all_leagues), row.names = FALSE)), collapse = "\n")
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
  } else if (is_user_league(user_league_key) | is_available_league(user_league_key)) {
    assign("user_league_key", user_league_key, envir = the)
    user_league_name <- yahoofantasyr::yf_league(user_league_key) |>
      dplyr::select(league_key, name, game_code, season) |>
      dplyr::pull(name)
    assign("user_league_name", user_league_name, envir = the)
    packageStartupMessage(paste("You have entered the league key for", get("user_league_name", envir = the)))
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

