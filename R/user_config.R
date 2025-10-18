.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to yahooFantasyR! Checking authentication...")
  # internal state for storing user data
  the <- new.env(parent = emptyenv())

  # Example: Automatically check refresh token
  token <- yahoofantasyr::refresh_token()
  if (!is.na(token)) {
    # token <- readRDS(token_file)
    # options(yahoo_token = token)
    packageStartupMessage("✔ Yahoo token loaded successfully.")
  } else {
    packageStartupMessage("⚠ No Yahoo token found. Run `yf_auth()` to authenticate.")
  }

  # Capture the user's league key
  packageStartupMessage("Printing your leagues to console")
  all_leagues <- yahoofantasyr::yf_user_leagues()
  all_leagues <- all_leagues |>
    dplyr::select(league_key, name, game_code, season)
  print(all_leagues)
  # user_league_key <- readline(prompt = "Enter  for your analysis: ")
  # user_league_key <- trimws(user_league_key)

  get_league_key <- function(prompt = "Enter your Yahoo 'league_key': ", max_attempts = 3) {
    for (i in seq_len(max_attempts)) {
      league <- trimws(readline(prompt))
      if (nzchar(league)) {
        message("✅ League ID entered: ", league)
        return(league)
      }
      message("⚠️  No input detected. Attempt ", i, " of ", max_attempts, ".")
    }
    message("❌ No league ID entered after ", max_attempts, " attempts.")
    message(paste("Please note that you will need to manually enter a league key for",
                   "functions requiring a 'league_key'." ))
    return(NULL)
  }

  user_league_key <- get_league_key()

  # save to internal state if populated
  if (!is.null(user_league_key)) {
    the$user_league_key <- user_league_key
    the$user_league_name <- all_leagues |>
      dplyr::filter(league_key == the$user_league_key) |>
      dplyr::pull(name)

    message(paste("Using league:", the$user_league_name))
  }


}
