.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to yahooFantasyR! Checking authentication...")
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
  user_league_key <- readline(prompt = "Enter 'league_key' for your analysis: ")
  user_league_key <- trimws(user_league_key)
  # save to internal state
  the$user_league_key <- user_league_key
  the$user_league_name <- all_leagues |>
    dplyr::filter(league_key == the$user_league_key) |>
    dplyr::pull(name)

  packageStartupMessage(paste("Using league:", the$user_league_name))
}
