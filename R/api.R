refresh_token <- function() {
  token_path <- file.path(tempdir(), "refreshed_token.rds")

  # use the initializing token for the first use. afterwards use refresh token
  if (!file.exists(token_path)) {
    return_token <- token
  } else {
    return_token <- readRDS(token_path)
    return_token$expires_at <- as.POSIXct(return_token$expires_at, origin = "1970-01-01")
  }

  # Refresh if expired
  if (Sys.time() > return_token$expires_at) {
    refreshed_token <- httr2::oauth_flow_refresh(
      client,
      refresh_token = return_token$refresh_token
    )

    saveRDS(refreshed_token, token_path)
    return_token <- refreshed_token
  }

  return(return_token$access_token)
}


#' GET Yahoo Fantasy API endpoints
#'
#' @param endpoint API endpoint url to access resources and subresources
#'
#' @return List from endpoint response
#' @export
#'
#' @examples
#' # fantasy_football_seasons <- yf_get("games;game_codes=nfl")
yf_get <- function(endpoint) {
  access_token <- refresh_token()

  url <- paste0("https://fantasysports.yahooapis.com/fantasy/v2/", endpoint, "?format=json")

  # Build request
  req <- httr2::request(url) |>
    httr2::req_headers(Authorization = paste("Bearer", access_token))

  # Perform request
  resp <- httr2::req_perform(req)

  # Check for unauthorized
  if (httr2::resp_status(resp) == 401) stop("Invalid or expired token.")

  # Parse JSON
  httr2::resp_body_json(resp)
}
