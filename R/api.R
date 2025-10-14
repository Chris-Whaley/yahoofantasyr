#' GET Yahoo Fantasy API endpoints
#'
#' @param endpoint API endpoint url to access resources and subresources
#'
#' @return List from endpoint response
#' @export
#'
#' @examples
#' #fantasy_football_seasons <- yf_get("games;game_codes=nfl")
yf_get <- function(endpoint) {
  access_token <- yahoofantasyr::refresh_token()

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
