#' Refresh a locally stored token
#'
#' @return a refreshed access token
#' @export
#'
refresh_token <- function() {
  token_path <- file.path(tempdir(), "refreshed_token.rds")

  # if (!file.exists(".//data//refreshed_token.rds")) {
  if (!file.exists(token_path)) {
    return_token <- token
  } else {
    # load(".//data//refreshed_token.rda")
    # return_token <- readRDS(".//data//refreshed_token.rds")
    return_token <- readRDS(token_path)
    # return_token <- refreshed_token
    # return_token$expires_at <- as.POSIXct(refreshed_token$expires_at, origin = "1970-01-01")
    return_token$expires_at <- as.POSIXct(return_token$expires_at, origin = "1970-01-01")
  }

  # Refresh if expired
  if (Sys.time() > return_token$expires_at) {
    refreshed_token <- httr2::oauth_flow_refresh(
      client,
      refresh_token = return_token$refresh_token
    )

    # Save token for reuse
    # saveRDS(refreshed_token, ".//data//refreshed_token.rds")
    saveRDS(refreshed_token, token_path)
    # usethis::use_data(refreshed_token, overwrite = TRUE)
    return_token <- refreshed_token
  }

  return_token$access_token
}
