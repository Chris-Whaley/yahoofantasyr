#' Refresh a locally stored token
#'
#' @return a refreshed access token
#' @export
#'
refresh_token <- function() {
  # if (!file.exists(token_file)) {
  #   initialize_token()
  # }
  # token <- readRDS(token_file)
  # data("sysdata.rda")

  # Refresh if expired
  if (Sys.time() > token$expires_at) {
    refreshed_token <- httr2::oauth_flow_refresh(
      client,
      refresh_token = token$refresh_token
    )

    # Save token for reuse
    # saveRDS(refreshed_token, token_file)
    usethis::use_data(refreshed_token, overwrite = TRUE)
    token <- refreshed_token
  }

  token$access_token
}
