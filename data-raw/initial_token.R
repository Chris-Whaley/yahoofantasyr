client_id <- "dj0yJmk9dnFSQUx1UWF0VU9uJmQ9WVdrOVdYZ3hVbTF1VGprbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PTkx"
client_secret <- "efbc0114249caeba96ff6ec8e64738de027caec4"

# 1: Create an Application Client
client <- httr2::oauth_client(
  id = client_id,
  secret = client_secret,
  token_url = "https://api.login.yahoo.com/oauth2/get_token",
  name = "test"
)


# Start interactive OAuth flow
auth_url <- httr2::oauth_flow_auth_code_url(
  client,
  auth_url = "https://api.login.yahoo.com/oauth2/request_auth",
  scope = "fspt-r",
  redirect_uri = "oob"
)

# 2: Yahoo shows a code — copy/paste it here
utils::browseURL(auth_url)
auth_code <- readline(prompt = "Enter the Yahoo authorization code: ")
auth_code <- trimws(auth_code)
if (nchar(auth_code) == 0) stop("No authorization code provided.")

# 3: Exchange code for access token
# exchange code for tokens (manual POST)
token_req <- httr2::request("https://api.login.yahoo.com/oauth2/get_token") |>
  httr2::req_auth_basic(client_id, client_secret) |>
  httr2::req_body_form(
    grant_type   = "authorization_code",
    code         = auth_code,
    redirect_uri = "oob"
  )

token_resp <- httr2::req_perform(token_req)
if (httr2::resp_status(token_resp) >= 400) {
  stop("Token exchange failed: ", httr2::resp_status(token_resp), "\n", httr2::resp_body_string(token_resp))
}


# 4: Create initial token (initial user input needed, which I'll do)
token <- httr2::resp_body_json(token_resp, simplifyVector = TRUE)
token$expires_at <- Sys.time() + as.numeric(token$expires_in)


# 5: Refresh the token so we can only expose the refreshable token (no user input needed)
refreshable_token <- httr2::oauth_flow_refresh(
  client,
  refresh_token = token$refresh_token
)
refreshable_token$expires_at <- as.POSIXct(refresh_token$expires_at, origin = "1970-01-01")

# 6: Save token internally so it can be refreshed when needed by loading in the refresh_token function
usethis::use_data(refreshable_token, client, internal = TRUE, overwrite = TRUE)
