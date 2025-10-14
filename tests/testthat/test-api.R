test_that("Can retrieve nfl game codes for each season along with associated metadata", {
  expect_no_error(yf_get("games;game_codes=nfl"))
})
