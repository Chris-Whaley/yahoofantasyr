test_that("game key retrieval works", {
  expect_no_error(yf_gamekeys())
})


test_that("game key returns correct season only", {
  expect_equal(yf_game_from_key("461")$season, "2025")
})


test_that("season year returns correct game key", {
  expect_equal(yf_game_from_year(2025)$game_key, "461")
})
