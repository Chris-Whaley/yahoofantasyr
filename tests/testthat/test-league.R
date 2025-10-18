test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("multiplication works", {
  expect_type(yf_league_settings(league_key, "roster"), "dataframe")
})
