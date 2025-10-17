test_that("game key retrieval works", {
  expect_no_error(yf_gamekeys())
})

test_that("game key retrieval works", {
  expect_no_error(yf_game_from_key("461"))
})
