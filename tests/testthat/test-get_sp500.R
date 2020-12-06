test_that("get_sp500 returns something", {

  tickers <- rfinance::get_sp500()
  testthat::expect_true(is.character(tickers))
  testthat::expect_true(length(tickers) > 0)
})
