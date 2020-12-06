test_that("get_tickers returns something", {
  tickers <- rfinance::get_tickers()
  
  testthat::expect_true(is.character(tickers))
  testthat::expect_true(length(tickers) > 0)
})
