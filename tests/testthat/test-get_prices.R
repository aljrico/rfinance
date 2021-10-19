test_that("get_prices works", {
  ticker <- sample(rfinance::get_sp500(), 1) # Get a random ticker
  prices <- rfinance::get_prices(ticker)
  testthat::expect_that(prices, testthat::is_a("data.frame"))
})
