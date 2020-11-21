test_that("download_statements outputs a data.table", {
  msft_dt <- rfinance::get_statements('MSFT')
  testthat::expect_that(msft_dt, testthat::is_a("data.table"))
})
