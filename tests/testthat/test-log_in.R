test_that("log_in creates a connection instance", {
  rfinance::log_in(NULL, NULL)
  testthat::expect_true(exists("rfinanceConnection"))
})
