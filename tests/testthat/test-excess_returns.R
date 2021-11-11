testthat::test_that("excess_returns fails when expected", {
  testthat::expect_error(
    excess_returns(r = 1, rf = c(1,2))
  )
})

testthat::test_that("excess_returns works when expected", {
  # Process single rf case
  testthat::expect_is(
    excess_returns(r = rnorm(100), rf = rnorm(1)),
    'numeric'
  )
  
  # Process same-length case
  testthat::expect_is(
    excess_returns(r = rnorm(100), rf = rnorm(100)),
    'numeric'
  )
  
  # Process index date and single rf case
  testthat::expect_is(
    excess_returns(r = data.frame(r = rnorm(1), row.names = '2019-01-01'), rf = rnorm(1)),
    'data.frame'
  )
  
  # Process r and rf with index date
  r = rnorm(1)
  rf = rnorm(1)
  result = excess_returns(
    r = data.frame(r = r, row.names = '2019-01-01'), 
    rf = data.frame(rf = rf, row.names = "2019-01-01")
  )
  testthat::expect_is(result, 'data.frame')
  testthat::expect_equal(r - rf, result[[1]])
})
