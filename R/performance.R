#' Provided a returns time series, it will return Jensen's Alpha
#'
#' @param Ra A time series of asset returns
#' @param Rb A time series of benchmark returns
#' @param Rf A time series of risk free rate
#' @export
calculate_alpha <- function(Ra, Rb, Rf = 0){
  PerformanceAnalytics::CAPM.jensenAlpha(
    Ra,
    Rb,
    Rf
  )
}

#' Provided a returns time series, it will return Sharpe Ratio
#'
#' @param R A time series of asset returns
#' @param Rf A time series of risk free rate
#' @export
calculate_sharpe_ratio <- function(R, Rf = 0){
  PerformanceAnalytics::SharpeRatio(
    R,
    Rf
  )
}


geometric_mean <- function(x, na.rm = FALSE) exp(mean(log(x), na.rm = na.rm))
average_return <- function(x, na.rm = FALSE){
  geometric_mean(1 + x, na.rm = na.rm)
}
accumulated_return <- function(x, na.rm = FALSE){
  average_return(x, na.rm = na.rm) ^ length(x)
}