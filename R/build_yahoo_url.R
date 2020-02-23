#'
#' @description This function defines the standard Yahoo URL for the API call
#' @param symbol Refers to the ticker of the company
#' @param from Initial date
#' @param to Final date
#' @param interval Specifies the time frame used in the data. Can be '1d', '1wk' or '1mo'
#' @param event What type of event is being recorded. Can be 'div' for dividends, 'history' for price history or 'split'. 
#' @internal
build_yahoo_url <- function(symbol, from, to, interval = '1d', event, handle){
  # interval <- match.arg(period, c("1d", "1wk", "1mo"))
  # event <- match.arg(type, c("history", "div", "split"))
  n <- sample(c(1,2), 1)
  url <- glue::glue("https://query{n}.finance.yahoo.com/v7/finance/download/{symbol}?period1={from}&period2={to}&interval={interval}&events={event}&crumb={handle$content}")
  return(url)
}