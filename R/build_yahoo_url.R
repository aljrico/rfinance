build_yahoo_url <- function(symbol, from, to, period, type, handle){
  interval <- match.arg(period, c("1d", "1wk", "1mo"))
  event <- match.arg(type, c("history", "div", "split"))
  n <- if (stats::runif(1, 0, 1) >= 0.5) 1L else 2L
  url <- glue::glue("https://query{n}.finance.yahoo.com/v7/finance/download/{symbol}?period1={from}&period2={to}&interval={interval}&events={event}&crumb={handle$content}")
  return(url)
}