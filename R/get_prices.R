#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#' @param from Minimum date to get data from
#' @param to Maximum date to get data of
#'
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' prices <- get_prices('MSFT')
#' @rdname get_prices
#' @export
#'
#'
#'
get_prices <- function(symbol, from = '1970-01-01', to = '2030-01-01'){
  handle <- get_handle()
  
  output <- lapply(symbol, function(symbol){
    yahoo_url <- build_yahoo_url(symbol = symbol, from = date_to_unix(from), to = date_to_unix(to), period = '1d', type = 'history', handle = handle)
    prices <- readr::read_csv(curl::curl(yahoo_url, handle = handle$session), col_types = readr::cols())
    closeAllConnections()
    prices$name <- symbol
    return(prices)
  })
  
  names(output) <- symbol
  output <- dplyr::bind_rows(output)
  return(output) 
}

get_prices(c('MSFT'))
