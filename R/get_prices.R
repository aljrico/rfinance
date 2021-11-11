#' Download historic prices of a given company
#' @name get_prices
#' @description This function retrieves all available historic prices of a given company. This company is specified using the `symbol` argument. All available symbols can be found using `get_symbols_list()`
#' @return Returns a tibble.
#' @param symbol String that specifies the ticker/symbol of the company we want to download its prices.
#' @param from Minimum date to get data from.
#' @param to Maximum date to get data of.
#'
#' @examples
#'
#' prices <- get_prices('MSFT', from = '2010-04-05', to = '2011-03-02')
#' @rdname get_prices
#' @export
#'
#'
#'
get_prices <- function(symbol, from = '1970-01-01', to = Sys.Date()){
  handle <- get_handle()
  output <- lapply(symbol, function(symbol){
    yahoo_url <- build_yahoo_url(symbol = symbol, from = date_to_unix(from), to = date_to_unix(to), interval = '1d', event = 'history', handle = handle)
    prices <- data.table::data.table()
    
    tryCatch({
      suppressMessages({
        curl_connection <- curl::curl(yahoo_url, handle = handle$session)
        prices <- utils::read.csv(curl_connection) %>% 
          janitor::clean_names() %>% 
          tibble::as_tibble()
      })
      prices$name <- symbol
    }, 
    error = function(e){
      warning(glue::glue("Prices for {symbol} were not found."))
    })
    
    closeAllConnections()
    return(prices)
  })
  
  names(output) <- symbol
  output <- data.table::rbindlist(output, fill = TRUE)
  return(as.data.frame(output))
}
