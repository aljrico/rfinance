#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#' @param from Minimum date to get data from
#' @param to Maximum date to get data of
#' @param split_adjust Boolean deciding if data should adjusted with splits
#'
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' dividends <- get_dividends('MSFT')
#' @rdname get_dividends
#' @export
#'
#'
#'
get_dividends <-
  function(symbol, from = "1970-01-01", to = Sys.Date(), split_adjust = TRUE) {
    handle <- get_handle()
    yahoo_url <- build_yahoo_url(symbol = symbol, from = date_to_unix(from), to = date_to_unix(to), period = '1d', type = "div", handle = handle)

    dividends <- readr::read_csv(curl::curl(yahoo_url, handle = handle$session), col_types = readr::cols())
    colnames(dividends) <- c('date', 'dividends')
    closeAllConnections()

    # Adjust for splits
    if (split_adjust) dividends <- adjust_for_splits(dividends, symbol)
    
    dividends$name <- symbol

    dplyr::select(dividends, date, name, dividends)
  }
