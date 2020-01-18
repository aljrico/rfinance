#' Get all available symbols
#' 
#' @name get_symbols_list
#' 
#' @param index Includes only symbols that appear in determined indexes ('sp500'). Deafults to 'any'. 
#' @return Returns an array with all symbols available in the API.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' symbols <- get_symbols_list()
#' @rdname get_symbols_list
#' @export
#'
#'
#'
get_symbols_list <- function(index = 'any') {
  url <- glue::glue("https://financialmodelingprep.com/api/v3/company/stock/list")
  content <- url %>%
    httr::GET() %>%
    httr::content()

  symbols_list <- content$symbolsList

  symbols_list <- sapply(symbols_list, FUN = function(x) x$symbol)
  
  
  # Only include the ones appearing in the SP500 index
  if (index == "sp500") {
    sp500_wiki <- xml2::read_html(
      "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    )
    
    symbols_table <- sp500_wiki %>%
      rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
      rvest::html_table()
    
    symbols_table <- symbols_table[[1]]
    tickers <- as.character(symbols_table$`Symbol`)
    
    symbols_list <- intersect(symbols_list, tickers)
  }
  
  closeAllConnections()
  return(symbols_list)
}
