#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#' @param from Minimum date to get data from
#' @param to Maximum date to get data of
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' splits <- get_splits('MSFT')
#' @rdname get_splits
#' @export
#' 
get_splits <-
  function(symbol, from = '1970-01-01', to = Sys.Date()){
    
    handle <- get_handle()
    yahoo_url <- build_yahoo_url(symbol = symbol, from = date_to_unix(from), to = date_to_unix(to), period = '1d', type = 'split', handle = handle)
    
    splits <- readr::read_csv(curl::curl(yahoo_url,handle = handle$session), col_types = readr::cols())
    closeAllConnections()
    
    if(nrow(splits)==0) {
      splits <- NA
    } else {
      
      numeric_ratio <- function(x){
        this_ratio <- strsplit(x, ':') %>% unlist() %>% as.numeric()
        this_ratio[[1]] / this_ratio[[2]]
      }
      splits$stock_splits <- sapply(splits[['Stock Splits']], numeric_ratio)
      splits <- xts::xts(splits$stock_splits, as.Date(splits[['Date']], "%Y-%m-%d"))
      colnames(splits) <- paste(symbol,'split',sep='_')
    }
    return(splits)
  }
