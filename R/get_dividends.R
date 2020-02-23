#' Dividend payments history
#'
#' @description This function downloads all available dividend payments history of a given company.
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
  function(symbol, from = "1950-01-01", to = Sys.Date(), split_adjust = FALSE) {
    
    df_list <- list()
    pb <- progress::progress_bar$new(format = "Downloading Dividends [:bar] :percent eta: :eta", total = length(symbol), clear = TRUE)
    
    for(s in symbol){
      pb$tick()
      handle <- get_handle()
      yahoo_url <- build_yahoo_url(symbol = s, from = date_to_unix(from), to = date_to_unix(to), interval = '1d', event = "div", handle = handle)
      dividends <- NULL
      try(dividends <- readr::read_csv(curl::curl(yahoo_url, handle = handle$session), col_types = readr::cols()), silent = TRUE)
      if(is.null(dividends)){
        warning(glue::glue("... \n We didn't find any {symbol}. We'll skip this one"))
        next
      }
      
      colnames(dividends) <- c('date', 'dividends')
      closeAllConnections()
      
      
      # What to do if we can't find dividends for that company
      if(nrow(dividends) == 0){
        warning(glue::glue("No dividends found for '{s}'. It will be skipped"), call. = FALSE)
        next
      }
      
      # Adjust for splits
      if (split_adjust) dividends <- adjust_for_splits(dividends, s)
      
      dividends$name <- s
      
      df_list[[s]] <- 
        dividends %>% 
        dplyr::select(date, name, dividends) %>% 
        dplyr::arrange(date)
    }

    dplyr::bind_rows(df_list)
  }
