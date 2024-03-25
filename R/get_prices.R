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
  .d <- `[`
  remove_symbol_from_colname = function(data) {
    for(col in names(data)){
      new_col = col |> 
        stringr::str_remove(symbol) |> 
        stringr::str_remove("_")
      
      data.table::setnames(data, old = col, new = new_col)
    }
    return(data)
  }
  
  output <- lapply(symbol, function(symbol){
    upper_symbol = toupper(symbol)
    data_env = new.env()
    quantmod::getSymbols(upper_symbol, env = data_env)
    
    data_env[[upper_symbol]] |> 
      data.table::as.data.table() |> 
      janitor::clean_names() |> 
      remove_symbol_from_colname() |> 
      .d(, ticker := upper_symbol)
  }) |> 
    data.table::rbindlist(fill = TRUE)
  
  return(output)
}
