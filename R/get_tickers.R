#' Download list of tickers from a given public index
#' @name get_tickers
#' @description This function returns the list of available tickers in the data base
#' @return Returns a vector.
#'
#'
#' @examples
#'
#' tickers <- rfinance::get_tickers()
#' @rdname get_tickers
#' @export
#'
get_tickers <- function() {
  if(!exists("rfinanceConnection")){
    log_in(NULL, NULL)
  }
  
  rfinanceConnection$get_tickers()
}
