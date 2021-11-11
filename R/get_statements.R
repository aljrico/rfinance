#' Get Statements
#' @name get_statements
#' @description This function checks uses an existing connection handler to retrieve financial statements from the database.
#' @return Returns a data.table with the financial statements
#' @param ticker String with the ticker of the company.
#'
#' @export
#'
get_statements <- function(ticker){
  check_credentials()
  rfinanceConnection$get_statements(ticker)
}