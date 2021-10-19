#' Get Statements
#' @name get_statements
#' @description This function checks uses an existing connection handler to retrieve financial statements from the database.
#' @return Returns a data.table with the financial statements
#' @param ticker String with the ticker of the company.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @export
#'
get_statements <- function(ticker){
  check_credentials()
  rfinanceConnection$get_statements(ticker)
}