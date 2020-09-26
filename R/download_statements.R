#' Download Statements
#' @name download_statements
#' @description This function checks uses an existing connection handler to retrieve financial statements from the database.
#' @return Returns a data.table with the financial statements
#' @param ticker String with the ticker of the company.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @example download_statements('MSFT')
#' @rdname download_statements
#' @export
#'
download_statements <- function(ticker){
  if(exists("rfinance_connection_handler")){
    return(rfinance_connection_handler$get_statements(ticker))
  }else{
    stop("You need to log in first.")
  }
}