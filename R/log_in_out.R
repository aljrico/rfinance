#' Log In
#' @name log_in
#' @description This function checks user credentials, opens a connection with the database and stores its handler in the global environment.
#' @return Does not return anything
#' @param username String containing the username.
#' @param password String containing the password.
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @rdname log_in
#' @export
#'
log_in <- function(username = 'consumer-1', password = 'HWjZKZcWWkYnU8E8WZaxnFgoAZU8cn'){
  rfinance_connection_handler <- ConnectionHandler$new(username, password)
  assign("rfinance_connection_handler", rfinance_connection_handler, envir = globalenv())
}

#' Log Out
#' @name log_out
#' @description This function looks for the connection handler, removes it and closes all connections.
#' @return Does not return anything
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @rdname log_out
#' @export
#'
log_out <- function(){
  if(exists("rfinance_connection_handler")){
    rm(rfinance_connection_handler, envir = globalenv())
  }
}