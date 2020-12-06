#' Log In
#' @name log_in
#' @description This function checks user credentials, opens a connection with the database and stores its handler in the global environment.
#' @return Does not return anything
#' @param username String containing the username.
#' @param password String containing the password.
#' @param environment Leave blank. Which environment should the connection object be placed on?
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @rdname log_in
#' @export
#'
log_in <- function(username, password, environment = 1){
  rfinanceConnection <- ConnectionHandler$new(username, password)
  assign("rfinanceConnection", rfinanceConnection, envir = as.environment(environment))
}

#' Log Out
#' @name log_out
#' @description This function looks for the connection handler, removes it and closes all connections.
#' @return Does not return anything
#' @param environment Leave blank. Which environment should the connection object be placed on?
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.com}{Personal Website}
#'
#' @rdname log_out
#' @export
#'
log_out <- function(environment = 1){
  if(exists("rfinanceConnection")){
    rm("rfinanceConnection", envir = as.environment(environment))
  }
}