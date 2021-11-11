#' Check Credentials
#' @name check_credentials
#' @description This function will check whether a user has logged in. In case it hasn't, it will default to the 'rfinance' user
#' @return NULL
#'
check_credentials <- function(){
  if(!exists("rfinanceConnection")){
    cli::cli_alert_warning("No credentials found. Defaulting to tester.")
    cli::cli_alert_warning("To get rid of all limitations and enjoy the full 'rfinance' experience, register at {.url www.weirwood.ai}")
    log_in("rfinance", "rfinance")
  }
}