#' This is a ConnectionHandler class
#' @title ConnectionHandler class
#' @docType class
#' @description This class creates objects to handle connections to the database
#' @export
#'
ConnectionHandler <- R6::R6Class(
  "ConnectionHandler",
  private = list(
    premium = NULL,
    token = NULL,
    log = function(msg) {
      full_msg <- paste0("  rfinance: ", msg)
      cli::cli_alert_info(msg)
    },
    credentials = list(
      username = "rfinance",
      password = "rfinance",
      token = NULL
    ),
    content_type = "application/x-www-form-urlencoded",
    base_url = "https://finten.weirwood.ai",
    read_credentials = function(username, password){
      
      # Check if any credentials have been provided
      if(is.null(username) | is.null(password)){
        private$log("No credentials provided. The default user will be used")
      }else{
        private$credentials$username = username
        private$credentials$password = password
      }
      
      # Send credentials to the database
      response <- httr::POST(
        url = glue::glue(private$base_url, "/users/login"), 
        body = glue::glue("username={private$credentials$username}&password={private$credentials$password}"), 
        httr::content_type(private$content_type), 
        encode = "json"
      )
      
      # Parse response
      response_content <- httr::content(response)
      
      # Check if the response contains any error
      if(any(names(response_content) == "error")){
        error_message <- glue::glue("{response_content$error}")
        stop(error_message)
      }else{
        
        # If the response is correct, we can 
        # save up the token
        private$credentials$token = httr::content(response)$token
        private$token = private$credentials$token
        private$premium = httr::content(response)$isPremium
      }
      
    },
    manage_errors = function(response){
      if(response$status_code == 401){
        stop("Invalid credentials.")
      }else if(response$status_code == 403){
        warning(glue::glue("The ticker {ticker} is not available."))
        return(NULL)
      }
    }
  ),
  public = list(
    
    #' @param username username
    #' @param password password
    #'
    initialize = function(username = NULL, password = NULL) {
      private$log("Checking credentials...")
      private$read_credentials(username, password)
      },
    
    #' Retrieves statements
    #'
    #' @param ticker Company symbol
    get_statements = function(ticker) {
      
      # Define auxiliary functions
      process_response <- function(response){
        raw_content <- httr::content(response)
        filings <- raw_content$filings
        
        filings_list <- lapply(filings, function(f){
          file_list <- as.list(f)
          file_table <- data.table::as.data.table(file_list)
          return(file_table)
        })
        
        statements_table <- data.table::rbindlist(filings_list, fill = TRUE)
      }
      clean_numbers_format <- function(statements){
        can_be_numeric <- function(x) {
          is.na(suppressWarnings(as.numeric(x)))
        }
        format_numerics <- function(dt) {
          for (j in 1:ncol(dt)) {
            if (all(can_be_numeric(dt[[j]]))) {
              data.table::set(dt, j = j, value = as.numeric(dt[[j]]))
            }
          }
          return(dt)
        }
        
        format_numerics(statements)
      }
      
      # Send log
      private$log(glue::glue("Looking for {ticker} statements"))

      # Send HTTP request
      statements_url <- glue::glue(private$base_url, "/company/filings?ticker={ticker}")
      response <- httr::GET(
        url = statements_url,
        httr::add_headers(Authorization = glue::glue("Bearer {private$credentials$token}")),
        encode = "json"
      )
      
      # Manage possible errors
      private$manage_errors(response)
      
      # Process HTTP response
      statements <- process_response(response)
      
      # Standardise column names
      statements <- janitor::clean_names(statements)
      
      # Add new column containing the selected ticker
      statements$ticker <- rep(ticker, nrow(statements))
      
      return(tibble::as_tibble(statements))
    },
    
    #' Retrieves list of available tickers
    #' @param NULL
    #' 
    get_tickers = function(){
      
      # Auxiliary Functions
      process_response <- function(response){
        raw_content <- httr::content(response)
        unlist(raw_content$tickers)
      }
      
      # Send log
      private$log(glue::glue("Looking for tickers list"))
      
      # Send HTTP request
      tickers_url <- glue::glue(private$base_url, "/company/tickers")
      response <- httr::GET(
        url = tickers_url,
        httr::add_headers(Authorization = glue::glue("Bearer {private$credentials$token}")),
        encode = "json"
      )
      
      # Manage possible errors
      private$manage_errors(response)
      
      # Process response
      tickers <- process_response(response)
      
      return(tickers)
    }
  )
)
