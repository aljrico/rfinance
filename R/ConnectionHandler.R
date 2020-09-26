#' This is a ConnectionHandler class
#' @title ConnectionHandler class
#' @docType class
#' @description This class creates objects to handle connections to the database
#' @export
#'
ConnectionHandler = R6::R6Class(
  "ConnectionHandler",
  private = list(
    log = function(msg){
      full_msg = paste0("  rfinance: ", msg)
      message(full_msg)
    },
    credentials = list(
      username = NULL,
      password = NULL
    ),
    database = 'secgov-dev',
    collection = 'visited-links',
    base_url = 'mongodb+srv://%s:%s@dev-cluster.vvwni.gcp.mongodb.net/%s?retryWrites=true&w=majority',
    connection_url = NULL,
    connection_object = NULL,
    open_connection = function(){
      private$log("Establishing connection...")
      private$connection_object = mongolite::mongo(collection = 'visited-links', url = private$connection_url)
    },
    close_connection = function(){
      private$connection_object$disconnect()
      private$connection_object = NULL
      private$log("Connection closed.")
    }
  ),
  public = list(
    initialize = function(username, password){
      private$log("Checking credentials...")
      
      private$credentials$username = username
      private$credentials$password = password
      
      private$connection_url = sprintf(private$base_url, private$credentials$username, private$credentials$password, private$database)
      
    },
    get_statements = function(ticker){
      download_raw_data <- function(ticker){
        docs <- mongolite::mongo(collection = 'filings', url = private$connection_url) 
        json_string <- paste0('{"TradingSymbol":"', ticker, '"}')
        table_result <- data.table::data.table(docs$find(json_string))
        return(table_result)
      }
      order = function(dt){
        dt[order(-balance_sheet_date)]
      }
      
      # Establish Connection
      private$open_connection()
      
      # Get data
      private$log("Downloading data...")
      table_result <- 
        ticker %>% 
        download_raw_data() %>% 
        janitor::clean_names() %>% 
        order()
      
      # Close Connection
      private$close_connection()
      
      return(table_result)
      
    }
  )
)