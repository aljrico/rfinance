get_handle <- function(curl.options = list(), force.new = FALSE)
{
  
  # establish session
  new_session <- function() {
    for (i in 1:5) {
      h <- curl::new_handle()
      curl::handle_setopt(h)
      
      # random query to avoid cache
      random_url <- paste(sample(c(letters, 0:9), 4), collapse = "")
      cu <- paste0("https://finance.yahoo.com?", random_url)
      z <- curl::curl_fetch_memory(cu, handle = h)
      if (NROW(curl::handle_cookies(h)) > 0)
        break
      Sys.sleep(0.1)
    }
    
    if (NROW(curl::handle_cookies(h)) == 0)
      stop("Could not establish session after 5 attempts.")
    
    return(h)
  }
  
  my_session <- new_session()
  
  n <- if (runif(1, 0, 1) >= 0.5) 1L else 2L
  query_url <- glue::glue("https://query{n}.finance.yahoo.com/v1/test/getcrumb")
  cres <- curl::curl_fetch_memory(query_url, handle = my_session)
  content <- rawToChar(cres$content)
  return(list(session = my_session, content = content))
}
