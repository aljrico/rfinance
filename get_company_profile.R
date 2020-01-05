fetch_company_profile <- function(symbol) {
  url <- glue::glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}")
  content <- url %>%
    httr::GET() %>%
    httr::content()
  
  content <- content$profile
  company_profile <- as.data.frame(content)
  
  company_profile %>% 
    dplyr::mutate(symbol = symbol) %>% 
    dplyr::select(symbol, companyName, sector, industry, price, beta, exchange, lastDiv, mktCap)
}


#' Functions to call and retrieve financial data from remote databases, by using company tickers (symbols). It uses publicly available API to download all data.
#'
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string. Example: 'MSFT'
#'
#'
#' @author Alejandro Jiménez Rico \email{aljrico@@gmail.com}, \href{https://aljrico.github.io}{Personal Website}
#'
#'
#' @examples
#'
#' company_profile <- get_company_profile('MSFT')
#' @rdname get_company_profile
#' @export
#'
#'
#'
get_company_profile <- function(symbol) {
  suppressWarnings(dplyr::bind_rows(pbapply::pblapply(symbol, FUN = fetch_company_profile)))
}
