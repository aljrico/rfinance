fetch_company_profile <- function(symbol) {
  url <- glue::glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}")
  content <- url %>%
    httr::GET() %>%
    httr::content()
  
  # Substitutes NULL by NA. A list with NULL values can not be transformed to data.frame, whereas NA values are more permissive.
  clean_null <- function(x) ifelse(is.null(x), NA, x)
  
  content <- content$profile
  content <- lapply(content, FUN = clean_null)
  company_profile <- as.data.frame(content)
  
  company_profile %>% 
    dplyr::mutate(symbol = symbol) %>% 
    dplyr::select(symbol, companyName, sector, industry, price, beta, exchange, lastDiv, mktCap, ceo, description)
}


#' Basic Company Profile
#' @name get_company_profile
#' @description This function downloads the latest basic information of a given company. Full name, sector, last dividend, etc.
#' @return Returns a tibble
#' @param symbol Specifies the ticker or symbol the user wants to download the data of, in the form of a string.
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
get_company_profile <- function(symbol) {
  suppressWarnings(dplyr::bind_rows(pbapply::pblapply(symbol, FUN = fetch_company_profile)))
}
