#' get_sp500
#'
#' @name get_sp500
#' @description This function parses the most recent list of tickers for a given index
#' @return Returns a vector.
#' @param index Indicates what index should the tickets be retrieved from. Currently only 'sp100', 'sp400' and 'sp500' are working.
#' @rdname get_sp500
#' @export
#'
get_sp500 <- function(index = "sp500") {
  
  getTickersFromPage <- function(sp) {
    wiki <- xml2::read_html(sp[["wiki"]])
    symbols_table <- wiki %>%
      rvest::html_nodes(xpath = sp[["xpath"]]) %>%
      rvest::html_table()
    
    symbols_table <- symbols_table[[1]]
    return(as.character(symbols_table$`Symbol`))
  }
  
  
  SP1000 <- list(wiki = "https://en.wikipedia.org/wiki/List_of_S%26P_1000_companies", xpath = "//*[@id='mw-content-text']/div/table[3]")
  SP500 <- list(wiki = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", xpath = "//*[@id='mw-content-text']/div/table[1]")
  SP400 <- list(wiki = "https://en.wikipedia.org/wiki/List_of_S%26P_400_companies", xpath = "//*[@id='mw-content-text']/div/table[1]")

  if (index == "sp1000") {
    tickers <- getTickerFromPage(SP1000)
  }

  if (index == "sp500") {
    tickers <- getTickersFromPage(SP500)
  }

  if (index == "sp400") {
    tickers <- getTickersFromPage(SP400)
  }

  if (index == "all") {
    tickers <- unique(c(getTickersFromPage(SP500), getTickerFromPage(SP1000)))
  }


  for (i in 1:length(tickers)) tickers[[i]] <- gsub("\\.", "-", tickers[[i]])

  return(tickers)
}
