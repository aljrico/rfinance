.is_date <- function(x){
  !suppressWarnings({
    x |> 
      lubridate::ymd() |> 
      is.na() |> 
      any()
  })
}

.find_date_column <- function(df){
  results <- sapply(colnames(df), function(c){
    df[[c]] |> .is_date()
  })
  
  which(results)
}

#' Excess Returns
#' @name excess_returns
#' @description This function will calculate the excess returns against the risk-free
#' @param r an array, matrix, data.frame or anything that can be interpreted as a time series of returns
#' @param rf risk-free rate. In same period as the returns or as a single digit average 
#'
#' @export
#'
excess_returns <- function(r, rf = 0){
  
  if(length(rf) == 1) return(r - rf)
  if(length(r) == length(rf)) return(r - rf)
  if(!.is_date(zoo::index(r)) | !.is_date(zoo::index(rf))) stop("Incompatible lengths")
  tab <- data.frame(r = r, rf = rf)
  tab[which(zoo::index(r) %in% zoo::index(rf)), ]
}
