#' @description This function adjusts prices or dividends by past company splits
#' @internal adjust_for_splits
#'
adjust_for_splits <- function(df, symbol){
  variables <- colnames(df)
  dates <- df$date
  
  df <- xts::xts(df[[2]], as.Date(df[[1]]))
  colnames(df) <- variables[[2]]
  splits <- get_splits(symbol, from = "1800-01-01")
  if(length(splits) > 1){
    if (nrow(splits) > 0 & nrow(df) > 0) {
      df <- df * TTR::adjRatios(splits = merge(splits, dates))[, 1]
    }
  }
  
  df <- data.frame(df)
  df <- tibble::as_tibble(df)
  df$date <- dates
  
  df <- dplyr::select(df, c(variables))
  return(df)
}