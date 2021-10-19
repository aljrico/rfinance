#' Date To Unix
#' @description Basic 'util' function to transform date-like values into unix. This is necessary for the Yahoo API.
#' @param Date String that can be transformed into Date format.
#' @keywords internal
date_to_unix <- function(Date) {
  posixct <- as.POSIXct(as.Date(Date, origin = "1970-01-01"))
  trunc(as.numeric(posixct))
}
