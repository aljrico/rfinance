# Quiets concerns of R CMD check regarding global variables
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".", "rfinanceConnection", "getTickerFromPage"))
}