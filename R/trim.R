#' Function for determining a pathway
#'
#' Parsing function
#'
#' @param x String
#' @keywords pathway decision tree
#' @export

trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
