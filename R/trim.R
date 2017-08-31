#' Function for determining a pathway
#'
#'@param x String
#'@keywords pathway decision tree
#'@export
#'@examples
#'
#'trim()
#'

trim <-function (x){
  gsub("^\\s+|\\s+$", "", x)}
