#' Function for determining a pathway
#'
#'@param newtree Decision tree
#'@keywords pathway decision tree
#'@export
#'@examples
#'
#'ptree_y()

ptree_y <- function(newtree, node_id)
{
  p<-predict_party(newtree, node_id)[[1]]
  return(p)
}