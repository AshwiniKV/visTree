#' Function for determining a pathway
#'
#'@param newtree Decision tree
#'@keywords pathway decision tree
#'@export
#'@examples
#'
#'ptree_left()

ptree_left <- function(newtree, start_id){
  node<-as.list(node_party(newtree))
  if (!is.null(node[[start_id]]$kids)){node[[start_id]]$kids[1]} 
}
