#' Function for determining a pathway
#'
#'Identifies a node that corresponds to the right split
#'
#'@param newtree Decision tree
#'@param start_id Start ID 
#'@keywords pathway decision tree
#'@export


ptree_right <- function(newtree, start_id){
  node<-as.list(node_party(newtree))
  if (!is.null(node[[start_id]]$kids)){node[[start_id]]$kids[2]}
}