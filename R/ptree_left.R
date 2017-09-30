#'Function for determining a pathway
#'
#'Identifies a node that corresponds to the left split
#'
#'@param newtree Decision tree generated as a party object
#'@param start_id Start ID 
#'@keywords pathway decision tree
#'

ptree_left <- function(newtree, start_id){
  node<-as.list(node_party(newtree))
  if (!is.null(node[[start_id]]$kids)){node[[start_id]]$kids[1]} 
}
