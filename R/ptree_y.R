#' Function for determining a pathway
#'
#'Identifies the predicted outcome value for the relevant node.
#'
#'@param newtree Decision tree generated as a party object
#'@param node_id Node ID
#'@keywords pathway decision tree
#'@export
#'
ptree_y <- function(newtree, node_id)
{
  # Picks out the prediction from the ctree structure
  p<-predict_party(newtree, node_id)[[1]]
  return(p)
}