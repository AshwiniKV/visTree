#' Function for determining a pathway
#' 
#' Decision tree structure
#'
#'@param newtree Decision tree generated as a party object
#'@param node_id Node ID
#'@param start_criteria Character vector
#'@param as.party.tree Convert to a party object
#'@keywords pathway decision tree
#'@export
#'@import partykit
#'@importFrom partykit ctree
#'@examples
#' l_node(partykit::ctree(Girth ~ ., data = trees), as.party.tree = FALSE)
#'

l_node <- function(newtree, node_id = 1, start_criteria = character(0), as.party.tree)
{
  tree<-node_party(newtree)
  node<-as.list(node_party(newtree))
  
  if (!length(nodeapply(tree, ids = nodeids(tree))[[node_id]])) {
    prediction <- predict_party(newtree, node_id)[[1]]
    ypred <- paste(start_criteria,',y =',prediction)
    print(ypred)
  }
  
  left_node_id <- ptree_left(newtree, node_id)
  right_node_id <- ptree_right(newtree,node_id)
  
  if (is.null(left_node_id) != is.null(right_node_id)) {
    print('left node ID != right node id')
  }
  ypred <- character(0)
  if (!is.null(left_node_id)) {
    if(as.party.tree == TRUE){
      new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, TRUE, TRUE), sep=',')
    }else{
      new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, TRUE, FALSE), sep=',')
    }
    if (1 == node_id)
      if(as.party.tree == TRUE){
        new_criteria <- ptree_criteria(newtree, node_id, TRUE, TRUE)
      }else{
        new_criteria <- ptree_criteria(newtree, node_id, TRUE, FALSE)
      }
    ypred <- l_node(newtree, left_node_id, new_criteria, as.party.tree)
  }
  if (!is.null(right_node_id)) {
    if(as.party.tree == TRUE){
      new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, FALSE, TRUE), sep=',')
    }else{
      new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, FALSE, FALSE), sep=',')
    }
    if (1 == node_id)
      if(as.party.tree == TRUE){
        new_criteria <- ptree_criteria(newtree, node_id, F, TRUE)
      }else{
        new_criteria <- ptree_criteria(newtree, node_id, F, FALSE) 
      }
    ypred <- paste(ypred, l_node(newtree, right_node_id, new_criteria, as.party.tree))
  }
  if(!is.character(ypred)){
    return(ypred)
  }
}