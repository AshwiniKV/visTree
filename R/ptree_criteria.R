#' Function for determining a pathway
#'
#'@param newtree decision tree
#'@keywords pathway decision tree
#'@export
#'@examples
#'
#'ptree_criteria()

ptree_criteria <- function(newtree, node_id, left)
{
  tree<-node_party(newtree)
  node<-as.list(node_party(newtree))
  if (length(nodeapply(tree, ids = nodeids(tree))[[node_id]]) == "0") # Check if this is a terminal node
  {
    return("(error: terminal node)");
  } 
  if (node[[node_id]]$split$breaks){
    sp <- node[[node_id]]$split$breaks
    if(as.party.tree == TRUE){
      split_id<-node[[node_id]]$split$varid
      vn<-names(data_party(newtree))[split_id]
    }else{
      vn <- names(node[[node_id]]$info$p.value)}
    if (left) {
      op <- '<='   
    } else {
      op <- '>'
    }
    return(paste(vn, op, sp))
  } else {
    if (left){
      l <- is.null(node[[node_id]]$split$breaks)
    } else {
      l <-  is.null(!node[[node_id]]$split$breaks)
    }
    
    r <- paste(attr(node[[node_id]]$split$breaks, 'levels')[l], sep='', collapse="','")
    return(paste(names(node[[node_id]]$info$p.value), " in ('", r,"')", sep=''))
  }
}