#' Function for determining a pathway
#'
#'Identifies the splitting criteria for the relevant node leading to lower level inner nodes or a terminal node.
#'
#'@param newtree Decision tree
#'@param node_id Node id
#'@param left Splits to the left
#'@param as.party.tree convert to a party object
#'@keywords pathway decision tree

ptree_criteria <- function(newtree, node_id, left, as.party.tree)
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
    # Left being true then the left string of variables with split points are returned 
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