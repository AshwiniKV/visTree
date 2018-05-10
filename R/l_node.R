#' Function for determining a pathway
#'
#' Decision tree structure
#'
#' @param newtree Decision tree generated as a party object
#' @param node_id Node ID
#' @param start_criteria Character vector
#' @keywords pathway decision tree
#' @export
#'

l_node <- function(newtree, node_id = 1, start_criteria = character(0)) {
  tree <- node_party(newtree)
  node <- as.list(node_party(newtree))

  if (!length(nodeapply(tree, ids = nodeids(tree))[[node_id]])) {
    prediction <- predict_party(newtree, node_id)[[1]]
    ypred <- paste(start_criteria, ",y =", prediction)
    print(ypred)
  }

  left_node_id <- ptree_left(newtree, node_id)
  right_node_id <- ptree_right(newtree, node_id)

  if (is.null(left_node_id) != is.null(right_node_id)) {
    print("left node ID != right node id")
  }
  ypred <- character(0)
  if (!is.null(left_node_id)) {
    new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, TRUE), sep = ",")
    if (1 == node_id) {
      new_criteria <- ptree_criteria(newtree, node_id, TRUE)
    }
    ypred <- l_node(newtree, left_node_id, new_criteria)
  }
  if (!is.null(right_node_id)) {
    new_criteria <- paste(start_criteria, ptree_criteria(newtree, node_id, FALSE), sep = ",")
    if (1 == node_id) {
      new_criteria <- ptree_criteria(newtree, node_id, F)
    }
    ypred <- paste(ypred, l_node(newtree, right_node_id, new_criteria))
  }
  if (!is.character(ypred)) {
    return(ypred)
  }
}
