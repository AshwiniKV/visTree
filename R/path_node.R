#' Function for determining a pathway
#'
#' Generates the pathway from the root node to individual terminal nodes of a decision tree generated as a party object using the partykit package.
#'
#' @param newtree Decision tree generated as a party object
#' @param idnumber Terminal ID number
#' @keywords pathway decision tree
#' @export
#' @import partykit
#' @importFrom utils capture.output tail
#'
path_node <- function(newtree, idnumber = 0) {
  pathway <- capture.output(l_node(newtree))
  gerid <- gsub("*\\[1](*)\\'*", "\\1", pathway)
  gerid <- gsub("\"", "", gerid)
  terminal.id <- nodeids(newtree, terminal = TRUE)
  if (idnumber > 0) {
    index <- which(terminal.id == idnumber)
    if (length(index) > 0) {
      return(gerid[index])
    } else {
      stop("This ID does not correspond to a Terminal Node")
    }
  } else {
    return(paste(gerid, collapse = " ;"))
  }
}
