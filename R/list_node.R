#' Function for determining a pathway
#' 
#' Generates the pathway from the root node to individual terminal nodes of a decision tree generated as a party object using the partykit package. 
#'
#'@param newtree Decision tree generated as a party object
#'@param as.party.tree Convert to a party object
#'@keywords pathway decision tree
#'@export
#'@import partykit
#'@importFrom utils capture.output tail
#'@importFrom partykit ctree
#'@examples
#'list_node(partykit::ctree(Girth ~ ., data = trees), as.party.tree = FALSE)
#'
list_node<-function(newtree, as.party.tree){
  if(as.party.tree == TRUE){
    pathway<-capture.output(l_node(newtree, as.party.tree = T))
  }else{
    pathway<-capture.output(l_node(newtree, as.party.tree = F)) 
  }
  gerid<-gsub("*\\[1](*)\\'*", "\\1", pathway)
  gerid<-gsub("\"", "", gerid)
  return(paste(gerid, collapse = " ;"))
}
