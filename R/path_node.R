#' Function for determining a pathway
#' 
#' Generates the pathway from the root node to individual terminal nodes of a decision tree generated as a party object using the partykit package. 
#'
#'@param newtree Decision tree generated as a party object
#'@param as.party.tree Convert to a party object
#'@param idnumber Terminal ID number
#'@keywords pathway decision tree
#'@export
#'@import partykit
#'@importFrom utils capture.output tail
#'@importFrom partykit ctree
#'@examples
#'path_node(partykit::ctree(Girth ~ ., data = trees), as.party.tree = FALSE)
#'
path_node<-function(newtree, as.party.tree, idnumber = 0){
  if(as.party.tree == TRUE){
    pathway<-capture.output(l_node(newtree, as.party.tree = T))
  }else{
    pathway<-capture.output(l_node(newtree, as.party.tree = F)) 
  }
  terminal.id<-nodeids(newtree, terminal = TRUE)
  gerid<-gsub("*\\[1](*)\\'*", "\\1", pathway)
  gerid<-gsub("\"", "", gerid)
  if(idnumber >0){
    index<-which(terminal.id == idnumber)
    if(length(index)>0){
    return(gerid[index])
    }else{stop("This ID does not correspond to a Terminal Node")}
  }else{
  return(paste(gerid, collapse = " ;"))
  }
}