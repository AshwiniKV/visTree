#' Function for determining a pathway
#'
#'@param treet Decision tree
#'@keywords pathway decision tree
#'@export
#'@examples
#'
#'path_node()

path_node<-function(treet){
  pathway<-capture.output(l_node(treet))
  gerid<-gsub("*\\[1](*)\\'*", "\\1", pathway)
  gerid<-gsub("\"", "", gerid)
  return(paste(gerid, collapse = " ;"))
}