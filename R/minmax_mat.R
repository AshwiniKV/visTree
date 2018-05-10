#' Minmax matrix
#'
#' Identifies splits and relevant criteria
#'@param Y Response variable in the dataset
#'@param str Structure of pathway from the root node in the decision tree to each terminal node
#'@param varnms Names of covariates
#'@param interval logical. Continuous (interval = FALSE) and Categorical variable (interval = TRUE).
#'@keywords visualization pathway decision tree
#'@export
#'

minmax.mat <- function(str,varnms, Y,interval) {
  ## Helper function to create a matrix of ranges for each variable in a path to a node
  comps <- strsplit(str,",")
  MMM <- matrix(data=rep(c(-Inf,Inf, " "),length(varnms)),nrow=length(varnms),ncol=3,byrow=TRUE) ### min-max matrix
  rownames(MMM) <- varnms
  #nodeids(potentialtree)
  Mlist<-list()
  #Y<-fitted(cond.tree)[[3]]
  if(is.numeric(Y)){length.mat<-length(comps[[1]])-2}
  if(is.factor(Y)){if(interval == TRUE){length.mat<-length(comps[[1]])-3}else{length.mat<-length(comps[[1]])-2}}
  for(i in 1:length.mat) {
    nodestr <- strsplit(trim(comps[[1]][i])," ")
    node.varnm <- trim(nodestr[[1]][1])
    node.dir <- trim(nodestr[[1]][2])
    node.split <- trim(nodestr[[1]][3])
    var.row <- which(varnms==node.varnm)
    if(node.dir == "<=") {
      MMM[var.row,2] <- as.numeric(node.split)
      MMM[var.row,3] <- "<="
    } else {
      MMM[var.row,1] <- as.numeric(node.split)
      MMM[var.row,3] <- ">"
    }
    Mlist[[i]]<-c(node.varnm, MMM[var.row,])
    output <- matrix(unlist(Mlist), ncol = 4, byrow = TRUE)
  }
  if(is.factor(Y)){y<-paste0(comps[[1]][length(comps[[1]]) - 2],",",comps[[1]][length(comps[[1]]) - 1])}
  if(is.numeric(Y)){y <- comps[[1]][length(comps[[1]]) - 1]}
  return(list(M=MMM,y=y))
}
