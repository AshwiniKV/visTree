#' Visualization of subgroups for decision trees
#' 
#' This visualization characterizes subgroups defined by a decision tree structure and identifies the range of covariate values associated with outcome values in each subgroup. 
#'
#'@param cond.tree Decision tree generated as a party object.
#'@param rng Restrict plotting to a particular set of nodes. Default value is set as NULL
#'@param interval logical. Continuous (interval = FALSE) and Categorical variable (interval = TRUE).
#'@param as.party.tree logical. Convert to a party object from other decision tree types such as an rpart object. 
#'@param color.type Color palettes (rainbow = 1; heat.colors = 2; terrain.colors = 3; topo.colors = 4 ; cm.colors = 5)
#'@param alpha Transparency for individual horizonatal colored bars within each subplot. Values between 0 to 1.
#'@keywords visualization pathway decision tree
#'@author Ashwini Venkatasubramaniam and Julian Wolfson 
#'@export
#'@importFrom utils capture.output tail 
#'@examples
#'visTree(partykit::ctree(Girth ~ ., data = trees), interval = FALSE, as.party.tree = FALSE)
#'

visTree <- function(cond.tree,rng=NULL, interval, as.party.tree, color.type = 2, alpha = 0.4) {
  ## Wrapper function to produce plots from a conditional inference tree
  ## 'range' parameter can restrict plotting to a particular set of nodes
  if(as.party.tree == TRUE){
    splittree<-path_node(cond.tree, as.party.tree = TRUE)
  }else{
    splittree<-path_node(cond.tree, as.party.tree = FALSE) 
  }
  structure<-strsplit(splittree, split=";")
  #structure<-unlist(strsplit(path_node(cond.tree), split=";"))
  terminal.id<-nodeids(cond.tree, terminal = TRUE)
  if(length(structure[[1]]) == length(terminal.id)){
    structure[[1]]<-sapply(1:length(structure[[1]]), function(i){
      paste0(structure[[1]][i],",", terminal.id[i], " ")})
  }
  
  input.info<-data_party(cond.tree)
  #  X <- input.info[,2:(length(input.info)-3)]
  if(as.party.tree == TRUE){
    X <- input.info[,2:(length(input.info)-2)]
    Y <- fitted(cond.tree)[[2]]
  }else{
    X <- input.info[,2:(length(input.info)-3)]
    Y <- fitted(cond.tree)[[3]]
  }
  if(is.factor(Y)) {
    n.terminals <- length(structure[[1]])
    #prob.mat <- matrix(data=unlist(lapply(structure[[1]],function(S) {
    # unlist(lapply(strsplit(S,","),function(split.S) {
    # seg <- unlist(split.S[length(split.S)])
    # as.numeric(trim(strsplit(seg,"=")[[1]][2]))
    #})) 
    #})), nrow=n.terminals)
    y.list <- sapply(1:length(structure[[1]]),function(j) {
      seg<-strsplit(structure[[1]],",")
      if(interval == TRUE){paste0(seg[[j]][c((length(seg[[j]])-2): (length(seg[[j]])-1))], collapse = ",")}else{
        paste0(seg[[j]][length(seg[[j]])-2], collapse = ",")}
    })
    
    x.list <- sapply(1:length(structure[[1]]),function(j) {
      seg<-strsplit(structure[[1]],",")
      x.l<-sapply(1:length(seg), function(i){
        if(interval == TRUE){
          x.length<-length(seg[[i]]) - 3}else{
            x.length<-length(seg[[i]])-2}
      })  
      if(interval == TRUE){
        paste0(seg[[j]][1:(length(seg[[j]])-3)], collapse = ",")}else{
          paste0(seg[[j]][1:(length(seg[[j]])-2)], collapse = ",")
        }
    })
    
    term.node<-sapply(1:length(structure[[1]]), function(j){
      seg<-strsplit(structure[[1]], ",")
      if(interval == TRUE){paste0(tail(seg[[j]],1), collapse = ",")}else{
        paste0(tail(seg[[j]],1), collapse = ",")}
    })
    
    structure <- lapply(1:length(y.list),function(i) {
      paste0(x.list[[i]],", ",y.list[[i]], ", ", term.node[[i]])
    })
  }
  if(length(unlist(structure))==1) { stop("Tree has only a single node; nothing to visualize.") }
  #terminal.id<-nodeids(cond.tree, terminal = TRUE)
  n.terminals <- ifelse(is.null(rng),length(unlist(structure)),length(rng))
  if(is.null(rng)) { 
    index <- 1:n.terminals } else { 
      index <- min(rng):min(max(rng),length(unlist(structure))) } ## Should probably do some range checking
  if(length(index)>10) stop("Number of subgroups is too large")
  par(mfrow=c(2,ceiling(length(index)/2)),mar=c(2,1,3,1))
  invisible(
  sapply(unlist(structure)[index],function(S) { plot_minmax(minmax_mat(Y, S,colnames(X), interval), X,Y, S, color.type, alpha)})
  )
}