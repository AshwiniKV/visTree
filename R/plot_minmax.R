#' Generate individual subplots within the graphical visualization
#'
#' A series of sub-plots that correspond to individual terminal nodes within the decision tree structure. Each subplot is composed of a histogram in the background that shows the distribution of the outcome for individuals in this subgroup and colored horizontal bars which summarize the set of covariate splits used to define the subgroup.
#'
#'@param My A matrix to define the split points within the decision tree structure
#'@param X Covariates
#'@param Y Response variable
#'@param str Structure of the pathway parsed to suggest the splits and relevant criterions for each terminal node
#'@param color.type Color palettes. (rainbow = 1; heat.colors = 2; terrain.colors = 3; topo.colors = 4 ; cm.colors = 5)
#'@param alpha Transparency of individual horizontal bars. Choose values between 0 to 1
#'@keywords matrix pathway decision tree
#'@export
#'@importFrom graphics barplot hist par plot polygon segments text
#'@importFrom stats aggregate ecdf fitted
#'@importFrom grDevices cm.colors col2rgb gray heat.colors rainbow rgb terrain.colors topo.colors
#'
#'

plot_minmax <- function(My,X,Y, str, color.type, alpha) {
  ## Main function which plots the bars for each variable along with a histogram of the outcome
  comps <- strsplit(str,",")
  mymat <- matrix(as.numeric(My$M[,-3]), ncol = 2)
  my.y <- My$y
  mydir<-My$M[,3]
  if(!is.factor(Y)) {
    my.y.val <- as.numeric(strsplit(trim(my.y)," ")[[1]][3])
    my.y.pct <- ecdf(Y)(my.y.val)
  }
  
  var.nms <- rownames(My$M)
  act.vars <- apply(mymat,1,function(x) { !all(abs(x)==Inf) })
  
  max.y <- sum(act.vars)+1  
  #rbw <- cm.colors(n=nrow(mymat))
  colors<-list(rainbow(n = nrow(mymat)), heat.colors(n = nrow(mymat)), terrain.colors(n = nrow(mymat)), topo.colors(n = nrow(mymat)), cm.colors(n = nrow(mymat)))
  rbw<-colors[[color.type]]
  
  ## Find the y's which "belong" in this node
  node.index <- 1:length(Y)
  for(i in 1:nrow(mymat)) {
    digit<-8
    node.index <- intersect(node.index,c(which(round(X[,i],digit)>round(mymat[i,1],digit)&round(X[,i],digit)<=round(mymat[i,2],digit))))
  }
  
  ## Create the underlying histogram, but don't plot it yet
  if(is.factor(Y)) {
    wdth <- 1/length(levels(Y))
    H <- hist(as.integer(Y[node.index])/length(levels(Y)),breaks=seq(0,1,length.out=length(levels(Y))+1),plot=FALSE)
    ## Scale the histogram so it fits vertically on the plot.
    scale.factor <- max.y/max(H$density)
    ## Set up an empty plot of the correct size
    plot(NA,xlim=c(0,1),ylim=c(0,max.y),ylab="",xlab="Percentile",font = 2, main=paste0("Node ID = ",tail(comps[[1]], 1),"(", "n = ",length(node.index),")"),bty="n",yaxt="n", cex.axis = 1.5,cex.main = 2.25)
    ## Plot the background histogram
    barplot(scale.factor*H$density,width=wdth,col=rgb(0,0,0,0.15),border=rgb(0,0,0,0.1),add=TRUE,space=0,yaxt="n")
    ## Add the category labels
    text(seq(wdth/2,1-wdth/2,by=wdth),rep(0,length(levels(Y))),levels(Y),pos=3,adj=0.5,cex=1.5, col=gray(0.1))
    #text(seq(wdth/2,1-wdth/2,by=wdth),rep(quantile(scale.factor*H$density, 0.97),length(levels(Y))),levels(Y),pos=3,adj=0.5,cex=1.5,col=gray(0.5))
    
  }
  else{
    H <- hist(ecdf(Y)(Y[node.index]),breaks=seq(0,1,by=0.1),plot=FALSE)
    ## Scale the histogram so it fits vertically on the plot.
    scale.factor <- max.y/max(H$density)
    ## Set up an empty plot of the correct size
    plot(NA,xlim=c(0,1),ylim=c(0,max.y),ylab="", xlab="Percentile",font = 2,main=paste0("Node ID = ", tail(comps[[1]], 1), " (Mean = ",round(my.y.val, 2),", n = ",length(node.index),")"),bty="n", yaxt = "n", cex.axis = 1.5, cex.main =2.25)
    ## Plot the background histogram
    bp<-barplot(scale.factor*H$density,width=0.1,col=rgb(0,0,0,0.15),border=rgb(0,0,0,0.1),add=TRUE,space=0, yaxt = "n")
    counts.ecdf<-ceiling(10*ecdf(Y)(Y[node.index]))
    mean.Ynode<-data.frame(counts.ecdf, Y[node.index])
    mean.Y<-aggregate(Y[node.index], list(counts.ecdf),  mean)$x
    if(all((mean.Y)<=10)){mean.Y<-round(mean.Y, 1)}else{
      mean.Y<-round(mean.Y, 0)
    }
    titles.Y<-rep(" ", length(H$density))
    titles.Y[!H$density == 0]<-mean.Y
    text(bp, 0, titles.Y, pos=3,adj=0.5,cex=1.5)
    ## Draw in a line for the mean
    mu.Y <- mean(Y[node.index])
    segments(ecdf(Y)(mu.Y),0,ecdf(Y)(mu.Y),max.y,col=rgb(0,0,0,0.5),lwd=2)    
  }
  
  ## Now plot the horizontal bars corresponding to each variable.
  j <- 1
  for(i in which(act.vars)) {
    F.x <- ecdf(X[,var.nms[i]])
    lo <- ifelse(mymat[i,1]==-Inf,0,F.x(mymat[i,1]))
    hi <- ifelse(mymat[i,2]==Inf,1,F.x(mymat[i,2]))
    polygon(c(lo,lo,hi,hi),c(j-0.5,j+0.5,j+0.5,j-0.5),col=makeTransparent(rbw[i],alpha = alpha),border=NA)
    
    if(rownames(My$M)[i] == "sex"){
      if(mymat[i] != "-Inf"){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i],"-", c("male", "female")[mymat[i]]), font = 2, cex = 1.6)}else{
          text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i],"-", "female"), font = 2, cex = 1.6)
        }}
    if(rownames(My$M)[i] != "sex"){
      if(mymat[i,2] == Inf){idx <- 1}else{idx <-2}
      if(My$M[i,1] != -Inf && My$M[i,2] == Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], ">",round(as.numeric(My$M[i,1]), 2)), font = 2, cex = 1.6)
      }
      if(My$M[i,1] != -Inf && My$M[i,2] != Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], "<=",round(as.numeric(My$M[i,2]), 2), "\n",rownames(My$M)[i], ">",round(as.numeric(My$M[i,1]), 2)), font = 2, cex = 1.6)
      }
      if(My$M[i,1] == -Inf && My$M[i,2] != Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], "<=",round(as.numeric(My$M[i,2]), 2)), font = 2, cex = 1.6)
      }}## Label the variables
    j <- j+1
  }
}