#' Generate individual subplots within the graphical visualization
#'
#' A series of sub-plots that correspond to individual terminal nodes within the decision tree structure. Each subplot is composed of a histogram in the background that shows the distribution of the outcome for individuals in this subgroup and colored horizontal bars which summarize the set of covariate splits used to define the subgroup.
#'
#'@param My A matrix to define the split points within the decision tree structure
#'@param X Covariates
#'@param Y Response variable
#'@param str Structure of pathway from the root node in the decision tree to each terminal node
#'@param color.type Color palettes. (rainbow_hcl = 1; heat_hcl = 2; terrain_hcl = 3; sequential_hcl = 4; cm.colors = 5)
#'@param alpha Transparency of individual horizontal bars. Choose values between 0 to 1.
#'@param cond.tree Tree as a party object
#'@param text.main Change the size of the main titles
#'@param text.axis Change the size of the text of axis labels
#'@param text.title Change the size of the text in the title
#'@param text.bar Change the size of the text in the horizontal bar and below the bar plot
#'@param text.label Change the size of the axis annotation
#'@keywords matrix pathway decision tree
#'@export
#'@importFrom graphics barplot hist par plot polygon segments text title lines layout axis abline
#'@importFrom stats aggregate ecdf fitted smooth.spline density
#'@importFrom grDevices cm.colors col2rgb gray rgb 
#'
#'

plot_minmax <- function(My,X,Y, str, color.type, alpha, cond.tree, text.main, text.bar, text.title, text.axis, text.label) {
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
  colors<-list(colorspace::rainbow_hcl(n = nrow(mymat)), colorspace::heat_hcl(n = nrow(mymat)), colorspace::terrain_hcl(n = nrow(mymat)), colorspace::sequential_hcl(n = nrow(mymat)), cm.colors(n = nrow(mymat)))
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
    plot(NA,xlim=c(0,1),ylim=c(0,max.y),ylab="",xlab="Percentile",font = 2, main=paste0("Node ID = ",tail(comps[[1]], 1),"(", "n = ",length(node.index),")"),bty="n",yaxt="n", cex.axis = text.label,cex.main = text.title)
    
    ## Plot the background histogram
    
  }else{
    H <- hist(ecdf(Y)(Y[node.index]),breaks=seq(0,1,by=0.1),plot=FALSE)
    ## Scale the histogram so it fits vertically on the plot.
    scale.factor <- max.y/max(H$density)
    ## Set up an empty plot of the correct size
    #plot(NA,xlim=c(0,1),ylim=c(0,max.y),ylab="", font = 2,main=paste0("Node ID = ", tail(comps[[1]], 1), " (Mean = ",round(my.y.val, 2),", n = ",length(node.index),")"),bty="n", yaxt = "n", xaxt = "n",cex.axis = text.label, cex.main =text.title)
    plot(NA,xlim=c(0,1),ylim=c(0,max.y),ylab="", font = 2,main=paste0("Node ID = ", tail(comps[[1]], 1), " (n = ",length(node.index),")"),bty="n", yaxt = "n", xaxt = "n",cex.axis = text.label, cex.main =text.title)
    axis(side = 3, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = rep("", 6),tck = 0.05)
    title(main= "Percentile", line = -1.05, cex.main = text.axis)
    axis(side = 3, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = c(0, 20, 40, 60, 80, 100), line = -2.5, lwd = 0,cex.axis = text.label, font = 2)
    ## Plot the background histogram
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
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i],"-", c("male", "female")[mymat[i]]), font = 2, cex = text.bar)}else{
          text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i],"-", "female"), font = 2, cex = text.bar)
        }}
    if(rownames(My$M)[i] != "sex"){
      if(mymat[i,2] == Inf){idx <- 1}else{idx <-2}
      if(My$M[i,1] != -Inf && My$M[i,2] == Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], ">",round(as.numeric(My$M[i,1]), 2)), font = 2, cex = text.bar)
      }
      if(My$M[i,1] != -Inf && My$M[i,2] != Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], "<=",round(as.numeric(My$M[i,2]), 2), "\n",rownames(My$M)[i], ">",round(as.numeric(My$M[i,1]), 2)), font = 2, cex = text.bar)
      }
      if(My$M[i,1] == -Inf && My$M[i,2] != Inf){
        text(mean(c(lo,hi)),j,paste0(rownames(My$M)[i], "<=",round(as.numeric(My$M[i,2]), 2)), font = 2, cex = text.bar)
      }}## Label the variables
    j <- j+1
  }
  if(is.factor(Y)){
    bp<-barplot(scale.factor*H$density,width=wdth,yaxt = "n",col=rgb(0,0,0,0.15),border=rgb(0,0,0,0.1),add=TRUE,space=0)
    ## Add the category labels
    text(seq(wdth/2,1-wdth/2,by=wdth),rep(0,length(levels(Y))),levels(Y),pos=3,adj=0.5,cex=text.bar, col=gray(0.1))
    #text(seq(wdth/2,1-wdth/2,by=wdth),rep(quantile(scale.factor*H$density, 0.97),length(levels(Y))),levels(Y),pos=3,adj=0.5,cex=1.5,col=gray(0.5))
    
  }else{
    max.density<-max(hist(Y, plot = FALSE)$density)
    H <- hist(Y[node.index],plot=TRUE, prob = TRUE,main = " ",yaxt = "n",font = 2,cex.axis = text.bar,border = rgb(0,0,0,0.1),col = rgb(0,0,0,0.15))
    lines(density(Y[node.index]), lty = 2, lwd = 1.5)
    if(inherits(cond.tree, "constparty")){
      title(main = paste0(names(cond.tree$data)[1], " (Mean = ",round(my.y.val, 0), ")"), cex.main = text.main)}else{
        title(main = paste0(names(as.party(cond.tree)$data)[1], " (Mean = ",round(my.y.val, 0), ")"), cex.main = text.main)
      }
    
    ## Draw in a line for the mean
    mu.Y <- mean(Y[node.index])
    abline(v = mu.Y, col=rgb(0,0,0,0.5),lwd=2) 
    #counts.ecdf<-ceiling(10*ecdf(Y)(Y[node.index]))
    #mean.Ynode<-data.frame(counts.ecdf, Y[node.index])
    #mean.Y<-aggregate(Y[node.index], list(counts.ecdf),  mean)$x
    #if(all((mean.Y)<=10)){mean.Y<-round(mean.Y, 1)}else{
    # mean.Y<-round(mean.Y, 0)
    #}
    #titles.Y<-rep(" ", length(H$density))
    #titles.Y[!H$density == 0]<-mean.Y
    #bp<-barplot(scale.factor*H$density, names.arg = titles.Y, width=0.1,col=rgb(0,0,0,0.15),border=rgb(0,0,0,0.1),add=FALSE,yaxt = "n",space=0, cex.names = text.bar, font = 2)
    #lines(smooth.spline(y = scale.factor*H$density, x = H$mids, spar = 0.4),col = rgb(0,0,0,0.4), lwd = 2)
    #abline(h = 0, lwd = 2)
    #titles.Y<-rep(" ", length(H$density))
    #titles.Y[!H$density == 0]<-mean.Y
    #text(bp, 0, titles.Y, pos=3,cex=text.bar, font = 2)
    #title(main = )
    #if(inherits(cond.tree, "constparty")){
    #title(main = paste0(names(cond.tree$data)[1], " (Mean = ",round(my.y.val, 0), ")"), cex.main = text.main)}else{
    # title(main = paste0(names(as.party(cond.tree)$data)[1], " (Mean = ",round(my.y.val, 0), ")"), cex.main = text.main)
    #}
    ## Draw in a line for the mean
    #mu.Y <- mean(Y[node.index])
    #segments(ecdf(Y)(mu.Y),0,ecdf(Y)(mu.Y),max.y,col=rgb(0,0,0,0.5),lwd=2) 
  }
}