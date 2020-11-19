#' Plot PULS Splitting Rule Tree
#'
#' Print the PULS tree in the form of dendrogram.
#'
#' @param x PULS object.
#' @param margin An extra fraction of white space to leave around the borders of
#'   the tree. (Long labels sometimes get cut off by the default computation).
#' @param which Labeling modes, which are:
#'   * 1: only splitting variable names are shown, no splitting rules.
#'   * 2: only splitting rules to the left branches are shown.
#'   * 3: only splitting rules to the right branches are shown.
#'   * 4 (default): splitting rules are shown on both sides of branches.
#' @param text Whether to print the labels on the tree.
#' @param cols Whether to use color or not.
#' @param ... Arguments to be passed to [graphics::plot.default()] and
#'   [graphics::lines()].
#'
#' @return A plot of splitting order.
#' @export
#'
#' @examples
#' library(fda)
plot.PULS<-function(x,margin,which,text=TRUE,cols=NULL,...){
    ## This function sets some defaults and changes things a bit, but is mostly a
    ## wrapper for our slightly modified version of rpart's plot function (see plots.R).

	if(missing(margin)){margin<-c(.12,.02,0,.05)}
	if(missing(which)){which <- 4}

	plot.rpart(x,margin=margin,...)

	# lines(x=c(.88,.88),y=c(0,1))
	#
	# for(i in seq(0,1,.1)){
	# 	lines(x=c(.86,.88),y=c(i,i))
	# 	text(.73,i,i)
	# }

	if(text){
		text.PULS(x,which=which,abbrev=abbrev,cols=cols)
	}


}

Nclustplot<-function(x, main, type, ylab, xlab,...){

	if(missing(main)){main<-"Marginal Cluster Analysis"}
	if(missing(type)){type<-"b"}
	if(missing(ylab)){ylab<-"Proportion of Deviance Explained"}
	if(missing(xlab)){xlab<-"Number of Clusters"}

    inds <- seq(from=2,to=nrow(x$frame), by =2)
    plot(inds,round((1-as.numeric(x$frame$yval[inds])/1),digits=2),type=type, xaxt="n", ylab=ylab, xlab=xlab, main=main)
    axis(1, at=inds, labels= as.character(2 + 0:(length(inds)-1)))

}

PULSplot<-function(x,toclust.fd, main, type, ylab, xlab,...,version){
  #Make a nice plot of observations with cluster info a medoid highlighted
  #Version 1: Row of plots with one cluster per row, in same order as tree?, with medoids in bolder and others lightened
  clustids<-x$Membership
  medoids<-x$frame$medoid[x$frame$var=="<leaf>"]

  #Re-do the membership ids with a second ordered set to make plot
  clustid2<-as.numeric(factor(clustids))
  if(version!=2){
  par(mfrow=c(1,1))
  plot(toclust.fd,col="grey",lty=clustid2,lwd=.9,main="Plot of the functional responses by clusters with medoids",ylab="Response",xlab="")
  lines(toclust.fd[medoids],col=clustid2[medoids],lty=1,lwd=3)
  }
  if(version==2){
    nobs<-length(toclust.fd$fdnames$reps)
    ids=1:nobs
    ylim<-c(min(fda::predict.fd(toclust.fd))-.05,max(fda::predict.fd(toclust.fd)))
    par(mfrow=c(1,length(medoids)))
    for (i in (1:length(medoids))){
    plot(toclust.fd[clustids==clustids[medoids[i]]],col="grey",lty=clustid2[medoids[i]],lwd=.9,main= paste("Cluster", deparse(clustids[medoids[i]])),ylim=ylim,ylab="Response",xlab="")
    lines(toclust.fd[medoids[i]],col=clustid2[medoids[i]],lwd=2)

    }
    par(mfrow=c(1,1))
  }

}

PULSggplot<-function(x,toclust.fd, main, type, ylab, xlab,...){

  #Make a nice plot of observations with cluster info a medoid highlighted
  #Version 2: Row of plots with one cluster per row, in same order as tree?, with medoids in bolder and others lightened
  clustids<-x$Membership
  medoids<-x$frame$medoid[x$frame$var=="<leaf>"]

  #Re-do the membership ids with a second ordered set to make plot
  clustid2<-as.numeric(factor(clustids))
  plot(toclust.fd,col="grey",lty=clustid2,lwd=.9,main="Plot of the functional responses by clusters with medoids",ylab="Response",xlab="")
  lines(toclust.fd[medoids],col=clustid2[medoids],lty=1,lwd=3)


}


silh.plot<-function(toclust.fd,intervals,maxnclust){
  silhs<-numeric()
  clustmems<-numeric()
  par(mfrow=c(1,maxnclust-1))
  for (i in (2:maxnclust)){
    #Run cluster analysis of each size (would really benefit from having distance matrices saved and passed on or just from pruning the tree)

    clust<-puls(toclust.fd,intervals=intervals,nclusters=i)
     clustmems[,i-1]=clust$Membership
    #Extract cluster membership info
    silhs<-c(silhs,mean(cluster::silhouette(clust$Membership,dmatrix=clust$Dist)[,3]))
    plot(cluster::silhouette(clust$Membership,dmatrix=clust$Dist))
  }
  par(mfrow=c(1,1))
  plot(2:(length(silhres)+1),silhres,type="l",ylim=c(0,max(silhres)))
  print(data.frame(Size=2:(length(silhres)+1),AveSilh=silhres))
  return(silhs,clustmems)


}
