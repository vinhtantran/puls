#' Partitioning Using Local Subregions (PULS)
#'
#' PULS function for functional data (only used when you know that the data
#' shouldn't be converted into functional because it's already smooth, e.g.
#' your data are step function)
#'
#' @param toclust.fd A functional data object as the result of `fda` package.
#' @param method The clustering method you want to run in each subregion. Can be
#'   chosen between `pam` and `ward`.
#' @param intervals A data set (or matrix) with rows are intervals and columns
#'   are the beginning and ending indexes of of the interval.
#' @param spliton Restrict the partitioning on a specific set of subregions.
#' @param distmethod The method for calculating the distance matrix. Choose
#'   between `"usc"` and `"manual"`.
#' @param labels The name of entities.
#' @param nclusters The number of clusters.
#' @param minbucket The minimum number of data points in one cluster allowed.
#' @param minsplit The minimum size of a cluster that can still be considered to
#'   be a split candidate.
#'
#' @seealso [fda::is.fd()]
#' @export
#'
#' @examples
#' library(fda)
#'
#' # Build common argval fd object from predicted.mat
#' NBASIS <- 300
#' NORDER <- 4
#' predicted.mat <- readRDS("data/predicted.mat.rds")
#' y <- t(predicted.mat)
#' spline_basis <-
#'   create.bspline.basis(rangeval = c(1, 366),
#'                        nbasis = NBASIS,
#'                        norder = NORDER)
#'
#' # No need for any more smoothing
#' fdParobj <-
#'   fdPar(fdobj = spline_basis,
#'         Lfdobj = 2,
#'         lambda = .000001)
#' yfd.full <- smooth.basis(argvals = 1:366,
#'                          y = y,
#'                          fdParobj = fdParobj)
#' yfd.train <-
#'   smooth.basis(
#'     argvals = 1:366,
#'     y = t(predicted.mat[1:(nrow(predicted.mat) - 4), ]),
#'     fdParobj = fdParobj
#'   )
#'
#' Jan <- c(1, 31); Feb <- c(31, 59); Mar <- c(59, 90)
#' Apr <- c(90, 120); May <- c(120, 151); Jun <- c(151, 181)
#' Jul <- c(181, 212); Aug <- c(212, 243); Sep <- c(243, 273)
#' Oct <- c(273, 304); Nov <- c(304, 334); Dec <- c(334, 365)
#'
#' intervals <-
#'   rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
#'
#' PULS4.pam <- PULS(toclust.fd = yfd.train$fd, intervals = intervals,
#'                   nclusters = 4, method = "pam")
#' PULS4.pam
PULS <- function(toclust.fd,
                 method = c("pam", "ward"),
                 intervals = c(0, 1),
                 spliton = NULL,
                 distmethod = c("usc", "manual"),
                 labels = toclust.fd$fdnames[2]$reps,
                 nclusters = length(toclust.fd$fdnames[2]$reps),
                 minbucket = 2,
                 minsplit = 4) {

  # Tan Tran, 6/28/17, check the distmethod arguments
  distmethod <- match.arg(distmethod)
  method <- match.arg(method)

  if (is.null(spliton)) spliton <- 1:nrow(intervals)
  #MG - 11/24/2014

  #Version of PULS that uses fda.usc distance functions under the hood for increased speed

  #Recommend that rows in intervals are named
  # Package dependencies

  ## Ensure that important options make sense
  if(minbucket >= minsplit)
    stop("minbucket must be less than minsplit")

  if(!fda::is.fd(toclust.fd))
    stop("fda's fd object must be provided containing functional data")

  ## Make variables that are simple derivatives of inputs that will be used a lot.
  labs <- labels
  nvars <- length(intervals[, 1])  #Now number of subregions so number of rows in intervals matrix - drop this?
  nobs <- length(labs)
  weights <- rep(1, nobs) #Could change this later but no reason to change weights in preliminary version

  members <- 1:nobs

  nsub <- nrow(intervals)

  #CREATE nxn by nsub distance matrices - will save time later to have all these done once and then subset each distance matrix:
  #Eventually replace with option to pass this information from external setup function so function can be re-run more quickly

  dsubs <- array(0, dim = c(nobs, nobs, nsub))

  #Works if the $fd version of the fd object is passed into the initial function
  for (j in 1:nsub){
    dsubs[, , j] = as.matrix(fdistmatrix(toclust.fd,
                                         subrange = intervals[j, ],
                                         distmethod))
  }

  # Checks that rows were named in the intervals matrix, if not it names them
  # by number of row
  if (any(is.null(rownames(intervals))))
    rownames(intervals) <- 1:length(intervals[, 1])

  dsubsnames <- rownames(intervals)

  Dist <- fdistmatrix(toclust.fd, subrange = range(intervals), distmethod)


  distmats <- matrix()

  ## Set up a vector containing each observation's membership. Put into global environment, but this will be deleted at the end
  ## of this function. Using global environment allows us to modify things recursively as we partition clusters.
  assign(".Cloc", rep(1, nobs), envir = .GlobalEnv)

  ## Likewise, set up the first (entire dataset) cluster in our Cluster frame where we keep track of each of the clusters and the
  ## partitioning.
  assign(".Cluster_frame",
         data.frame(number = 1,
                    var = "<leaf>",
                    n = nobs,
                    wt = sum(weights[members]),
                    inertia = inertiaD(Dist[members,members]),
                    bipartvar="NA",
                    bipartsplitrow = NA,
                    bipartsplitcol = NA,
                    inertiadel = 0,
                    yval = 1,
                    medoid = med(members, Dist),
                    category = NA,
                    cut = NA,
                    loc = 0.1,
                    stringsAsFactors = FALSE),
         envir = .GlobalEnv)


  # This loop runs until we have nclusters, have exhausted our observations or
  # run into our minsplit restriction (minbucket not easily controlled in PULS).
  while ((sum(.Cluster_frame$var == "<leaf>") < nclusters)) {
    # Passing the responses, the global distance matrix, the subreg distance
    # matrices, and names of intervals
    check <- checkem(toclust.fd, Dist, dsubs, dsubsnames, weights, minbucket,
                     minsplit, spliton, method)
    if (check == 0) { break }
  }


  # Most of the rest of the function does some bizarre text operations
  # the reason for this is because I stole a lot of code from rpart,
  # so we need to follow their text and labelling conventions to that our
  # objects which inherit from rpart can print and plot correctly.

  # Change the number column to rownames...
  rownames(.Cluster_frame) <- .Cluster_frame$number
  .Cluster_frame2 <- .Cluster_frame[, -1]

  # This is what will print at each terminal node on the dendrogram
  # (See plot.MonoClust).
  textfxn<-function(yval,dev,wt,ylevel,digits,n,meds,names, use.n){
    paste("\n  n=", n,"\n           M=",meds, sep="")
  }

  # Seperate categorical and quantitative splits as the text and plot functions
  # must treat them a bit differently
  var <- .Cluster_frame2$var
  cattog <- .Cluster_frame2$category

  splits <- which(var != '<leaf>')
  cat_splits <- which(var != '<leaf>' & cattog == 1)

  # Piece together a vector of labels to be printed. Kind of a weird way to do
  # this, but again, following rparts conventions, and we want to allow the user
  # to have options regarding how to print inequalities.
  ineq <- rep(c('<', '>='), length(splits))
  level <- .Cluster_frame2$cut[splits]
  level <- rep(level, each = 2)
  vars <- rep(var[splits], each = 2)
  labsnum <- c('root', paste(vars, ineq, level, sep = ' '))
  labs <- c('root',
            sapply(splits, getlevels, cats = cat_splits, varnames = var,
                   frame = .Cluster_frame2, catnames = catnames,
                   quali_ordered = quali_ordered))

  ## name a column what I probably should hav already named it, but I don't want to change all the code.
  colnames(.Cluster_frame2)[4] <- "dev"

  ## Reorder the columns so they print out nicely, again because I don't want to go back and change things.
  .Cluster_frame2 <- .Cluster_frame2[,c(1,12,2,3,4,5,6,7,8,9,10,11,13)]

  ## This follows somewhat odd rpart conventions.
  dendfxns<-list("text"=textfxn)

  # We will return a PULS object that also inherits from rpart with all of the neccesary components.
  rpartobj<-list("frame"=.Cluster_frame2,"labels"=labs,"labelsnum" = labsnum, "functions"=dendfxns,Membership =.Cloc, Dist=Dist)
  class(rpartobj)<-c("PULS","rpart")

  ## Get rid of our global assignments.
  rm(list = c(".Cluster_frame", ".Cloc"), envir = globalenv())

  return(rpartobj)

}


abbreviate <- function(string,abbrev){
  ## This function abbreviates the text. We want to handle categories, which rpart is not really prepared for
  ## so we use somewhat uneccesary regular expressions to get rid of unwanted characters in our categorical orderings.
  charvec <- strsplit(string," ",fixed=TRUE)[[1]]
  abbreviated <- sapply(charvec,function(x)if( x != "<" & x != ">=" & grepl("^\\s*[^0-9]",x,perl=TRUE) ) substr(x,1,abbrev)
                        else x)
  paste(abbreviated,collapse=" ")
}


getlevels <- function(sind,cats,varnames,frame,catnames,quali_ordered){
  ## A bit of a pain in the ass to get categorical ordering levels to print correctly.
  ## To be honest, I forgot what the last part here does, but I am certain it is neccesary.
  name <- varnames[sind]
  if(sind %in% cats == 0){
    level <- frame$cut[sind]
    labs <- c(paste(name,"<",level,sep=" "),paste(name,">=",level,sep=" "))
  } else{
    qualind <- which(catnames==varnames[sind])[1]
    toret <- c(paste(quali_ordered[[qualind]][1:(frame$cut[sind]-1)],collapse=" "),paste(quali_ordered[[qualind]][-c(1:(frame$cut[sind]-1))],collapse=" "))
    return(toret)
  }
}


splitter<-function(splitrow,toclust.fd,Dist,dsubs,dsubsname,weights,method){
  ## This function does the actual act of partitioning, given the row that is to be split "splitrow"

  number <- .Cluster_frame$number[splitrow]
  mems   <- which(.Cloc == number)
  split  <- c(.Cluster_frame$bipartsplitrow[splitrow],.Cluster_frame$bipartsplitcol[splitrow])

  Datamems<-toclust.fd[mems]

  #Might be able to pass old split from FindSplit but also can just re-do clustering based on members and subregion that was optimal
  #This will slow algorithm unnecessarily to redo clustering here with large data sets!

  # Tan, 7/6/17, add another choice of method, "ward".
  cursplit <- as.numeric()
  if (method == "pam") {
    # Generate variable to use for splitting based on subregion i
    cursplit <-
      cluster::pam(x = stats::as.dist(dsubs[mems, mems, dsubsname == split[2]]),
                   k = 2)$clustering
  } else {
    cursplit <-
      stats::cutree(
        stats::hclust(
          stats::as.dist(dsubs[mems, mems, dsubsname == split[2]]),
          method = "ward.D"),
        k = 2)
  }

  ifelse(meanmean.fd(Datamems[cursplit==1])<meanmean.fd(Datamems[cursplit==2]),
         cursplit<-as.numeric(cursplit==2),
         cursplit<-as.numeric(cursplit==1)) #Trying to create coding to partially sort the splits from small to large with 0s for smaller and 1 for larger means
  memsA<-mems[cursplit==0]; memsB<-setdiff(mems,memsA);


  ## Make the new clusters.
  Anum<-number*2
  Bnum<-number*2+1

  .Cloc[memsA]<<-Anum
  .Cloc[memsB]<<-Bnum

  variable <- split[2]#colnames(Data)[.Cluster_frame$bipartsplitcol[splitrow]]


  #Cutting this check from the more general MonoClust - could use this instead of making binary quant variables as well

  ## This seperates the categorical variable from the level.
  ## Probably bad coding, parsing strings over and over.
  #  if(grepl(variable,"*~*",fixed=TRUE)){
  #    variable <- strsplit(variable,"*~*",fixed=TRUE)[[1]]
  #  }


  ## Is the split categorical?
  #if(variable %in% catnames){
  #  .Cluster_frame[splitrow,12] <<- 1
  #}else { .Cluster_frame[splitrow,12] <<- 0 }

  nr<-nrow(.Cluster_frame)

  ## The old cluster now changes some attributes after splitting.
  .Cluster_frame[splitrow,2] <<- variable
  .Cluster_frame[splitrow,6] <<- variable
  .Cluster_frame[splitrow,13] <<- 1 #value of cut (or maybe 0?)

  ## New cluster 1 gets some new attributes
  .Cluster_frame[nr+1,1] <<- Anum
  .Cluster_frame[nr+1,2] <<- "<leaf>"
  .Cluster_frame[nr+1,3] <<- length(memsA)
  .Cluster_frame[nr+1,4] <<- sum(weights[memsA])
  .Cluster_frame[nr+1,5] <<- inertiaD(Dist[memsA,memsA])
  .Cluster_frame[nr+1,10] <<- 1-.Cluster_frame[splitrow,9]/.Cluster_frame[1,5]
  .Cluster_frame[nr+1,11] <<- med(memsA,Dist)
  .Cluster_frame[nr+1,14] <<- .Cluster_frame[splitrow,14] - 1/nr

  ## As does new cluster 2.
  .Cluster_frame[nr+2,1] <<- Bnum
  .Cluster_frame[nr+2,2] <<- "<leaf>"
  .Cluster_frame[nr+2,3] <<- length(memsB)
  .Cluster_frame[nr+2,4] <<- sum(weights[memsB])
  .Cluster_frame[nr+2,5] <<- inertiaD(Dist[memsB,memsB])
  .Cluster_frame[nr+2,10] <<- 1-.Cluster_frame[splitrow,9]/.Cluster_frame[1,5]
  .Cluster_frame[nr+2,11] <<- med(memsB,Dist)
  .Cluster_frame[nr+2,14] <<- .Cluster_frame[splitrow,14] + 1/nr
}


## We are keeping all of the information regarding which clusters we have in
## frame. At this point, we want to find the terminal node with the greatest change in inertia and bipartion it.

FindSplit <- function(frame,row,toclust.fd,Dist,dsubs,dsubsname,warn,weights,minbucket,minsplit,spliton,method){  #passes distance matrices and names of subregions - MG

  bycol<-numeric()

  number<-frame[row,1]
  mems<-which(.Cloc==number)
  Cutsmems<-matrix(0,nrow=length(mems),ncol=length(dsubsname))

  inertiap<-frame[row,5]

  if(inertiap == 0 | frame[row,3] == 1){return(0);}

  ## Subset the fd object
  Datamems<-toclust.fd[mems]

  #Cutsmems<-Cuts[mems,] #Generate from clustering only for those subjects candidates for being split


  #Create potential splits by doing 2-group clustering of available data in each subinterval

  ##For each subregion generate possible cut, calculate the inertia.

  # Tan 6/29/17, modified to split on a restricted set of intervals
  # for(i in (1:length(dsubsname))){
  for(i in spliton){

    # Tan 9/29, rewrite to make it easier to read
    # If #obs satisfies minsplit then split further, otherwise not bother to do anything
    # Check for minsplit could occur earlier, but want to replace inertia change with NAs so the search will look elsewhere
    if (length(mems) >= minsplit) {
      # Tan, 7/6/17, add another choice of method, "ward".
      pot.split.unsorted <- as.numeric()
      if (method == "pam") {
        pot.split.unsorted <- pam(x=as.dist(dsubs[mems,mems,i]),k=2)$clustering
      } else if (method == "ward") {
        pot.split.unsorted<-cutree(hclust(as.dist(dsubs[mems,mems,i]), method="ward.D"),
                         k=2)
      }

      #Trying to create coding to partially sort the splits from small to large with 0s for smaller and 1 for larger means
      ifelse(meanmean.fd(Datamems[pot.split.unsorted==1]) < meanmean.fd(Datamems[pot.split.unsorted==2]),
             pot.split<-as.numeric(pot.split.unsorted==2),
             pot.split<-as.numeric(pot.split.unsorted==1))

      Cutsmems[,i] <- Cuts_col <- pot.split #Keeps splits from each subregion
      memsA<-mems[which(Cuts_col==0)]; memsB<-setdiff(mems,memsA);
      #Turn inertia change to NA if min size of either group is smaller than minbucket or total size is smaller than minsplit- should allow other splits that don't violate condition to be selected from other subregions or leaves
      ifelse((min(length(memsA),length(memsB))>=minbucket),bycol<-cbind(bycol,inertiaD(Dist[memsA,memsA]) + inertiaD(Dist[memsB,memsB])),bycol<-cbind(bycol, NA))
    } else {bycol<-cbind(bycol, NA)}
  }
  # Difference between current cluster and the possible splits
  vals <- inertiap - bycol
  ## Say no diference if we have NA or infinite (happens when no split is possible)
  vals[!is.finite(vals) | is.na(vals)] <- 0

  ## This is the best split.
  maxval<-max(vals)

  ## This is the maximum inertia change indep
  ind <- which((inertiap - bycol) == maxval,arr.ind=TRUE)
  if(maxval==0){ind <- which(is.na(inertiap - bycol),arr.ind=TRUE)} #Creates a problem if all potential splits are NA for desired depth of tree - fix later


  ## If multiple splits produce the same inertia change output a warning.
  if(nrow(ind) > 1 & .MonoClustwarn==0){.MonoClustwarn <<- 1; warning("One or more of the splits chosen had an alternative split that reduced deviance by the same amount.")}
  split<-ind[1,]

  memsA<-mems[Cutsmems[,spliton[split[2]]]==0]; memsB<-setdiff(mems,memsA);

  # calculate our change in inertia
  inertiadel <- inertiap - inertiaD(Dist[memsA,memsA]) - inertiaD(Dist[memsB,memsB])

  ## Update our frame
  frame[row,7] <- spliton[split[1]]
  frame[row,8] <- dsubsname[spliton[split[2]]] #Put the name of the subregion used for the split OR maybe use bipartvar as slot for name of split?
  frame[row,9] <- inertiadel
  .Cluster_frame<<-frame
}


checkem<-function(toclust.fd,Dist, dsubs,dsubsname,weights,minbucket,minsplit,spliton,method){

  ## Current terminal nodes
  candidates<-which(.Cluster_frame$var == '<leaf>' & is.na(.Cluster_frame$bipartsplitrow))
  ## Split the best one. Return to Nada which never gets output.
  Nada <- sapply(candidates,function(x)FindSplit(.Cluster_frame,x,toclust.fd,Dist, dsubs,dsubsname,warn,weights,minbucket,minsplit,spliton,method))

  ## See which ones are left.
  candidates2 <- which(.Cluster_frame$var == '<leaf>')
  ## If nothing's left, stop running.
  if(length(candidates2)==0){return(0)}

  ## Find the best inertia change of all that are possible
  maxone <- max(.Cluster_frame$inertiadel[candidates2],na.rm=TRUE)
  splitrow<-candidates2[which(.Cluster_frame$inertiadel[candidates2]==maxone)]

  ## Make new clusters from that cluster
  splitter(splitrow, toclust.fd,Dist,dsubs,dsubsname,weights, method)


}


## Calculate inertia for a given subset of the data from a distance matrix.
inertiaD <- function(X){
  if (!is.matrix(X)) return(0) else
    if (dim(X)[1]==0) return(0) else
      if (!is.matrix(X)) return(X) else
        return(sum(X^2)/(dim(X)[1]*2))
}

## Find medoid of the cluster.
med <- function(members,Dist){
  if(length(members)==1){return(members)}
  else{
    if(length(members)==0){return(0)}
    dists<-apply(Dist[members,members],1,sum)
    medoid<-members[which(dists==min(dists))]
    return(medoid[1])
  }
}

#Eventually modify to allow unions of subintervals to be defined
fdistmatrix <- function(yfd, subrange, distmethod) {
  N=length(yfd$fdnames$reps)
  #Convert to an fdata object evaluated just over the range defined in subrange:
  Ydist=matrix(0,nrow=N,ncol=N)

  if (distmethod == "usc") {
    nargs=length(yfd$basis$params+2)*5 #Create a higher resolution grid to predict and then remake fdata objects on reduced domains
    t_high=seq(from=subrange[1],to=subrange[2],length.out=nargs)

    predfd=predict(yfd,t_high)
    yfdata=fdata(mdata=t(predfd),argvals=t_high)

    Ydist=as.matrix(as.dist(metric.lp(yfdata),diag=T,upper=T))
  } else {
    for (j1 in 1:(N-1)){
      fdfirst=yfd[j1]
      for (i1 in (j1+1):N){
        fdsecond=yfd[i1]
        diff1=minus.fd(fdfirst,fdsecond)
        Ydist[j1,i1]=sqrt(inprod(diff1,diff1,rng=subrange));
      }
    }

    Ydist=as.matrix(as.dist(t(Ydist),upper=T,diag=T))
  }

  return(Ydist)
}


meanmean.fd <- function(yfd) {
  mfd = mean(predict(mean(yfd)))
  return(mfd)
}

