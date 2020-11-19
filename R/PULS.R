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
#'   between `usc` and `manual`.
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
#' \donttest{
#' library(fda)
#'
#' # Build a simple fd object from already smoothed smoothed_arctic
#' data(smoothed_arctic)
#' NBASIS <- 300
#' NORDER <- 4
#' y <- t(as.matrix(smoothed_arctic[, -1]))
#' splinebasis <- create.bspline.basis(rangeval = c(1, 365),
#'                                     nbasis = NBASIS,
#'                                     norder = NORDER)
#' fdParobj <- fdPar(fdobj = splinebasis,
#'                   Lfdobj = 2,
#'                   # No need for any more smoothing
#'                   lambda = .000001)
#' yfd <- smooth.basis(argvals = 1:365, y = y, fdParobj = fdParobj)
#'
#' Jan <- c(1, 31); Feb <- c(31, 59); Mar <- c(59, 90)
#' Apr <- c(90, 120); May <- c(120, 151); Jun <- c(151, 181)
#' Jul <- c(181, 212); Aug <- c(212, 243); Sep <- c(243, 273)
#' Oct <- c(273, 304); Nov <- c(304, 334); Dec <- c(334, 365)
#'
#' intervals <-
#'   rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
#'
#' PULS4_pam <- puls(toclust.fd = yfd$fd, intervals = intervals,
#'                   nclusters = 4, method = "pam")
#' PULS4_pam
#' }
puls <- function(toclust.fd,
                 method = c("pam", "ward"),
                 intervals = c(0, 1),
                 spliton = NULL,
                 distmethod = c("usc", "manual"),
                 labels = toclust.fd$fdnames[2]$reps,
                 nclusters = length(toclust.fd$fdnames[2]$reps),
                 minbucket = 2,
                 minsplit = 4) {

  ## Ensure that important options make sense
  if (!fda::is.fd(toclust.fd))
    stop("\"toclust.fd\" must be fda's fd object")

  if (minbucket >= minsplit) {
    stop("\"minbucket\" must be less than \"minsplit\".")
  }

  method <- match.arg(method)
  distmethod <- match.arg(distmethod)

  if (is.null(spliton))
    spliton <- 1:nrow(intervals)

  ## Make variables that are simple derivatives of inputs that will be used a lot.
  # labs <- labels
  # nvars <- length(intervals[, 1])  #Now number of subregions so number of rows in intervals matrix - drop this?
  nobs <- length(labels)
  # No reason to change weights in preliminary version
  weights <- rep(1, nobs)
  members <- 1:nobs
  nsub <- nrow(intervals)

  # Create n x n x nsub distance matrices
  dsubs <- array(0, dim = c(nobs, nobs, nsub))

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


  # distmats <- matrix()

  ## Set up a vector containing each observation's membership
  cloc <- rep(1, nobs)

  ## Likewise, set up the first (entire dataset) cluster in our Cluster frame where we keep track of each of the clusters and the
  ## partitioning.
  cluster_frame <- new_node(number = 1,
                            var = "<leaf>",
                            n = nobs,
                            wt = sum(weights[members]),
                            inertia = inertiaD(Dist[members,members]),
                            inertia_explained = 0,
                            yval = 1,
                            medoid = med(members, Dist),
                            loc = 0.1)


  done_running <- FALSE
  # This loop runs until we have nclusters, have exhausted our observations or
  # run into our minsplit restriction (minbucket not easily controlled in PULS).
  while ((sum(cluster_frame$var == "<leaf>") < nclusters && !done_running)) {
    # Passing the responses, the global distance matrix, the subreg distance
    # matrices, and names of intervals
    checkem_ret <- checkem(toclust.fd, cluster_frame, cloc, Dist, dsubs, dsubsnames, weights, minbucket,
                     minsplit, spliton, method)

    if (!identical(cloc, checkem_ret$cloc)) {
      cluster_frame <- checkem_ret$frame
    } else {
      done_running <- TRUE
    }

    cloc <- checkem_ret$cloc
  }


  # Most of the rest of the function does some bizarre text operations
  # the reason for this is because I stole a lot of code from rpart,
  # so we need to follow their text and labelling conventions to that our
  # objects which inherit from rpart can print and plot correctly.

  # Change the number column to rownames...
  # rownames(.Cluster_frame) <- .Cluster_frame$number
  # .Cluster_frame2 <- .Cluster_frame[, -1]

  # This is what will print at each terminal node on the dendrogram
  # (See plot.MonoClust).
  textfxn<-function(yval,dev,wt,ylevel,digits,n,meds,names, use.n){
    paste("\n  n=", n,"\n           M=",meds, sep="")
  }

  # Seperate categorical and quantitative splits as the text and plot functions
  # must treat them a bit differently
  var <- cluster_frame$var
  # cattog <- cluster_frame$category

  splits <- which(var != '<leaf>')
  # cat_splits <- which(var != '<leaf>' & cattog == 1)

  # Piece together a vector of labels to be printed. Kind of a weird way to do
  # this, but again, following rparts conventions, and we want to allow the user
  # to have options regarding how to print inequalities.
  ineq <- rep(c('<', '>='), length(splits))
  level <- cluster_frame$cut[splits]
  level <- rep(level, each = 2)
  vars <- rep(var[splits], each = 2)
  labsnum <- c('root', paste(vars, ineq, level, sep = ' '))
  # labs <- c('root',
  #           sapply(splits, getlevels, varnames = var,
  #                  frame = cluster_frame, catnames = catnames))

  ## name a column what I probably should hav already named it, but I don't want to change all the code.
  # colnames(cluster_frame)[4] <- "dev"

  ## Reorder the columns so they print out nicely, again because I don't want to go back and change things.
  # cluster_frame <- cluster_frame[,c(1,12,2,3,4,5,6,7,8,9,10,11,13)]

  ## This follows somewhat odd rpart conventions.
  # dendfxns<-list("text"=textfxn)

  # For display purpose, all -99 is turned to NA
  cluster_frame <-
    replace(cluster_frame,
            cluster_frame == -99,
            NA)

  # We will return a PULS object that also inherits from rpart with all of the neccesary components.
  puls_obj <-list("frame"=cluster_frame,"labelsnum" = labsnum,Membership =cloc, Dist=Dist)
  class(puls_obj)<-c("PULS","rpart")

  return(puls_obj)

}


abbreviate <- function(string,abbrev){
  ## This function abbreviates the text. We want to handle categories, which rpart is not really prepared for
  ## so we use somewhat uneccesary regular expressions to get rid of unwanted characters in our categorical orderings.
  charvec <- strsplit(string," ",fixed=TRUE)[[1]]
  abbreviated <- sapply(charvec,function(x)if( x != "<" & x != ">=" & grepl("^\\s*[^0-9]",x,perl=TRUE) ) substr(x,1,abbrev)
                        else x)
  paste(abbreviated,collapse=" ")
}


getlevels <- function(sind,varnames,frame){
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


splitter<-function(toclust.fd,split_row,frame,cloc,Dist,dsubs,dsubsname,weights,method){
  ## This function does the actual act of partitioning, given the row that is to be split "split_row"

  number <- frame$number[split_row]
  mems <- which(cloc == number)
  split  <- c(frame$bipartsplitrow[split_row],
              frame$bipartsplitcol[split_row])

  Datamems<-toclust.fd[mems]

  #Might be able to pass old split from FindSplit but also can just re-do clustering based on members and subregion that was optimal
  #This will slow algorithm unnecessarily to redo clustering here with large data sets!

  # Tan, 7/6/17, add another choice of method, "ward".
  cursplit <- as.numeric()
  if (method == "pam") {
    # Generate variable to use for splitting based on subregion i
    cursplit <-
      cluster::pam(x = stats::as.dist(dsubs[mems, mems, split[2]]),
                   k = 2)$clustering
  } else {
    cursplit <-
      stats::cutree(
        stats::hclust(
          stats::as.dist(dsubs[mems, mems, split[2]]),
          method = "ward.D"),
        k = 2)
  }

  # Trying to create coding to partially sort the splits from small to large with 0s for smaller and 1 for larger means
  if (meanmean.fd(Datamems[cursplit==1]) < meanmean.fd(Datamems[cursplit==2])) {
    cursplit <- as.numeric(cursplit==2)
  } else {
    cursplit <- as.numeric(cursplit==1)
  }

  memsA <- mems[cursplit==0]
  memsB <- setdiff(mems,memsA)


  ## Make the new clusters.
  Anum<-number*2
  Bnum<-number*2+1

  cloc[memsA] <- Anum
  cloc[memsB] <- Bnum

  # variable <- split[2]

  # nr <- nrow(frame)

  ## The old cluster now changes some attributes after splitting.
  frame$var[split_row] <- dsubsname[split[2]]
  frame$bipartvar[split_row] <- dsubsname[split[2]]
  frame$cut[split_row] <- 1 #value of cut (or maybe 0?)

  ## New cluster 1 gets some new attributes
  node_a <-
    new_node(
      number = Anum,
      var = "<leaf>",
      n = length(memsA),
      wt = sum(weights[memsA]),
      inertia = inertiaD(Dist[memsA,memsA]),
      # ?
      yval = 1-frame$inertiadel[split_row]/frame$inertia[1],
      medoid = med(memsA,Dist),
      loc = frame$loc[split_row] - 1/nrow(frame)
    )

  ## As does new cluster 2.
  node_b <-
    new_node(
      number = Bnum,
      var = "<leaf>",
      n = length(memsB),
      wt = sum(weights[memsB]),
      inertia = inertiaD(Dist[memsB,memsB]),
      # ?
      yval = 1-frame$inertiadel[split_row]/frame$inertia[1],
      medoid = med(memsB,Dist),
      loc = frame$loc[split_row] + 1/nrow(frame)
    )

  # Insert two new rows right after split row
  frame <- tibble::add_row(frame,
                           tibble::add_row(node_a, node_b),
                           .after = split_row)

  # This has to be updated last because it needs leaf nodes list
  # See Chavent (2007) for definition. Basically,
  # 1 - (sum(current inertia)/inertia[1])
  frame$inertia_explained[split_row] <-
    1 - sum(frame$inertia[frame$var == "<leaf>"]) / frame$inertia[1]

  return(list(frame = frame, cloc = cloc))
}


## We are keeping all of the information regarding which clusters we have in
## frame. At this point, we want to find the terminal node with the greatest change in inertia and bipartion it.

FindSplit <- function(toclust.fd,frame_row,cloc,Dist,dsubs,dsubsname,warn,weights,minbucket,minsplit,spliton,method){  #passes distance matrices and names of subregions - MG

  bycol<-numeric()

  number<-frame_row$number
  mems<-which(cloc==number)
  Cutsmems<-matrix(0,nrow=length(mems),ncol=length(dsubsname))

  inertiap<-frame_row$inertia

  if(inertiap == 0 || frame_row$n == 1) {
    frame_row$bipartsplitrow <- 0L
    return(frame_row)
  }

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
        pot.split.unsorted <- cluster::pam(x=stats::as.dist(dsubs[mems,mems,i]),k=2)$clustering
      } else if (method == "ward") {
        pot.split.unsorted<-stats::cutree(stats::hclust(stats::as.dist(dsubs[mems,mems,i]), method="ward.D"),
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
  if(nrow(ind) > 1) {
    warning("One or more of the splits chosen had an alternative split that reduced deviance by the same amount.")
  }
  split<-ind[1,]

  memsA<-mems[Cutsmems[,spliton[split[2]]]==0]; memsB<-setdiff(mems,memsA);

  # calculate our change in inertia
  inertiadel <- inertiap - inertiaD(Dist[memsA,memsA]) - inertiaD(Dist[memsB,memsB])

  ## Update our frame
  frame_row$bipartsplitrow <- spliton[split[1]]
  frame_row$bipartsplitcol <- spliton[split[2]]
  frame_row$inertiadel <- inertiadel

  return(frame_row)
}


checkem <- function(toclust.fd, frame, cloc, Dist, dsubs, dsubsname, weights, minbucket,
                    minsplit, spliton, method){

  ## Current terminal nodes
  candidates <- which(frame$var == "<leaf>" &
                        frame$bipartsplitrow == -99L)

  frame[candidates, ] <-
    purrr::map_dfr(candidates,
                   ~ FindSplit(toclust.fd, frame[.x, ], cloc, Dist, dsubs,
                               dsubsname, warn, weights, minbucket, minsplit,
                               spliton, method))

  ## See which ones are left
  candidates2 <- which(frame$var == "<leaf>")
  ## If there is something left, call splitter
  if (length(candidates2) > 0) {

    ## Find the best inertia change of all that are possible
    maxone <- max(frame$inertiadel[candidates2], na.rm = TRUE)
    split_row<-candidates2[which(frame$inertiadel[candidates2] == maxone)]

    ## Make new clusters from that cluster
    splitter_ret <-
      splitter(toclust.fd,split_row, frame, cloc, Dist,dsubs,dsubsname,weights, method)

    frame <- splitter_ret$frame
    cloc <- splitter_ret$cloc
  }

  return(list(frame = frame, cloc = cloc))


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

meanmean.fd <- function(yfd) {
  mfd = mean(fda::predict.fd(fda::mean.fd(yfd)))
  return(mfd)
}

