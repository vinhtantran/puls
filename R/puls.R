#' Partitioning Using Local Subregions (PULS)
#'
#' PULS function for functional data (only used when you know that the data
#' shouldn't be converted into functional because it's already smooth, e.g.
#' your data are step function)
#'
#' @param toclust.fd A functional data object (i.e., having class `fd`) created
#'   from `fda` package. See [fda::fd()].
#' @param method The clustering method you want to run in each subregion. Can be
#'   chosen between `pam` and `ward`.
#' @param intervals A data set (or matrix) with rows are intervals and columns
#'   are the beginning and ending indexes of of the interval.
#' @param spliton Restrict the partitioning on a specific set of subregions.
#' @param distmethod The method for calculating the distance matrix. Choose
#'   between `"usc"` and `"manual"`. `"usc"` uses [fda.usc::metric.lp()]
#'   function while `"manual"` uses squared distance between functions. See
#'   [@details].
#' @param labels The name of entities.
#' @param nclusters The number of clusters.
#' @param minbucket The minimum number of data points in one cluster allowed.
#' @param minsplit The minimum size of a cluster that can still be considered to
#'   be a split candidate.
#'
#' @details
#' If choosing `distmethod = "manual"`, the L2 distance between all pairs of
#'   functions \eqn{y_i(t)} and \eqn{y_j(t)} is given by:
#' \deqn{d_R(y_i, y_j) = \sqrt{\int_{a_r}^{b_r} [y_i(t) - y_j(t)]^2 dt}.}
#'
#' @seealso [fda::is.fd()]
#'
#' @export
#'
#' @return A `PULS` object. See [PULS.object] for details.
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
#' PULS4_pam <- PULS(toclust.fd = yfd$fd, intervals = intervals,
#'                   nclusters = 4, method = "pam")
#' PULS4_pam
#' }
PULS <- function(toclust.fd,
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
    stop("\"toclust.fd\" must be a functional data object (fda::is.fd).")

  if (minbucket >= minsplit) {
    stop("\"minbucket\" must be less than \"minsplit\".")
  }

  method <- match.arg(method)
  distmethod <- match.arg(distmethod)

  if (is.null(spliton))
    spliton <- seq_len(nrow(intervals))

  # Make variables that are simple derivatives of inputs that will be used a lot
  nobs <- length(labels)
  # No reason to change weights in preliminary version
  weights <- rep(1, nobs)
  members <- 1:nobs
  nsub <- nrow(intervals)

  # Create n x n x nsub distance matrices
  dsubs <- array(0, dim = c(nobs, nobs, nsub))

  for (j in 1:nsub) {
    dsubs[, , j] <- as.matrix(fdistmatrix(toclust.fd,
                                          subrange = intervals[j, ],
                                          distmethod))
  }

  # Checks that rows were named in the intervals matrix, if not it names them
  # by number of row
  if (any(is.null(rownames(intervals))))
    rownames(intervals) <- seq_len(length(intervals[, 1]))

  dsubsnames <- rownames(intervals)

  dist <- fdistmatrix(toclust.fd, subrange = range(intervals), distmethod)

  # Set up a vector containing each observation's membership
  cloc <- rep(1, nobs)

  # Set up the first cluster in cluster_frame
  cluster_frame <-
    new_node(number = 1,
             var = "<leaf>",
             n = nobs,
             wt = sum(weights[members]),
             inertia = monoClust::inertia_calc(dist[members, members]),
             inertia_explained = 0,
             medoid = monoClust::medoid(members, dist),
             loc = 0.1)

  done_running <- FALSE
  # This loop runs until we have nclusters, have exhausted our observations or
  # run into our minsplit restriction (minbucket not easily controlled in PULS).
  while ((sum(cluster_frame$var == "<leaf>") < nclusters && !done_running)) {

    checkem_ret <- checkem(toclust.fd, cluster_frame, cloc, dist, dsubs,
                           dsubsnames, weights, minbucket, minsplit, spliton,
                           method)

    if (!identical(cloc, checkem_ret$cloc)) {
      cluster_frame <- checkem_ret$frame
    } else {
      done_running <- TRUE
    }

    cloc <- checkem_ret$cloc
  }

  # For display purpose, all -99 is turned to NA
  cluster_frame <-
    replace(cluster_frame,
            cluster_frame == -99,
            NA)

  # Add medoids of each cluster
  medoids <- cluster_frame$medoid[cluster_frame$var == "<leaf>"]
  names(medoids) <- cluster_frame$number[cluster_frame$var == "<leaf>"]

  # Whether there exists an alternate splitting route
  alt <- any(cluster_frame$alt)

  puls_obj <- list(frame = cluster_frame,
                   membership = cloc,
                   dist = dist,
                   # Add terms to keep track of subregion name
                   terms = dsubsnames,
                   medoids = medoids,
                   alt = alt)

  class(puls_obj) <- "PULS"

  return(puls_obj)
}

#' Split Function
#'
#' Given the Cluster's frame's row position to split at `split_row`, this
#' function performs the split, calculate all necessary information for the
#' splitting tree and cluster memberships.
#'
#' @param split_row The row index in frame that would be split on.
#' @inheritParams checkem
#'
#' @return Updated `frame` and `cloc` saved in a list.
#'
#' @keywords internal
splitter <- function(toclust.fd, split_row, frame, cloc, dist, dsubs, dsubsname,
                     weights, method) {

  number <- frame$number[split_row]
  mems <- which(cloc == number)
  split <- c(frame$bipartsplitrow[split_row],
             frame$bipartsplitcol[split_row])

  data_mems <- toclust.fd[mems]

  # Generate variable to use for splitting based on subregion i
  if (method == "pam") {
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

  # Partially sort the splits with 0 for smaller and 1 for larger means
  if (mean_fd(data_mems[cursplit == 1]) < mean_fd(data_mems[cursplit == 2])) {
    cursplit <- as.numeric(cursplit == 2)
  } else {
    cursplit <- as.numeric(cursplit == 1)
  }

  mems_a <- mems[cursplit == 0]
  mems_b <- setdiff(mems, mems_a)

  # Make the new clusters.
  num_a <- number * 2
  num_b <- number * 2 + 1

  cloc[mems_a] <- num_a
  cloc[mems_b] <- num_b

  # The old cluster now changes some attributes after splitting.
  frame$var[split_row]       <- dsubsname[split[2]]

  ## New cluster 1 gets some new attributes
  node_a <-
    new_node(
      number  = num_a,
      var     = "<leaf>",
      n       = length(mems_a),
      wt      = sum(weights[mems_a]),
      inertia = monoClust::inertia_calc(dist[mems_a, mems_a]),
      medoid  = monoClust::medoid(mems_a, dist),
      loc     = frame$loc[split_row] - 1 / nrow(frame)
    )

  ## As does new cluster 2.
  node_b <-
    new_node(
      number  = num_b,
      var     = "<leaf>",
      n       = length(mems_b),
      wt      = sum(weights[mems_b]),
      inertia = monoClust::inertia_calc(dist[mems_b, mems_b]),
      medoid  = monoClust::medoid(mems_b, dist),
      loc     = frame$loc[split_row] + 1 / nrow(frame)
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

#' Find the Best Split
#'
#' Find the best split in terms of reduction in inertia for the transferred
#' node, indicate by row. Find the terminal node with the greatest change in
#' inertia and bi-partition it.
#'
#' @param frame_row One row of the split tree as data frame.
#' @inheritParams checkem
#'
#' @return The updated `frame_row` with the next split updated.
#' @keywords internal
find_split <- function(toclust.fd, frame_row, cloc, dist, dsubs, dsubsname,
                       weights, minbucket, minsplit, spliton, method) {

  bycol <- numeric()

  number <- frame_row$number
  mems <- which(cloc == number)
  inertiap <- frame_row$inertia

  if (inertiap == 0L || frame_row$n < minsplit || frame_row$n == 1L) {
    frame_row$bipartsplitrow <- 0L
    return(frame_row)
  }

  # Subset the fd object
  data_mems <- toclust.fd[mems]
  cuts_mems <- matrix(0, nrow = length(mems), ncol = length(dsubsname))

  # Create potential splits by doing 2-group clustering of available data in
  # each subinterval. For each subregion generate possible cut, calculate the
  # inertia.
  for (i in spliton) {

    # If satisfies minsplit then split further, otherwise skip
    # Check for minsplit could occur earlier, but want to replace inertia change
    # with NAs so the search will look elsewhere
    if (length(mems) >= minsplit) {
      if (method == "pam") {
        pot_split_unsorted <-
          cluster::pam(x = stats::as.dist(dsubs[mems, mems, i]),
                       k = 2)$clustering
      } else if (method == "ward") {
        pot_split_unsorted <-
          stats::cutree(stats::hclust(stats::as.dist(dsubs[mems, mems, i]),
                                      method = "ward.D"),
                        k = 2)
      }

      # Create coding to partially sort the splits from small to large
      if (mean_fd(data_mems[pot_split_unsorted == 1]) <
          mean_fd(data_mems[pot_split_unsorted == 2])) {
        pot_split <- as.numeric(pot_split_unsorted == 2)
      } else {
        pot_split <- as.numeric(pot_split_unsorted == 1)
      }

      # Keeps splits from each subregion
      cuts_mems[, i] <- cuts_col <- pot_split

      mems_a <- mems[which(cuts_col == 0)]
      mems_b <- setdiff(mems, mems_a)

      # Turn inertia change to NA if min size of either group is smaller than
      # minbucket or total size is smaller than minsplit
      if (min(length(mems_a), length(mems_b)) >= minbucket) {
        bycol <- cbind(bycol, monoClust::inertia_calc(dist[mems_a, mems_a]) +
                         monoClust::inertia_calc(dist[mems_b, mems_b]))
      } else {
        bycol <- cbind(bycol, NA)
      }
    } else {
      bycol <- cbind(bycol, NA)
    }
  }

  # Difference between current cluster and the possible splits
  vals <- inertiap - bycol
  # No diference if we have NA or infinite (happens when no split is possible)
  vals[!is.finite(vals) | is.na(vals)] <- 0L

  # This is the best split
  maxval <- max(vals)

  # This is the maximum inertia change index
  ind <- which((inertiap - bycol) == maxval, arr.ind = TRUE)

  # Create a problem if all potential splits are NA for desired depth of tree
  if (maxval == 0) {
    ind <- which(is.na(inertiap - bycol), arr.ind = TRUE)
  }

  # If multiple splits produce the same inertia change output a warning.
  if (nrow(ind) > 1) {
    frame_row$alt <- TRUE
  }

  split <- ind[1, ]

  mems_a <- mems[cuts_mems[, spliton[split[2]]] == 0]
  mems_b <- setdiff(mems, mems_a)

  # Calculate change in inertia
  inertiadel <- inertiap -
    monoClust::inertia_calc(dist[mems_a, mems_a]) -
    monoClust::inertia_calc(dist[mems_b, mems_b])

  ## Update frame
  frame_row$bipartsplitrow <- spliton[split[1]]
  frame_row$bipartsplitcol <- spliton[split[2]]
  frame_row$inertiadel <- inertiadel

  return(frame_row)
}

#' First Gate Function
#'
#' This function checks what are available nodes to split and then call
#' `find_split()` on each node, then decide which node creates best split, and
#' call `splitter()` to perform the split.
#'
#' @param frame The split tree transferred as data frame.
#' @param cloc Vector of current cluster membership.
#' @param dist Distance matrix of all observations in the data.
#' @param dsubs Distance matrix calculated on each subregion. A
#'   three-dimensional matrix.
#' @param dsubsname Subregion names.
#' @param weights (Currentlt unused) Weights on observations.
#' @param minsplit The minimum number of observations that must exist in a node
#'   in order for a split to be attempted.
#' @inheritParams PULS
#'
#' @return It is not supposed to return anything because global environment was
#'   used. However, if there is nothing left to split, it returns 0 to tell the
#'   caller to stop running the loop.
#' @keywords internal
checkem <- function(toclust.fd, frame, cloc, dist, dsubs, dsubsname, weights,
                    minbucket, minsplit, spliton, method) {

  # Current terminal nodes
  candidates <- which(frame$var == "<leaf>" &
                        frame$bipartsplitrow == -99L)
  # Split the best one
  frame[candidates, ] <-
    purrr::map_dfr(
      candidates,
      ~ find_split(toclust.fd, frame[.x, ], cloc, dist, dsubs,
                   dsubsname, weights, minbucket, minsplit, spliton,
                   method))

  # See which ones are left
  candidates2 <- which(frame$var == "<leaf>" & frame$bipartsplitrow != 0L)
  # If there is something left, call splitter
  if (length(candidates2) > 0L) {
    # Find the best inertia change of all that are possible
    split_row <- candidates2[which.max(frame$inertiadel[candidates2])]

    ## Make new clusters from that cluster
    splitter_ret <-
      splitter(toclust.fd, split_row, frame, cloc, dist, dsubs, dsubsname,
               weights, method)

    frame <- splitter_ret$frame
    cloc <- splitter_ret$cloc
  }

  return(list(frame = frame, cloc = cloc))
}
