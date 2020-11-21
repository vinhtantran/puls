#' PULS Tree Object
#'
#' The structure and objects contained in PULS, an object returned from
#' the [puls()] function and used as the input in other functions in the
#' package.
#'
#' @name PULS.object
#'
#' @return
#' \describe{
#'   \item{frame}{Data frame in the form of a [tibble::tibble()] representing
#'     a tree structure with one row for each node. The columns include:
#'     \describe{
#'       \item{number}{Index of the node. Depth of a node can be derived by
#'         `number %/% 2`.}
#'       \item{var}{Name of the variable used in the split at a node or
#'         `"<leaf>"` if it is a leaf node.}
#'       \item{n}{Cluster size, the number of observations in that cluster.}
#'       \item{wt}{Weights of observations. Unusable. Saved for future use.}
#'       \item{inertia}{Inertia value of the cluster at that node.}
#'       \item{bipartsplitrow}{Position of the next split row in the data set
#'         (that position will belong to left node (smaller)).}
#'       \item{bipartsplitcol}{Position of the next split variable in the data
#'         set.}
#'       \item{inertiadel}{Proportion of inertia value of the cluster at that
#'         node to the inertia of the root.}
#'       \item{medoid}{Position of the data point regarded as the medoid of
#'         its cluster.}
#'       \item{loc}{y-coordinate of the splitting node to facilitate showing
#'         on the tree. See [plot.PULS()] for details.}
#'       \item{inertia_explained}{Percent inertia explained as described in
#'         Chavent (2007). It is `1 - (sum(current inertia)/inertial[1])`.}
#'       \item{alt}{Indicator of an alternative cut yielding the same reduction
#'         in inertia at that split.}
#'     }}
#'   \item{membership}{Vector of the same length as the number of rows in the
#'     data, containing the value of `frame$number` corresponding to the leaf
#'     node that an observation falls into.}
#'   \item{dist}{Distance matrix calculated using the method indicated in
#'     `distmethod` argument of [puls()].}
#'   \item{terms}{Vector of subregion names in the data that were used to
#'     split.}
#'   \item{medoids}{Named vector of positions of the data points regarded as
#'     medoids of clusters.}
#'   \item{alt}{Indicator of having an alternate splitting route occurred when
#'     splitting.}
#' }
#'
#' @references
#' * Chavent, M., Lechevallier, Y., & Briant, O. (2007). DIVCLUS-T: A monothetic
#' divisive hierarchical clustering method. Computational Statistics & Data
#' Analysis, 52(2), 687-701. \doi{10.1016/j.csda.2007.03.013}.
#'
#' @seealso [puls()].
NULL

#' Distance between Functional Objects
#'
#' Calculate the distance between functional objects over the defined range.
#'
#' @param yfd A functional data object as the result of `fda` package.
#' @param subrange A vector of two values indicating the value range of
#'   functional object to calculate on.
#' @inheritParams puls
#'
#' @return A distance matrix with diagonal value and the upper half.
#' @export
#'
#' @examples
#' library(fda)
#' # Examples taken from fda::Data2fd()
#' data(gait)
#' # Function only works on two dimensional data
#' gait <- gait[, , 1]
#' gaitbasis3 <- create.fourier.basis(nbasis = 5)
#' gaitfd3 <- Data2fd(gait, basisobj = gaitbasis3)
#'
#' fdistmatrix(gaitfd3, c(0.2, 0.4), "usc")
fdistmatrix <- function(yfd, subrange, distmethod) {
  n <- length(yfd$fdnames$reps)
  y_dist <- matrix(0, nrow = n, ncol = n)

  if (distmethod == "usc") {
    # Create a higher resolution grid to predict and then remake fdata objects
    # on reduced domains
    nargs <- length(yfd$basis$params + 2) * 5
    t_high <- seq(from = subrange[1], to = subrange[2], length.out = nargs)

    predfd <- fda::predict.fd(yfd, t_high)
    yfdata <- fda.usc::fdata(mdata = t(predfd), argvals = t_high)

    y_dist <- as.matrix(stats::as.dist(fda.usc::metric.lp(yfdata),
                                       diag = T,
                                       upper = T))
  } else {
    for (j1 in seq_len(n - 1)) {
      fdfirst <- yfd[j1]
      for (i1 in (j1 + 1):n) {
        fdsecond <- yfd[i1]
        diff1 <- fda::minus.fd(fdfirst, fdsecond)
        y_dist[j1, i1] <- sqrt(fda::inprod(diff1, diff1, rng = subrange))
      }
    }

    y_dist <- as.matrix(stats::as.dist(t(y_dist), upper = T, diag = T))
  }

  return(y_dist)
}

# Create A New Node for Split Data Frame
new_node <- function(number,
                     var,
                     n,
                     wt,
                     inertia,
                     bipartsplitrow = -99L,
                     bipartsplitcol = -99L,
                     inertiadel = 0,
                     inertia_explained = -99,
                     medoid,
                     loc,
                     alt = FALSE) {

  one_row_table <- tibble::tibble(
    number, var, n,
    wt,
    inertia,
    bipartsplitrow,
    bipartsplitcol, inertiadel,
    inertia_explained,
    medoid,
    loc,
    alt)

  return(one_row_table)
}

# Find a approximate mean of a cluster of function
mean_fd <- function(yfd) {
  mfd <- mean(fda::predict.fd(fda::mean.fd(yfd)))
  return(mfd)
}

#' Coerce A PULS Object to MonoClust Object.
#'
#' An implementation of the [monoClust::as_MonoClust()] S3 method for PULS
#' object. The purpose of this is to reuse plotting and printing functions from
#' [monoClust] package.
#'
#' @param x A PULS object to be coerced to MonoClust object.
#' @param ... For extensibility.
#'
#' @return A MonoClust object coerced from PULS object.
#' @export
#'
#' @importFrom dplyr `%>%`
#'
#' @seealso [monoClust::MonoClust.object] and [PULS.object]
as_MonoClust.PULS <- function(x, ...) {

  # Check frame
  if (is.null(x$frame))
    stop("Object needs a \"frame\" object. See ?PULS.object for details.")

  frame <- x$frame
  if (!is.data.frame(frame))
    stop("\"frame\" object must be a data.frame or a data.frame derivation.")

  # Add missing columns
  frame <-
    frame %>%
    tibble::add_column(cut = NA,
                       split.order = NA) %>%
    dplyr::select("number", "var", "cut", "n", "inertia", "bipartsplitrow",
                  "bipartsplitcol", "inertiadel", "medoid", "loc",
                  "split.order", "inertia_explained", "alt")

  MonoClust_obj <-
    list(frame = frame,
         membership = x$membership,
         dist = x$dist,
         terms = x$terms,
         centroids = NULL,
         medoids = x$medoids,
         alt = x$alt,
         circularroot = list(var = NULL, cut = NULL))

  class(MonoClust_obj) <- "MonoClust"
  return(MonoClust_obj)
}
