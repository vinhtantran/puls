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
                     cut = -99L,
                     n,
                     wt,
                     inertia,
                     # ?
                     bipartvar = "NA",
                     bipartsplitrow = -99L,
                     bipartsplitcol = -99L,
                     inertiadel = 0,
                     inertia_explained = -99,
                     # ?
                     yval,
                     medoid,
                     # ?
                     category = NA,
                     loc,
                     # split.order = -99L,
                     # ?
                     stringsAsFactors = FALSE
                     # alt = list(
                     #   tibble::tibble(bipartsplitrow = numeric(),
                     #                  bipartsplitcol = numeric()))
                     ) {

  one_row_table <- tibble::tibble(
    number, var, cut, n,
    # ?
    wt,
    inertia,
    # ?
    bipartvar,
    bipartsplitrow,
    bipartsplitcol, inertiadel,
    inertia_explained,
    # ?
    yval,
    medoid,
    # ?
    category,
    loc,
    # split.order,
    # alt,
    stringsAsFactors)

  return(one_row_table)
}

#' Find Tree Depth Based on Node Indexes
#'
#' @param nodes Vector of node indexes in the tree.
#'
#' @details
#' When building PULS tree, the node index was created with the rule that
#'   new node indexes are the split node times 2 plus 0 (left) and 1 (right).
#'   Therefore, this function is just a back-transform, taking a log base 2.
#'
#' @return Depth of the node, with 0 is the root relative to the input.
#'
#' @keywords internal
tree_depth <- function(nodes) {
  depth <- floor(log2(nodes) + 1e-07)
  return(depth - min(depth))
}
