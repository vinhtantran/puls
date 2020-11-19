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
                     # split.order = -99L,
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
    # split.order,
    alt)

  return(one_row_table)
}

# Find a approximate mean of a cluster of function
mean_fd <- function(yfd) {
  mfd <- mean(fda::predict.fd(fda::mean.fd(yfd)))
  return(mfd)
}
