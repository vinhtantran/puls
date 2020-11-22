#' Distance Between Functional Objects
#'
#' Calculate the distance between functional objects over the defined range.
#'
#' @param fd A functional data object `fd` of `fda` package.
#' @param subrange A vector of two values indicating the value range of
#'   functional object to calculate on.
#' @inheritParams PULS
#'
#' @details
#' If choosing `distmethod = "manual"`, the L2 distance between all pairs of
#'   functions \eqn{y_i(t)} and \eqn{y_j(t)} is given by:
#' \deqn{d_R(y_i, y_j) = \sqrt{\int_{a_r}^{b_r} [y_i(t) - y_j(t)]^2 dt}.}
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
fdistmatrix <- function(fd, subrange, distmethod) {

  if (!fda::is.fd(fd))
    stop("\"fd\" must be a functional data object (checked with fda::is.fd).")

  n <- length(fd$fdnames$reps)
  y_dist <- matrix(0, nrow = n, ncol = n)

  if (distmethod == "usc") {
    # Create a higher resolution grid to predict and then remake fdata objects
    # on reduced domains
    nargs <- length(fd$basis$params + 2) * 5
    t_high <- seq(from = subrange[1], to = subrange[2], length.out = nargs)

    predfd <- fda::predict.fd(fd, t_high)
    fdata <- fda.usc::fdata(mdata = t(predfd), argvals = t_high)

    y_dist <- as.matrix(stats::as.dist(fda.usc::metric.lp(fdata),
                                       diag = T,
                                       upper = T))
  } else {
    for (j1 in seq_len(n - 1)) {
      fdfirst <- fd[j1]
      for (i1 in (j1 + 1):n) {
        fdsecond <- fd[i1]
        diff1 <- fda::minus.fd(fdfirst, fdsecond)
        y_dist[j1, i1] <- sqrt(fda::inprod(diff1, diff1, rng = subrange))
      }
    }

    y_dist <- as.matrix(stats::as.dist(t(y_dist), upper = T, diag = T))
  }

  return(y_dist)
}
