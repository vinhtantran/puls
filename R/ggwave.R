#' Plot the Partitioned Functional Wave by PULS
#'
#' After partitioning using PULS, this function can plot the functional waves
#' and color different clusters as well as their medoids.
#'
#' @param puls.obj A `PULS` object as a result of [PULS()].
#' @param xlab Labels for x-axis. If not provided, the labels stored in `fd`
#'   object will be used.
#' @param ylab Labels for y-axis.  If not provided, the labels stored in `fd`
#'   object will be used.
#' @param lwd Linewidth of normal waves.
#' @param alpha Transparency of normal waves.
#' @param lwd.med Linewidth of medoid waves.
#' @inheritParams PULS
#'
#' @return A ggplot2 object.
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
#' PULS4_pam <- PULS(toclust.fd = yfd$fd, intervals = intervals,
#'                   nclusters = 4, method = "pam")
#' ggwave(toclust.fd = yfd$fd, intervals = intervals, puls = PULS4_pam)
#' }
ggwave <- function(toclust.fd, intervals, puls.obj,
                   xlab = NULL, ylab = NULL, lwd = 0.5, alpha = 0.4,
                   lwd.med = 1) {

  fdobj <- toclust.fd
  if (!(fda::is.fd(fdobj)))
    stop(paste("\"toclust.fd\" must be a functional data object (fda::is.fd)."))

  # Recreate data frame
  coef <- fdobj$coefs
  coefd <- dim(coef)
  ndim <- length(coefd)
  if (ndim > 2)
    stop("Function only supports 2-dimensional functional object.")

  nbasis <- coefd[1]
  nx <- max(c(501, 10 * nbasis + 1))
  nrep <- coefd[2]

  basisobj <- fdobj$basis
  rangex <- basisobj$rangeval

  y <- seq(rangex[1], rangex[2], len = round(nx))

  if (min(y) < rangex[1] || max(y) > rangex[2])
    stop("Values in y are outside the basis range.")

  fdmat <- fda::eval.fd(y, fdobj, Lfdobj = 0)

  obs_label <- as.character(seq_len(ncol(fdmat)))

  # Data frame of membership to join with the estimated data later
  membership_df <-
    tibble::tibble(name = obs_label,
                   membership = as.character(puls.obj$membership))

  # Axis labels
  fdnames <- fdobj$fdnames
  fdlabelslist <- fda::fdlabels(fdnames, nrep, 1)

  if (is.null(xlab))
    xlab <- fdlabelslist$xlabel
  if (is.null(ylab))
    ylab <- fdlabelslist$ylabel

  # Estimate the data points from fd object on a fine grid
  colnames(fdmat) <- obs_label
  est_data <- tibble::as_tibble(fdmat) %>%
    tibble::add_column(x = y) %>%
    tidyr::pivot_longer(cols = seq_len(ncol(fdmat))) %>%
    dplyr::left_join(membership_df, by = "name")

  # Create data frame of intervals to facilitate plot
  colnames(intervals) <- c("l", "r")

  intervals_tbl <-
    tibble::as_tibble(intervals, rownames = "name") %>%
    dplyr::mutate(x = (.data$l + .data$r) / 2,
                  .keep = "unused") %>%
    tibble::add_column(y = max(fdmat) * 1.1)

  # Plot
  p <-
    est_data %>%
    ggplot2::ggplot() +
    # Waves
    ggplot2::geom_line(ggplot2::aes(x = .data$x, y = .data$value,
                                    group = .data$name,
                                    color = .data$membership),
                       size = lwd, alpha = alpha) +
    # Medoid
    ggplot2::geom_line(
      data = est_data %>%
        dplyr::filter(.data$name %in% obs_label[puls.obj$medoids]),
      ggplot2::aes(x = .data$x, y = .data$value,
                   group = .data$name,
                   color = .data$membership),
      size = lwd.med) +
    # Intervals
    ggplot2::scale_x_continuous(breaks = unique(as.vector(intervals)),
                                minor_breaks = NULL) +
    ggplot2::geom_text(data = intervals_tbl,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$name)) +
    # Labels
    ggplot2::labs(x = xlab,
                  y = ylab)

  return(p)
}
