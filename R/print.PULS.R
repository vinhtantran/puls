#' Print PULS Clustering Results
#'
#' Render the `PULS` split tree in an easy to read format with important
#' information such as terminal nodes, etc.
#'
#' @param x PULS result object.
#' @param spaces Spaces indent between 2 tree levels.
#' @param digits Number of significant digits to print.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A nicely displayed PULS split tree in text.
#'
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
#' print(PULS4_pam)
#' }
print.PULS <- function(x, spaces = 2L, digits = getOption("digits"), ...) {

  if (!inherits(x, "PULS"))
    stop("Not a legitimate \"PULS\" object")

  frame <- x$frame
  node <- frame$number
  depth <- tree_depth(node)

  # 2L is because of 1 number (1 or 2 digits) and the bracket
  indent <- stringr::str_pad(stringr::str_c(node, ")"), depth * spaces + 2L)

  inertia_explained <- ifelse(!is.na(frame$inertia_explained),
                              format(signif(frame$inertia_explained,
                                            digits = digits)),
                              "")

  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  labs <- create_labels(x, digits = digits, ...)
  n <- frame$n

  z <- paste(indent, labs, n, format(signif(frame$inertia, digits = digits)),
             inertia_explained, term)

  cat("n =", n[1L], "\n\n")

  cat("Node) Split, N, Cluster Inertia, Proportion Inertia Explained\n")
  cat("      * denotes terminal node\n\n")
  cat(z, sep = "\n")

  # Add a note of duplicate here
  return(invisible(x))
}

#' Create Labels for Split Variables
#'
#' This function prints variable's labels for a `PULS` tree.
#'
#' @inheritParams print.PULS
#'
#' @return A list containing two elements:
#'   * `names`: A named vector of labels corresponding to range's names
#'   (at vector names).
#'   * `labels`: Vector of labels of splitting rules to be displayed.
#' @keywords internal
create_labels <- function(x, digits, ...) {

  frame <- x$frame

  # Create split labels
  split_index <- which(frame$var != "<leaf>")
  lsplit <- rsplit <- character(length(split_index))

  label <- frame$var[split_index]

  lsplit <- paste0(label, "--")
  rsplit <- paste0(label, "++")

  node <- frame$number
  parent <- match(node %/% 2, node[split_index])
  odd <- as.logical(node %% 2)

  labels <- character(nrow(frame))
  labels[odd] <- rsplit[parent[odd]]
  labels[!odd] <- lsplit[parent[!odd]]
  labels[1] <- "root"

  return(labels)
}
