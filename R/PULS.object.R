#' PULS Tree Object
#'
#' The structure and objects contained in PULS, an object returned from
#' the [PULS()] function and used as the input in other functions in the
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
#'     `distmethod` argument of [PULS()].}
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
#' @seealso [PULS()].
NULL
