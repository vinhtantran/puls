#' Coerce a PULS Object to MonoClust Object
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
