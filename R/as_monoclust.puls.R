#' Coerce a PULS Object to MonoClust Object
#'
#' An implementation of the [monoClust::as_MonoClust()] S3 method for PULS
#' object. The purpose of this is to reuse plotting and printing functions from
#' [monoClust](https://cran.r-project.org/package=monoClust) package.
#'
#' @param x A PULS object to be coerced to MonoClust object.
#' @param ... For extensibility.
#'
#' @return A MonoClust object coerced from PULS object.
#'
#' @importFrom monoClust as_MonoClust
#'
#' @export
#'
#' @seealso [monoClust::MonoClust.object] and [PULS.object]
as_MonoClust.PULS <- function(x, ...) {

  # Check frame
  if (is.null(x$frame))
    stop("Object needs a \"frame\". See PULS.object for details.")

  required_obj <- c("membership", "dist", "terms", "medoids", "alt")

  if (any(is.na(match(required_obj, names(x)))))
    stop("Object must have required objects. See PULS.object for details.")

  frame <- x$frame
  if (!is.data.frame(frame))
    stop("\"frame\" object must be a data.frame or a data.frame derivation.")

  required_cols <- c("number", "var", "n", "inertia", "bipartsplitrow",
                     "bipartsplitcol", "inertiadel", "medoid", "loc",
                     "inertia_explained", "alt")

  if (any(is.na(match(required_cols, colnames(frame)))))
    stop(strwrap("\"frame\" must have required columns. See PULS.object for
                 details.", prefix = " ", initial = ""))

  # Add missing columns
  frame <-
    frame %>%
    tibble::add_column(cut = NA,
                       split.order = NA) %>%
    dplyr::select(dplyr::all_of(required_cols), "cut", "split.order")

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
