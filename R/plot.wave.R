#' Plot the Partitioned Functional Wave by PULS
#'
#' After partitioning using PULS, this function can plot the functional waves and
#' color different clusters as well as their medoids
#'
#' @param fd the fdSmooth object, created by fda::smooth.basis() function
#' @param puls the output of PULS algorithm
#' @param member whether or not making cluster's members different colors
#' @param medoids whether or not highlighting the medoid of each cluster
#' @param separate if \code{TRUE}, the function will create separate plot for each cluster
#' @param ... other optional plotting arguments
#'
#' @examples
plot.wave <- function(fd, puls, member = T, medoids = T, separate = F, ...) {

  if (member && !medoids) {
    plot.fd(fd$fd, col=puls$Membership,...)
  } else if (!member && !medoids) plot(fd, col="gray", ...)

  if (medoids) {
    meds <- puls$frame$medoid[puls$frame$var == "<leaf>"]
    lty.lst <- rep(3, length(puls$Membership))
    lty.lst[meds] <- 1

    lwd.lst <- rep(1, length(puls$Membership))
    lwd.lst[meds] <- 3

    # A trick to change color starting from 1
    m <- as.factor(puls$Membership)
    levels(m) <- 1:length(m)
    m <- as.numeric(m)

    if (!separate) {
      plot(fd$fd, col=m, lty=lty.lst, lwd=lwd.lst, ...)
    } else {
      par(ask=T)
      for (i in 1:length(meds)) {
        lty.lst1 <- lty.lst
        lty.lst1[-which(m == i)] <- 0
        plot(fd$fd, col=m, lty=lty.lst1, lwd=lwd.lst, ...)
      }
      par(ask=F)
    }
  }
}
