plot.wave <- function(fd, puls, member = T, medoids = T, lty = 3, ...) {
  if (member) {
    plot(fd, col=puls$Membership, lty, ...)
  } else plot(fd, col="gray", ...)

  if (medoids) {
    member <- unique(puls$Membership)
    for (i in member) {
      lines(1:dim(fd$argvals)[1], fd$y[,puls$frame$medoid[i]], type = "l", col=i, lwd=2)
    }

  }
}
