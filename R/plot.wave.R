plot.wave <- function(fd, puls, member = T, medoids = T, ...) {
  mbs <- unique(puls$Membership)

  if (member) {
    plot.fd(fd$fd, col=puls$Membership, ...)
  } else plot(fd, col="gray", ...)

  if (medoids) {
    lty.lst <- rep(3, length(puls$Membership))
    lty.lst[puls$frame$medoid[member]] <- 1

    lwd.lst <- rep(1, length(puls$Membership))
    lwd.lst[puls$frame$medoid[member]] <- 3

    plot(fd$fd, col=puls$Membership - min(puls$Membership) + 1, lty=lty.lst, lwd=lwd.lst)

    for (i in member) {
      lines(1:365, fd$y[,puls$frame$medoid[i]], type = "l", col=i, lwd=2)
    }

  }
}
