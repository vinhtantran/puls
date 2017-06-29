plot.wave <- function(fd, puls, member = T, medoids = T, ...) {

  if (member && !medoids) {
    plot.fd(fd$fd, col=puls$Membership,...)
  } else if (!member && !medoids) plot(fd, col="gray", ...)

  if (medoids) {
    meds <- puls$frame$medoid[puls$frame$var == "<leaf>"]
    lty.lst <- rep(3, length(puls$Membership))
    lty.lst[meds] <- 1

    lwd.lst <- rep(1, length(puls$Membership))
    lwd.lst[meds] <- 3

    plot(fd$fd, col=puls$Membership - min(puls$Membership) + 1, lty=lty.lst, lwd=lwd.lst)
  }
}
