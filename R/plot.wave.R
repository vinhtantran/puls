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

    # A trick to change color starting from 1
    m <- as.factor(puls$Membership)
    levels(m) <- 1:length(m)
    m <- as.numeric(m)
    plot(fd$fd, col=m, lty=lty.lst, lwd=lwd.lst)
  }
}
