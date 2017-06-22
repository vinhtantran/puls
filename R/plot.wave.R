plot.wave <- function(fd, puls, member = T, medians = T, ...) {
  if (member) {
    plot(fd, col=puls$Membership, ...)
  } else plot(fd, ...)
}
