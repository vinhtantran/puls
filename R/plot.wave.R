plot.wave <- function(fd, member = T, medians = T, ...) {
  if (member) {
    plot(yfd$fd, col=PULS4$Membership, ...)
  } else plot(yfd$fd, ...)
}