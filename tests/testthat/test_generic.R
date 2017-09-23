library(PULS)
context("Just a generic test to debug and doing step by step")

test_that("a generic", {
  load("data/soccer.wide.Rdata")
  load("data/intervals.Rdata")

  NORDER <- 1
  splinebasis <- create.bspline.basis(rangeval=c(1,275),norder=NORDER, breaks = 1:275)
  fdParobj<-fdPar(fdobj=splinebasis,Lfdobj=0,lambda=.000001) # No need for any more smoothing

  yfd<-smooth.basis(argvals=1:ncol(soccer.wide[,-1]), y=t(as.matrix(soccer.wide[,-1])), fdParobj=fdParobj)


  PULS(yfd$fd, intervals = intervals, labels = soccer.wide[,1], nclusters = 4)
  plot(PULS(yfd$fd, intervals = intervals, labels = soccer.wide[,1], nclusters = 4))
})
