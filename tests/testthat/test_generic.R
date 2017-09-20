library(PULS)
context("Just a generic test to debug and doing step by step")

test_that("a generic", {
  load("data/soccer.wide.Rdata")
  load("data/intervals.Rdata")

  PULS.nonfd(soccer.wide[,-1], intervals = intervals, labels = soccer.wide[,1])
})
