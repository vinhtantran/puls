# Create A New Node for Split Data Frame
new_node <- function(number,
                     var,
                     n,
                     wt,
                     inertia,
                     bipartsplitrow = -99L,
                     bipartsplitcol = -99L,
                     inertiadel = 0,
                     inertia_explained = -99,
                     medoid,
                     loc,
                     alt = FALSE) {

  one_row_table <- tibble::tibble(
    number, var, n,
    wt,
    inertia,
    bipartsplitrow,
    bipartsplitcol, inertiadel,
    inertia_explained,
    medoid,
    loc,
    alt)

  return(one_row_table)
}

# Find a approximate mean of a cluster of function
mean_fd <- function(yfd) {
  mfd <- mean(fda::predict.fd(fda::mean.fd(yfd)))
  return(mfd)
}
