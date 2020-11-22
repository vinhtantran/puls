## code to prepare `smoothed_arctic` dataset goes here
library(lubridate)
library(dplyr)
library(tibble)
library(fda)
library(stringr)

data(arctic_2019)

# Create day in the year column to replace Month and Day
north <-
  arctic_2019 %>%
  mutate(yday = yday(make_date(Year, Month, Day)),
         .keep = "all") %>%
  select(Year, yday, Extent)

# Extract first and last 30 days of each year to attach to previous and next
# years
north_tails <-
  north %>%
  # Assume that every year has 365 days
  filter(yday <= 365) %>%
  # First and last 30 days
  filter(yday < 31 | yday > 335) %>%
  # Turn days as the extra begin and end of previous and next years
  mutate(Year = ifelse(yday < 60, Year - 1, Year + 1)) %>%
  mutate(yday = ifelse(yday > 60, yday - 365, yday + 365))

# Problematic years with too many missing measurements
# 2019 was removed because there is no buffer in 2020 for it
PROBLEM_YEARS <- c(1978, 1987, 1988)

# Arctic data with buffers at beginning and ends to avoid overfitting at ends
north_extra <- north %>%
  bind_rows(north_tails) %>%
  arrange(Year, yday) %>%
  filter(!(Year %in% PROBLEM_YEARS),
         Year <= 2019)

years <- sort(unique(north_extra$Year))

lambda_lst <- vector("double", length(years))

NORDER <- 4
gcv_extract <- function(lambda, i, north_year) {
  splinebasis <- create.bspline.basis(
    rangeval = c(min(north_year$yday),
                 max(north_year$yday)),
    breaks = north_year$yday[-c(1, length(north_year$yday))],
    norder = NORDER)

  fdParobj <- fdPar(fdobj = splinebasis, Lfdobj = 2, lambda = lambda)

  ice.fd.obj <- smooth.basis(argvals = c(north_year$yday),
                             y = north_year$Extent,
                             fdParobj = fdParobj)
  return(ice.fd.obj$gcv)
}

set.seed(12345)
count <- 0
for (i in years) {
  north_year <- north_extra %>%
    filter(Year == i)

  count <- count + 1
  upper <- 1
  lambda <- optim(par = 0.2,
                  gcv_extract,
                  lower = 0.001,
                  upper = upper,
                  i = i,
                  north_year = north_year)
  while (lambda$par == upper) {
    upper <- upper + 5
    lambda <- optim(par = 0.2,
                    gcv_extract,
                    lower = lambda$par,
                    upper = upper,
                    i = i,
                    north_year = north_year)
  }
  lambda_lst[count] <- lambda$par
}

predicted_mat <- matrix(NA, length(years), 365)

count <- 0
for (i in years) {
  count <- count + 1
  north_year <- north_extra %>%
    filter(Year == i)

  splinebasis <- create.bspline.basis(
    rangeval = c(min(north_year$yday),
                 max(north_year$yday)),
    breaks = north_year$yday[-c(1, length(north_year$yday))], norder = NORDER)

  fdParobj <- fdPar(fdobj = splinebasis,
                    Lfdobj = 2,
                    lambda = lambda_lst[count])
  icefd <- smooth.basis(argvals = c(north_year$yday),
                        y = north_year$Extent,
                        fdParobj = fdParobj)$fd
  predicted_mat[count, ] <- predict(icefd, newdata = 1:365)
}

smoothed_arctic <- as_tibble(predicted_mat) %>%
  set_names(str_c("day", 1:365, sep = "_")) %>%
  add_column(year = as.integer(years), .before = 1)

usethis::use_data(smoothed_arctic, overwrite = TRUE)
