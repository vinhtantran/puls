## code to prepare `smoothed_arctic` dataset goes here
library(lubridate)
library(dplyr)
library(fda)

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
PROBLEM_YEARS <- c(1978, 1987, 1988, 2019)

# Arctic data with buffers at beginning and ends to avoid overfitting at ends
north_extra <- north %>%
  bind_rows(north_tails) %>%
  arrange(Year, yday) %>%
  filter(!(Year %in% PROBLEM_YEARS))

# ny <- length(unique(north_extra$Year))
years <- unique(north_extra$Year)
# n_79_86 <- 1986 - 1979 + 1

lambda.lst <- vector("double", length(years))

count <- 0
for (i in years) {
  print(i)
  count <- count + 1
  north_year <- north_extra %>%
    filter(Year == i)

  round <- 1
  while (round < 4) {
    # First round
    success <- FALSE
    n <- 1
    while (!success) {
      gcv.lst <- NULL
      if (round == 1) {
        range <- log(seq((n - 1) * 100 + 1,
                         n * 100,
                         by = 1))
      } else if (round == 2) {
        range <- log(seq(exp(range[smallest]) - 1,
                         exp(range[smallest]) + 1,
                         by = .1))
      } else if (round == 3) {
        range <- log(seq(exp(range[smallest]) - 0.1,
                         exp(range[smallest]) + 1,
                         by = .01))
      }

      range <- range[-1]

      for (j in seq_len(length(range))) {
        splinebasis <-
          create.bspline.basis(
            rangeval = c(min(north_year$yday),
                         max(north_year$yday)),
            breaks = north_year$yday[-c(1,
                                        length(north_year$yday))],
            norder = NORDER)
        fdParobj <-
          fdPar(fdobj = splinebasis, Lfdobj = 2, lambda = range[j])

        ice.fd.obj <-
          smooth.basis(argvals = north_year$yday,
                       y = north_year$Extent,
                       fdParobj = fdParobj)

        icefd <- ice.fd.obj$fd

        gcv.lst <- c(gcv.lst, ice.fd.obj$gcv)
      }

      smallest <- which(gcv.lst == min(gcv.lst))

      if (smallest == length(range)) {
        success = FALSE
        n = n + 1
      } else {
        success = TRUE
        round = round + 1
      }
    }
  }

  lambda.lst[count] <- range[smallest]
}

# n_89_18 <- 2018 - 1989 + 1
#
# for (i in seq(n_79_86 + 1, n_79_86 + n_89_18)) {
#   round <- 1
#   while (round < 4) {
#     # First round
#     success <- FALSE
#     n <- 1
#     while (!success) {
#       gcv.lst <- NULL
#       if (round == 1) {
#         range <- log(seq((n - 1) * 100 + 1,
#                          n * 100,
#                          by = 1))
#       } else if (round == 2) {
#         range <- log(seq(exp(range[smallest]) - 1,
#                          exp(range[smallest]) + 1,
#                          by = .1))
#       } else if (round == 3) {
#         range <- log(seq(exp(range[smallest]) - 0.1,
#                          exp(range[smallest]) + 1,
#                          by = .01))
#       }
#
#       range <- range[-1]
#
#       for (j in seq_len(length(range))) {
#         splinebasis <-
#           create.bspline.basis(rangeval=c(min(lapse[[i]]$days),
#                                           max(lapse[[i]]$days)),
#                                breaks = lapse[[i]]$days[-c(1,
#                                                            lapse[[i]]$count)],
#                                norder = NORDER)
#         fdParobj <- fdPar(fdobj = splinebasis, Lfdobj = 2, lambda = range[j])
#         ice.fd.obj <- smooth.basis(argvals = c(lapse[[i]]$days),
#                                    y = lapse[[i]]$extent,
#                                    fdParobj = fdParobj)
#         icefd <- ice.fd.obj$fd
#         gcv.lst <- c(gcv.lst, ice.fd.obj$gcv)
#       }
#
#       smallest <- which(gcv.lst == min(gcv.lst))
#       if (smallest == length(range)) {
#         success = FALSE
#         n = n + 1
#       } else {
#         success = TRUE
#         round = round + 1
#       }
#     }
#   }
#   lambda.lst[i] <- range[smallest]
# }

NORDER <- 4
lambda_lst <- c(3.04143399235758, 6.00000104178988, 5.46493931293214,
                11.000002482053, 2.69050509199204, 0.151289123842184,
                0.358207423110365, 0.115733218610054, 0.134841375374795,
                0.200032860831763, 0.118105181612112, 0.250682822163901,
                0.100225932497699, 0.156699405804881, 0.0928699218218082,
                0.167119680188913, 0.104431338024702, 0.220248433647683,
                0.337943516911988, 0.0627317200476132, 0.125412569510898,
                0.241791491643966, 0.0695567623646838, 0.155231825555467,
                0.341869684791558, 0.0820673929206614, 0.118119627429064,
                0.274505688757124, 0.173405889084251, 0.0343182158057013,
                0.128954226011491, 0.0593591426712641, 0.094157908594449,
                0.0568046820866631)

year_lst <- unique(north_extra[["Year"]])

smoothed_arctic <- matrix(NA, YEARS, 366)
fd_vector <- vector("list", length(unique(north_extra$Year)))

for (i in seq_len(length(unique(north_extra$Year)))) {
  ydata <-
    north_extra %>%
    filter(Year == year_lst[i]) %>%
    select(yday, Extent)

  splinebasis <- create.bspline.basis(rangeval = c(min(ydata$yday),
                                                   max(ydata$yday)),
                                      breaks = ydata$yday,
                                      norder = NORDER)

  fdParobj <- fdPar(fdobj = splinebasis,
                    Lfdobj = 2,
                    lambda = lambda_lst[i])
  fd_vector[i] <- smooth.basis(argvals = ydata$yday,
                               y = ydata$Extent,
                               fdParobj = fdParobj)
}

usethis::use_data(smoothed_arctic, overwrite = TRUE)
