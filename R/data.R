#' NOAA's Arctic Sea Daily Ice Extend Data
#'
#' A data set containing the daily ice extent at Arctic Sea from 1978 to 2019,
#' collected by National Oceanic and Atmospheric Administration (NOAA)
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{start}{years of available data (1978--2019)}
#'   \item{end}{month (01--12)}
#' }
#' @source <https://nsidc.org/data/G02135/versions/3>
"soccer_intervals"

#' Existence of Microorganisms Carried in Wind
#'
#' Data set is a part of a study on microorganisms carried in strong f\"ohn
#' winds at the Bonney Riegel location of Taylor Valley, an ice free area in the
#' Antarctic continent. Wind direction and wind speed data were obtained from
#' the meteorological station. Wind direction was recorded every 30 seconds and
#' wind speeds every 4 seconds at 1.15 meters above the ground surface. The
#' recorded wind directions and speeds were averaged at 15 minute intervals. For
#' wind direction, as discussed previously, winds from the north are defined as
#' 0/360 degrees and from the east as 90 degrees. 2007 data were collected from
#' August 4--11, 2007.
#'
#' @format A data frame with 671 rows and 3 variables:
#' \describe{
#'   \item{has.sensit}{A binary variable of the existence of particles in the
#'     wind (1) or not (0).}
#'   \item{WS}{wind speed measured in m/s.}
#'   \item{WDIR}{wind direction in degree with 0 indicates "from the north" and
#'     90 degrees indicate "from the east".}
#' }
#' @source Sabacka, M., Priscu, J. C., Basagic, H. J., Fountain, A. G., Wall, D.
#' H., Virginia, R. A., and Greenwood, M. C. (2012). "Aeolian flux of biotic and
#' abiotic material in Taylor Valley, Antarctica". In: Geomorphology 155-156,
#' pp. 102-111. issn: 0169555X. doi: 10.1016/j.geomorph.2011.12.009.
"soccer.wide"

#' NOAA's Arctic Sea Daily Ice Extend Data
#'
#' A data set containing the daily ice extent at Arctic Sea from 1978 to 2019,
#' collected by National Oceanic and Atmospheric Administration (NOAA)
#'
#' @format A data frame with 13391 rows and 6 variables:
#' \describe{
#'   \item{Year}{Years of available data (1978--2019).}
#'   \item{Month}{Month (01--12).}
#'   \item{Day}{Day of the month indicated in Column Month.}
#'   \item{Extent}{Daily ice extent, to three decimal places.}
#'   \item{Missing}{Whether a day is missing (1) or not (0)).}
#'   \item{Source Data}{data source in NOAA database.}
#' }
#' @source <https://nsidc.org/data/G02135/versions/3>
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(ggplot2)
#'
#' data(arctic_2019)
#'
#' # Create day in the year column to replace Month and Day
#' north <-
#'   arctic_2019 %>%
#'   mutate(yday = yday(make_date(Year, Month, Day)),
#'          .keep = "all") %>%
#'   select(Year, yday, Extent)
#'
#' ggplot(north) +
#'   geom_linerange(aes(x = yday, ymin = Year - 0.2, ymax = Year + 0.2),
#'                  size = 0.5, color = "red") +
#'   scale_y_continuous(breaks = seq(1980, 2020, by = 5),
#'                      minor_breaks = NULL) +
#'   labs(x = "Day",
#'        y = "Year",
#'        title = "Measurement frequencies were not always the same")
"arctic_2019"

#' Discrete Form of Smoothed Functional Form of Arctic Data
#'
#' Raw Arctic data were smoothed and then transformed into functional data using
#' `fda` package. To overcome the difficulty of exporting an `fda` object in a
#' package, the object was discretized into a data set with 365 columns
#' corresponding to 365 days a year and 39 rows corresponding to
#' 39 years. The years are from 1979 to 1986, then from 1989 to 2018. The years
#' 1978, 1987, and 1988 were removed because the measurements were not complete.
#'
#' @format A data frame with 39 rows corresponding to 39 years (1979 to 1986,
#'   1989 to 2019) and 366 columns:
#'   \describe{
#'     \item{year}{Year of data.}
#'     \item{day_1 to day_365}{365 days in a year.}
#'   }
#' @seealso NOAA's raw data at [arctic_2019] and the code to generate this data
#'   in data-raw/ folder of source code.
"smoothed_arctic"
