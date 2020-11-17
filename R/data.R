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
#'   \item{Year}{years of available data (1978--2019)}
#'   \item{Month}{month (01--12)}
#'   \item{Day}{day of the month indicated in Column Month (Fair, Good, Very
#'     Good, Premium, Ideal)}
#'   \item{Extent}{daily ice extent, to three decimal places}
#'   \item{Missing}{whether a day is missing (1) or not (0))}
#'   \item{Source Data}{data source in NOAA database}
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

#' Interpolated and Smoothed Version of Arctic Data
#'
#' Raw Arctic data are smoothed and turned into a functional data. All are done
#' by using `fda` package.
#'
#' @format A data frame with 13391 rows and 6 variables:
#' \describe{
#'   \item{Year}{years of available data (1978--2019)}
#'   \item{Month}{month (01--12)}
#'   \item{Day}{day of the month indicated in Column Month (Fair, Good, Very
#'     Good, Premium, Ideal)}
#'   \item{Extent}{daily ice extent, to three decimal places}
#'   \item{Missing}{whether a day is missing (1) or not (0))}
#'   \item{Source Data}{data source in NOAA database}
#' }
#' @seealso NOAA's raw data at [arctic_2019].
"smoothed_arctic"
