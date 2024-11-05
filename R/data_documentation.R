#' GTFS Data for Fortaleza, Brazil.
#'
#' A dataset containing GTFS (General Transit Feed Specification) data for Fortaleza's transit system. The data includes information on routes, trips, stops, stop times, and other elements necessary for transit planning and analysis.
#'
#' @format An object of class \code{wizardgtfs}, containing multiple data frames:
#' \describe{
#'   \item{agency}{Data frame with 1 row and 7 columns, providing information about the transit agency, including agency name, URL, timezone, and contact details.}
#'   \item{calendar}{Data frame with 3 rows and 10 columns, detailing service availability by day of the week, start and end dates for each service.}
#'   \item{fare_attributes}{Data frame with 2 rows and 6 columns, showing fare information, including price, currency, payment method, and transfer rules.}
#'   \item{fare_rules}{Data frame with 259 rows and 5 columns, linking fare IDs to routes, along with optional restrictions on origins, destinations, and zones.}
#'   \item{routes}{Data frame with 259 rows and 9 columns, listing route details such as route ID, agency ID, route short and long names, route type, and colors.}
#'   \item{shapes}{Data frame with 89,846 rows and 5 columns, representing the spatial paths of routes with latitude, longitude, point sequence, and cumulative distance traveled.}
#'   \item{stop_times}{Data frame with 1,719,386 rows and 9 columns, including stop times for each trip, with arrival and departure times, stop sequence, and stop ID information.}
#'   \item{stops}{Data frame with 4,793 rows and 12 columns, containing information about each stop, including stop ID, name, location (latitude and longitude), and accessibility.}
#'   \item{trips}{Data frame with 52,304 rows and 9 columns, detailing trips associated with routes, including trip IDs, route IDs, direction, block, and shape IDs.}
#' }
#'
#' @name for_gtfs
#' @docType data
#'
#' @details
#' The GTFS data format is widely used for representing public transportation schedules and associated geographic information. This dataset follows the GTFS standard and includes elements for advanced analysis in transit planning.
#'
#' @source Fortaleza transit agency (ETUFOR).
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(for_gtfs)
#'
#' # Access trips data
#' head(for_gtfs$trips)
#'
#' # Access stops data
#' head(for_gtfs$stops)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr n
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr if_else
#' @importFrom sf st_contains
#' @importFrom sf st_crosses
#' @importFrom sf st_crs<-
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_equals
#' @importFrom sf st_intersects
#' @importFrom sf st_overlaps
#' @importFrom sf st_point
#' @importFrom sf st_sfc
#' @importFrom sf st_touches
#' @importFrom sf st_within
#' @importFrom lubridate is.POSIXct
#' @importFrom lubridate is.POSIXlt
#' @importFrom lubridate is.Date
#' @importFrom lubridate interval
#' @importFrom lubridate int_start
#' @importFrom lubridate int_end
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 theme_light
#' @importFrom ggplot2 theme_linedraw
#' @importFrom data.table setnames
#' @importFrom forcats as_factor
utils::globalVariables(c("hour", "period", "n", "count"))
# data(for_gtfs)
