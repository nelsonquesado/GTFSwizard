#' GTFS Data for Fortaleza (Bus System), Brazil.
#'
#' A dataset containing GTFS (General Transit Feed Specification) data for Fortaleza's transit system by bus. The data includes information on routes, trips, stops, stop times, and other elements necessary for transit planning and analysis.
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
#' @name for_bus_gtfs
#' @docType data
#'
#' @details
#' The GTFS data format is widely used for representing public transportation schedules and associated geographic information. This dataset follows the GTFS standard and includes elements for advanced analysis in transit planning.
#'
#' @source Fortaleza transit agency (ETUFOR).
#'
#' @examples
#' # Load the dataset
#' data(for_bus_gtfs)
#'
#' # Access trips data
#' head(for_bus_gtfs$trips)
#'
#' # Access stops data
#' head(for_bus_gtfs$stops)
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
utils::globalVariables(c("hour", "period", "n", "count", "direction_id"))
#data("for_bus_gtfs")

##' GTFS Data for Fortaleza (Rail System), Brazil
#'
#' This dataset contains GTFS (General Transit Feed Specification) data for Fortaleza's rail transit system, managed by METROFOR. The data includes information on routes, trips, stops, stop times, shapes, and other necessary elements for transit analysis and planning.
#'
#' @format An object of class \code{wizardgtfs}, consisting of multiple data frames:
#' \describe{
#'   \item{agency}{Data frame with 1 row and 7 columns, providing information about the transit agency, including agency name, URL, timezone, language, and contact details.}
#'   \item{calendar}{Data frame with 1 row and 10 columns, detailing the service availability by day of the week, along with start and end dates for each service.}
#'   \item{calendar_dates}{Data frame with 26 rows and 3 columns, listing specific dates and exceptions (e.g., holidays) that modify the usual service pattern.}
#'   \item{routes}{Data frame with 3 rows and 9 columns, listing route details such as route ID, short and long names, route type, and colors associated with each route.}
#'   \item{stops}{Data frame with 39 rows and 10 columns, containing information about each stop, including stop ID, name, location (latitude and longitude), and additional details.}
#'   \item{stop_times}{Data frame with 3,420 rows and 10 columns, detailing arrival and departure times for each trip, along with stop sequences and stop IDs.}
#'   \item{trips}{Data frame with 215 rows and 7 columns, providing trip-specific information such as trip ID, headsign, direction, associated service ID, route ID, and shape ID.}
#'   \item{shapes}{Data frame with 80 rows and 5 columns, representing spatial paths of routes using latitude, longitude, point sequence, and cumulative distance traveled.}
#' }
#'
#' @name for_rail_gtfs
#' @docType data
#'
#' @details
#' The GTFS data format is widely adopted for representing public transportation schedules and spatial information. This dataset follows GTFS standards and is tailored for advanced analysis, particularly in transit planning and operations. Key tables included are `agency`, `routes`, `stops`, `stop_times`, `trips`, and `shapes`, each providing essential attributes for a comprehensive transit analysis.
#'
#' @source Cia Cearense de Transportes Metropolitanos (METROFOR).
#'
#' @examples
#' # Load the dataset
#' data(for_rail_gtfs)
#'
#' # Access trips data
#' head(for_rail_gtfs$trips)
#'
#' # Access stops data
#' head(for_rail_gtfs$stops)
#'
#data("for_rail_gtfs")
