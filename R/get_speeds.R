#' Calculate Speeds for GTFS Routes and Trips
#'
#' `get_speeds` calculates the average speed of trips and routes within a `wizardgtfs` object. It uses distance and duration to provide speed outputs based on the specified `method`.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If the `shapes` table is missing, it will be created automatically using `get_shapes()`.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.route"}{Calculates the average speed for each route based on average distance and duration.}
#'     \item{"by.trip"}{Calculates the average speed for each trip based on total distance and duration.}
#'     \item{"detailed"}{Calculates the speed for each segment between stops within a trip.}
#'   }
#'
#' @return A data frame containing speed calculations, depending on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `trips`, `average.speed`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `average.speed`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `trip_id`, `hour`, `from_stop_id`, `to_stop_id`, `speed`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' - This function calls specific sub-functions based on the selected `method`:
#'   \describe{
#'     \item{`by.route`}{Calculates average speed across each route.}
#'     \item{`by.trip`}{Calculates average speed across each trip.}
#'     \item{`detailed`}{Calculates speeds between consecutive stops within each trip.}
#'   }
#'
#' - If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#'  \dontrun{
#' # Calculate average route speeds
#' speeds_by_route <- get_speeds(gtfs = for_gtfs, method = "by.route")
#'
#' # Calculate trip speeds
#' speeds_by_trip <- get_speeds(gtfs = for_gtfs, method = "by.trip")
#'
#' # Calculate detailed speeds between stops
#' detailed_speeds <- get_speeds(gtfs = for_gtfs, method = "detailed")
#' }
#'
#' @seealso
#' [GTFSwizard::get_distances()], [GTFSwizard::get_durations()], [GTFSwizard::get_shapes()]
#'
#' @importFrom dplyr select mutate group_by left_join reframe
#' @importFrom GTFSwizard get_servicepattern get_distances get_durations
#' @export
get_speeds <- function(gtfs, method = 'by.route'){
  # Function code here...
}

# Internal functions for different methods
get_speeds_byroute <- function(gtfs){ ... }
get_speeds_bytrip <- function(gtfs){ ... }
get_speeds_detailed <- function(gtfs){ ... }


get_speeds <- function(gtfs, method = 'by.route'){

  if(purrr::is_null(gtfs$shapes)){

    gtfs <- GTFSwizard::get_shapes(gtfs)

    warning('\nThis gtfs object does not contain a shapes table.\nUsing get_shapes().')
  }

  if (method == 'by.route') {
    speeds <- get_speeds_byroute(gtfs)
  }

  if (method == 'by.trip') {
    speeds <- get_speeds_bytrip(gtfs)
  }

  if (method == 'detailed') {
    speeds <- get_speeds_detailed(gtfs)
  }

  if (!method %in% c('by.route',
                     'by.trip',
                     'detailed')) {
    speeds <- get_speeds_byroute(gtfs)
    warning('\n"method" should be one of "by.route", "by.trip" or "detailed".\nReturning "method = by.route"".')
  }

  return(speeds)

}

get_speeds_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){

    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

    warning('\nThis gtfs object is not of wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')

  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <-
    GTFSwizard::get_distances(gtfs, method = 'by.route')

  durations <-
    GTFSwizard::get_durations(gtfs, method = 'by.route')

  speeds <-
    durations %>%
    dplyr::left_join(distances,
                     by = c('route_id', 'service_pattern', 'pattern_frequency')) %>%
    dplyr::group_by(route_id) %>%
    dplyr::reframe(trips = n(),
                   average.speed = as.numeric((average.distance/1000) / (average.duration/3600)),
                   service_pattern,
                   pattern_frequency)

  return(speeds)

}

get_speeds_bytrip <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){

    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

    warning('\nThis gtfs object is not of wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')

  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <-
    GTFSwizard::get_distances(gtfs, method = 'by.trip')

  durations <-
    GTFSwizard::get_durations(gtfs, method = 'by.trip')

  speeds <-
    durations %>%
    dplyr::left_join(distances,
                     by = c('route_id', 'trip_id', 'service_pattern', 'pattern_frequency')) %>%
    dplyr::group_by(route_id, trip_id) %>%
    dplyr::reframe(average.speed = as.numeric((distance/1000) / (duration/3600)),
                   service_pattern,
                   pattern_frequency)

  return(speeds)

}

get_speeds_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){

    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

    warning('\nThis gtfs object is not of wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')

  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <-
    GTFSwizard::get_distances(gtfs, method = 'detailed')%>%
    dplyr::left_join(gtfs$trips %>% select(trip_id, shape_id),
                     by = 'shape_id',
                     relationship = 'many-to-many')

  durations <-
    GTFSwizard::get_durations(gtfs, method = 'detailed')

  speeds <-
    durations %>%
    dplyr::left_join(distances,
                     by = c('trip_id', 'from_stop_id', 'to_stop_id')) %>%
    dplyr::group_by(route_id, trip_id, hour, from_stop_id, to_stop_id) %>%
    dplyr::reframe(speed = (distance/1000) / (duration/3600),
                   service_pattern,
                   pattern_frequency)

  return(speeds)

}

