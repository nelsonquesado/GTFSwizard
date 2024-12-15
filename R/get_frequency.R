#' Calculate Route Frequency in GTFS Data
#'
#' The `get_frequency` function calculates route frequency within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide daily frequencies by route or detailed hourly frequencies.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.route"}{Calculates the total daily frequency for each route.}
#'     \item{"by.shape"}{Calculates the total daily frequency for each shape.}
#'     \item{"detailed"}{Calculates the hourly frequency for each route.}
#'   }
#'
#' @return A data frame containing route frequencies based on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `direction_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.shape"`}{Returns a data frame with columns: `shape_id`, `direction_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `direction_id`, `hour`, `frequency`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.route": Calculates the total daily frequency for each route.
#'
#' - "by.shape": Calculates the total daily frequency for each shape.
#'
#' - "detailed": Provides an hourly breakdown of frequency, showing the number of departures per hour for each route and direction.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' # Calculate daily route frequency
#' frequency_by_route <- get_frequency(gtfs = for_rail_gtfs, method = "by.route")
#'
#' # Calculate daily shape frequency
#' frequency_by_shape <- get_frequency(gtfs = for_rail_gtfs, method = "by.shape")
#'
#' # Calculate detailed hourly frequency
#' detailed_frequency <- get_frequency(gtfs = for_rail_gtfs, method = "detailed")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter
#' @importFrom stringr str_extract
#' @export
get_frequency <- function(gtfs, method = 'by.route'){

  if (method == "by.route") {
    freq <- get_frequency_byroute(gtfs)
  }

  if (method == "by.shape") {
    freq <- get_frequency_byshape(gtfs)
  }

  if (method == "detailed") {
    freq <- get_frequency_detailed(gtfs)
  }

  if (!method %in% c("by.route", "detailed", 'by.shape')) {
    freq <- get_frequency_byroute(gtfs)
    warning(crayon::cyan('method '), 'should be one of ', crayon::cyan('by.route'), ', ', crayon::cyan('by.shape'), ' or ', crayon::cyan('detailed'), '. Returning ', crayon::cyan('method = by.route.'))
  }

  return(freq)

}

get_frequency_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  if(purrr::is_null(gtfs$trips$direction_id)) {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
      dplyr::reframe(daily.frequency = n()) %>%
      dplyr::select(route_id, daily.frequency, service_pattern, pattern_frequency)

  } else {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
      dplyr::reframe(daily.frequency = n()) %>%
      dplyr::select(route_id, direction_id, daily.frequency, service_pattern, pattern_frequency)

  }

  return(freq)

}

get_frequency_byshape <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  if(purrr::is_null(gtfs$trips$direction_id)) {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
      dplyr::reframe(daily.frequency = n()) %>%
      dplyr::select(route_id, daily.frequency, service_pattern, pattern_frequency) %>%
      left_join(unique(select(gtfs$trips, route_id, shape_id)), by = c('route_id'), relationship = "many-to-many") %>%
      group_by(shape_id, service_pattern, pattern_frequency) %>%
      reframe(daily.frequency = sum(daily.frequency)) %>%
      select(shape_id, daily.frequency, service_pattern, pattern_frequency)

  } else {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
      dplyr::reframe(daily.frequency = n()) %>%
      dplyr::select(route_id, direction_id, daily.frequency, service_pattern, pattern_frequency) %>%
      left_join(unique(select(gtfs$trips, route_id, direction_id, shape_id)), by = c('route_id', 'direction_id')) %>%
      group_by(shape_id, direction_id, service_pattern, pattern_frequency) %>%
      reframe(daily.frequency = sum(daily.frequency)) %>%
      select(shape_id, direction_id, daily.frequency, service_pattern, pattern_frequency)

  }

  return(freq)

}

get_frequency_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  if(purrr::is_null(gtfs$trips$direction_id)) {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::mutate(hour = str_extract(as.character(departure), '\\d+')) %>%
      dplyr::group_by(route_id, hour, service_pattern, pattern_frequency) %>%
      dplyr::reframe(frequency = n()) %>%
      dplyr::select(route_id, hour, frequency, service_pattern, pattern_frequency)

  } else {

    freq <-
      gtfs$stop_times %>%
      dplyr::filter(!arrival_time == '') %>%
      dplyr::group_by(trip_id) %>%
      dplyr::reframe(departure = arrival_time[1]) %>%
      dplyr::left_join(gtfs$trips,
                       by = 'trip_id') %>%
      dplyr::left_join(service_pattern,
                       by = 'service_id',
                       relationship = "many-to-many") %>%
      dplyr::mutate(hour = str_extract(as.character(departure), '\\d+')) %>%
      dplyr::group_by(route_id, direction_id, hour, service_pattern, pattern_frequency) %>%
      dplyr::reframe(frequency = n()) %>%
      dplyr::select(route_id, direction_id, hour, frequency, service_pattern, pattern_frequency)

  }

  return(freq)

}

