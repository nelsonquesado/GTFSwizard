#' Calculate Headways in GTFS Data
#'
#' The `get_headways` function calculates headways within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide average headways by route, by trip, by hour, or detailed stop-level headways.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.route"}{Calculates the average headway for each route, assuming constant headways along stops.}
#'     \item{"by.hour"}{Calculates the hourly headway for each route, assuming constant headways along stops.}
#'     \item{"by.trip"}{Calculates headways for each trip, assuming constant headways along stops.}
#'     \item{"detailed"}{Calculates detailed headways between consecutive stops within each route and trip.}
#'   }
#'
#' @return A data frame containing service headways based on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `trips`, `average.headway`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.hour"`}{Returns a data frame with columns: `hour`, `trips`, `average.headway`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `headway`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `trip_id`, `stop_id`, `hour`, `headway`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.route": Calculates the average headway for each route based on the first stop time per trip.
#'
#' - "by.hour": Calculates the hourly headway for each route, grouping trips by hour.
#'
#' - "by.trip": Calculates headways for each trip, considering only the first stop time.
#'
#' - "detailed": Provides headway calculations for each consecutive stop within each trip.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' # Calculate average route headways
#' headways_by_route <- get_headways(gtfs = for_rail_gtfs, method = "by.route")
#'
#' # Calculate hourly headways
#' headways_by_hour <- get_headways(gtfs = for_rail_gtfs, method = "by.hour")
#'
#' # Calculate headways for each trip
#' headways_by_trip <- get_headways(gtfs = for_rail_gtfs, method = "by.trip")
#'
#' # Calculate detailed stop-level headways
#' detailed_headways <- get_headways(gtfs = for_rail_gtfs, method = "detailed")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter ungroup
#' @importFrom stringr str_split str_extract
#' @export

get_headways <- function(gtfs, method = 'by.route'){

  if (method == 'by.hour') {
    message('This method assumes constant headways along stops.')
    hw <- get_headway_byhour(gtfs)
  }

  if (method == 'by.route') {
    message('This method assumes constant headways along stops.')
    hw <- get_headway_byroute(gtfs)
  }

  if (method == 'by.trip') {
    message('This method assumes constant headways along stops.')
    hw <- get_headway_bytrip(gtfs)
  }

  if (method == 'detailed') {
    hw <- get_headway_detailed(gtfs)
  }

  if (!method %in% c("by.route", 'detailed', 'by.trip', 'by.hour')) {
    hw <- get_headway_byroute(gtfs)
    warning(crayon::cyan("method"), ' should be one of ', crayon::cyan("by.hour"), ', ', crayon::cyan("by.route"), ', ', crayon::cyan("by.trip"), ' or ', crayon::cyan("detailed"), '. Returning  ', crayon::cyan('method = "by.route"'), '.')
  }

  return(hw)

}

get_headway_byhour <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  hw <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    group_by(route_id, trip_id, direction_id, service_pattern, pattern_frequency) %>%
    reframe(arrival_time = arrival_time[1]) %>%
    dplyr::mutate(hour = str_extract(arrival_time, '\\d+'),
                  arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1] * 60 * 60 + x[2] * 60 + x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
    ) %>%
    dplyr::arrange(route_id, direction_id, arrival_time) %>%
    dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
    dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time) / 60) %>%
    dplyr::filter(headway.minutes >= 0) %>%
    dplyr::group_by(hour, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average_headway_minutes = mean(headway.minutes, na.rm = TRUE),
                   valid_trips = n()) %>%
    dplyr::select(hour, average_headway_minutes, valid_trips, service_pattern, pattern_frequency) %>%
    na.omit()

  return(hw)

}

get_headway_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  hw <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    group_by(route_id, trip_id, direction_id, service_pattern, pattern_frequency) %>%
    reframe(arrival_time = arrival_time[1]) %>%
    dplyr::mutate(arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1] * 60 * 60 + x[2] * 60 + x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
    ) %>%
    dplyr::arrange(route_id, direction_id, arrival_time) %>%
    dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
    dplyr::mutate(headway_minutes = (lead(arrival_time) - arrival_time) / 60) %>%
    dplyr::filter(headway_minutes >= 0) %>%
    dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average_headway_minutes = mean(headway_minutes, na.rm = TRUE),
                   valid_trips = n()) %>%
    dplyr::select(route_id, direction_id, average_headway_minutes, valid_trips, service_pattern, pattern_frequency) %>%
    na.omit()

  return(hw)

}

get_headway_bytrip <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  hw <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    group_by(route_id, trip_id, direction_id, service_pattern, pattern_frequency) %>%
    reframe(arrival_time = arrival_time[1]) %>% # assume headway constante ao longo das paradas
    dplyr::mutate(arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1] * 60 * 60 + x[2] * 60 + x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
    ) %>%
    dplyr::arrange(route_id, direction_id, arrival_time) %>%
    dplyr::group_by(route_id, direction_id, service_pattern, pattern_frequency) %>%
    dplyr::mutate(headway_minutes = (lead(arrival_time) - arrival_time) / 60) %>%
    dplyr::filter(headway_minutes >= 0) %>%
    stats::na.omit() %>%
    dplyr::select(route_id, trip_id, direction_id, headway_minutes, service_pattern, pattern_frequency) %>%
    dplyr::ungroup()


  return(hw)

}

get_headway_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  hw <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
    ) %>%
    dplyr::left_join(gtfs$trips, by = 'trip_id') %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::arrange(route_id, direction_id, arrival_time) %>%
    dplyr::group_by(route_id, direction_id, stop_id, service_pattern, pattern_frequency) %>%
    dplyr::mutate(headway_minutes = (lead(arrival_time) - arrival_time) / 60) %>%
    dplyr::filter(headway_minutes >= 0) %>%
    ungroup() %>%
    dplyr::select(route_id, trip_id, direction_id, stop_id, hour, headway_minutes, service_pattern, pattern_frequency) %>%
    na.omit()

  return(hw)

}

