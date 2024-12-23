#' Calculate Dwell Times in GTFS Data
#'
#' The `get_dwelltimes` function calculates dwell times within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide average dwell times per route, per trip, by hour, or detailed dwell times at each stop.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param max.dwelltime Numeric. The maximum allowable dwell time (in seconds). Dwell times exceeding this value are excluded from the calculations. Defaults to 90 seconds.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.hour"}{Calculates the average dwell time per hour of the day across all trips.}
#'     \item{"by.route"}{Calculates the average dwell time for each route.}
#'     \item{"by.trip"}{Calculates the average dwell time for each trip.}
#'     \item{"detailed"}{Calculates detailed dwell times at each stop within every trip.}
#'   }
#'
#' @return A data frame containing dwell times based on the specified method:
#'   \describe{
#'     \item{If `method = "by.hour"`}{Returns a data frame with columns: `hour`, `trips`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `trips`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `trip_id`, `stop_id`, `hour`, `dwell_time`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.hour": Calculates the average dwell time for each hour of the day.
#'
#' - "by.route": Calculates average dwell times across each route.
#'
#' - "by.trip": Calculates the total dwell time for each trip.
#'
#' - "detailed": Calculates the dwell time between consecutive stops within each trip.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' # Calculate dwell times by hour
#' dwelltimes_by_hour <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 120, method = "by.hour")
#'
#' # Calculate dwell times by route
#' dwelltimes_by_route <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 90, method = "by.route")
#'
#' # Calculate dwell times by trip
#' dwelltimes_by_trip <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 45, method = "by.trip")
#'
#' # Calculate detailed dwell times between stops
#' detailed_dwelltimes <- get_dwelltimes(gtfs = for_rail_gtfs, max.dwelltime = 60, method = "detailed")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter
#' @importFrom stringr str_split str_extract
#' @export
get_dwelltimes <- function(gtfs, max.dwelltime = 90, method = 'by.route'){

  if (method == 'by.hour') {
    dwell_time <- get_dwelltime_byhour(gtfs, max.dwelltime = max.dwelltime)
  }

  if (method == 'by.route') {
    dwell_time <- get_dwelltime_byroute(gtfs, max.dwelltime = max.dwelltime)
  }

  if (method == 'by.trip') {
    dwell_time <- get_dwelltime_bytrip(gtfs, max.dwelltime = max.dwelltime)
  }

  if (method == 'detailed') {
    dwell_time <- get_dwelltime_detailed(gtfs, max.dwelltime = max.dwelltime)
  }

  if (!method %in% c('by.hour', 'by.route', 'detailed', 'by.trip')) {
    dwell_time <- get_dwelltime_byroute(gtfs)
    warning(crayon::cyan('method'), ' should be one of ', crayon::cyan('by.hour'), ',', crayon::cyan(' by.route'), ',', crayon::cyan(' by.trip'), ', or ', crayon::cyan('detailed'), '. Returning ', crayon::cyan('method = "by.route"'),'.')
  }

  return(dwell_time)

}

get_dwelltime_byhour <- function(gtfs, max.dwelltime = 90){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  dwell_time <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(arrival_time, departure_time, service_pattern, pattern_frequency) %>%
    dplyr::reframe(n = n()) %>%
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>%
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::group_by(hour, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average.dwelltime = weighted.mean(dwell_time, n),
                   trips = n()) %>%
    dplyr::select(hour, trips, average.dwelltime, service_pattern, pattern_frequency)

  return(dwell_time)

}

get_dwelltime_byroute <- function(gtfs, max.dwelltime = 90){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  dwell_time <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(route_id, trip_id, arrival_time, departure_time, service_pattern, pattern_frequency) %>%
    dplyr::reframe(n = n()) %>%
    dplyr::mutate(arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>%
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average.dwelltime = weighted.mean(dwell_time, n),
                   trips = n()) %>%
    dplyr::select(route_id, trips, average.dwelltime, service_pattern, pattern_frequency)

  return(dwell_time)

}

get_dwelltime_bytrip <- function(gtfs, max.dwelltime = 90){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  dwell_time <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(route_id, trip_id, arrival_time, departure_time, service_pattern, pattern_frequency) %>%
    dplyr::reframe(n = n()) %>%
    dplyr::mutate(arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>%
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::group_by(route_id, trip_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average.dwelltime = weighted.mean(dwell_time, n)) %>%
    dplyr::select(route_id, trip_id, average.dwelltime, service_pattern, pattern_frequency)

  return(dwell_time)

}

get_dwelltime_detailed <- function(gtfs, max.dwelltime = 90){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  dwell_time <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+") %>% as.numeric(),
                  arrival_time = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  dwell_time = departure_time - arrival_time
    ) %>%
    dplyr::filter(dwell_time <= max.dwelltime) %>%
    dplyr::left_join(gtfs$trips, by = dplyr::join_by(trip_id)) %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::select(route_id, trip_id, stop_id, hour, dwell_time, service_pattern, pattern_frequency)

  return(dwell_time)

}

