#' Identify and Extract Stop Hubs Based on Transfers and Routes
#'
#' `get_stop_hubs` identifies transit hubs by analyzing connections between stops, considering walking distances, operating hours, and service patterns.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param walk_distance A numeric value specifying the walking distance (in meters) used to group stops into hubs. Defaults to `100`.
#' @param hour A numeric value or the string `"all"`, indicating the hour(s) of the day (0-24) for which to analyze transfers. Must be either a number in the range `0:24` or `"all"`. This parameter is required.
#' @param service A vector of service pattern identifiers. Defaults to the result of `higher_servicepattern()`.
#'
#' @return A `wzd_hubs` object (a specialized tibble) with the following columns:
#' \describe{
#'   \item{stop_id}{The ID of the stop identified as a hub.}
#'   \item{n_routes}{The number of unique routes passing through the hub.}
#'   \item{n_trips}{The number of unique trips passing through the hub.}
#' }
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Groups stops into hubs based on proximity (using `walk_distance`) and spatial buffers.
#'   \item Filters trips and stops based on the specified `hour` and service patterns.
#'   \item Calculates the number of unique routes and trips associated with each hub.
#' }
#'
#' @note
#' - Ensure the `gtfs` object contains `stop_times`, `trips`, and `stops` tables.
#'
#' - If `hour` is missing or invalid, an error will be raised.
#'
#' @examples
#' # Identify hubs for a GTFS object during peak hours (7 AM)
#' hubs <- get_stop_hubs(for_bus_gtfs, walk_distance = 150, hour = 7)
#'
#' # Analyze hubs for all hours and default service patterns
#' hubs <- get_stop_hubs(for_bus_gtfs, walk_distance = 200, hour = "all")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_stops_sf()]
#'
#' @importFrom dplyr select mutate group_by ungroup arrange reframe left_join filter
#' @importFrom sf st_buffer st_intersects
#' @importFrom stringr str_extract
#' @importFrom tidyr unnest
#' @export
get_stop_hubs <- function(gtfs,
                          walk_distance=100,
                          hour,
                          service=higher_servicepattern){

  if(missing('hour')){
    stop("'hour' is missing with no default")
  }
  if(is.character(hour)){
    if(hour!='all'){
      stop("Assertion on 'hour' failed: Must be a numeric between 0:24 or 'all'.")
    }
  }else{
    if(hour %in% 0:24 == FALSE | !is.numeric(hour)){
      stop("Assertion on 'hour' failed: Must be a numeric between 0:24 or 'all'.")
    }
  }


  UseMethod('get_stop_hubs')
}

#' @exportS3Method GTFSwizard::get_stop_hubs list
get_stop_hubs.list <- function(gtfs,
                                     walk_distance=100,
                                     hour,
                                     service=higher_servicepattern){
  message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  gtfs <- as_wizardgtfs(gtfs)
  get_stop_hubs.wizardgtfs(gtfs,walk_distance,hour,service)
}

#' @exportS3Method GTFSwizard::get_stop_hubs wizardgtfs
get_stop_hubs.wizardgtfs <- function(gtfs,
                          walk_distance=100,
                          hour,
                          service=higher_servicepattern){
  service_pattern <- get_servicepattern(gtfs)

  if(hour=='all'){
    hour <-  0:24
  }
  if(is.function(service)){
    service <-  service(gtfs)
  }
  stops_groups <- get_stops_sf(gtfs$stops) %>%
    latlon2epsg() %>%
    sf::st_buffer(walk_distance) %>%
    sf::st_intersects()

  stops_groups <- lapply(stops_groups, function(x) sort(gtfs$stops$stop_id[x]))

  stops_groups <- gtfs$stops %>%
    dplyr::mutate(groups = stops_groups)
  h <- hour
  transfers <- gtfs$stop_times %>%
    dplyr::select(trip_id,stop_id,stop_sequence,departure_time) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(stop_sequence,.by_group = T) %>%
    dplyr::mutate(next_stop = dplyr::lead(stop_id)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(next_stop = ifelse(is.na(next_stop),"",next_stop)) %>%
    dplyr::filter(!is.na(departure_time)&departure_time!="") %>%
    dplyr::left_join(gtfs$trips %>%
                       dplyr::select(trip_id,route_id,service_id),by = 'trip_id') %>%
    dplyr::mutate(hour = as.numeric(stringr::str_extract(departure_time,'^\\d{2}'))) %>%
    dplyr::mutate(hour = ifelse(hour>=24,hour-24,hour)) %>%
    dplyr::filter(hour %in% h) %>%
    dplyr::left_join(service_pattern,by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::filter(service_pattern %in% service) %>%
    dplyr::left_join(
      stops_groups %>% dplyr::select(stop_id,groups),
      by = 'stop_id'
    ) %>%
    dplyr::group_by(hour,service_pattern,pattern_frequency) %>%
    dplyr::reframe(
      hubs = do_hubs(stop_id,groups,next_stop,trip_id,route_id)
    ) %>%
    tidyr::unnest('hubs')

  attr(transfers,'stop_position') <- gtfs$stops %>% dplyr::select(stop_id,stop_lon,stop_lat) %>% unique()

  class(transfers) <- c('wzd_hubs',class(transfers))

  return(transfers)

}


do_hubs <- function(stop_id,groups,next_stop,trip_id,route_id){
  df <- tibble::tibble(
    `stop_id` = stop_id,
    `groups` = groups,
    `next_stop` = next_stop,
    `trip_id` = trip_id,
    `route_id` = route_id
  )
  df %>%
    dplyr::group_by(stop_id) %>%
    dplyr::reframe(
      n_routes = get_n_routes(stops = unique(unlist(groups)),df = df)
    ) %>%
    tidyr::unnest('n_routes')

}



get_n_routes <- function(stops,df){
  df %>%
    dplyr::filter(stop_id %in% stops) %>%
    dplyr::group_by(stop_id,next_stop) %>%
    dplyr::mutate(routes_similary = list(sort(unique(route_id)))) %>%
    dplyr::ungroup() %>%
    dplyr::reframe(
      n_routes = length(unique(routes_similary)),
      n_trips = length(unique(trip_id))
    )

}


