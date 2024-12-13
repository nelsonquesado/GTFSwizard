#' Filter GTFS Data by Service, Route, Date, Stop, Trip, and Time
#'
#' The `filter_` functions selectively filter data within a `wizardgtfs` object based on criteria such as service patterns, specific dates, service IDs, route IDs, trip IDs, stop IDs, or time ranges.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param servicepattern (Optional) A character vector of service patterns to retain. Defaults to the most frequent pattern (typical day) if `NULL`.
#' @param dates (Optional) A date or vector of dates (as "YYYY-MM-DD" character or POSIXct) to filter services active on those dates. Return the furtherst available date if `NULL`.
#' @param service (Optional) A character vector of service IDs to retain in the `wizardgtfs` object.
#' @param route (Optional) A character vector of route IDs to retain in the `wizardgtfs` object. When `keep = FALSE`, excludes the specified routes.
#' @param trip (Optional) A character vector of trip IDs to retain in the `wizardgtfs` object. When `keep = FALSE`, excludes the specified trips.
#' @param stop (Optional) A character vector of stop IDs to retain.
#' @param from (Optional) Start time in "HH:MM:SS" format to include only trips that start after this time. Defaults to `0:0:0`.
#' @param to (Optional) End time in "HH:MM:SS" format to include only trips that end before this time. Defaults to `48:00:00`.
#' @param keep Logical. When `TRUE` (default), retains specified `route` or `trip` IDs; when `FALSE`, excludes them.
#'
#' @details
#' Each `filter_` function targets a specific aspect of the GTFS data, applying filters to the relevant tables:
#'
#' - filter_servicepattern: Filters by specified service patterns in the GTFS data. If no pattern is provided, defaults to the most frequent one.
#'
#' - filter_date: Filters data by a date or dates, returning only services active on those dates.
#'
#' - filter_service: Filters by service ID, retaining data related to specified services.
#'
#' - filter_route: Filters by route ID. When `keep = TRUE`, only specified routes are retained; when `FALSE`, the specified routes are excluded.
#'
#' - filter_trip: Filters by trip ID, using `keep` to either retain or exclude specified trips.
#'
#' - filter_stop: Filters by stop ID, retaining only stops and related data (trips, routes, etc.) associated with the specified stops.
#'
#' - filter_time: Filters stop times within a specified time range (between `from` and `to`).
#'
#' These functions selectively subset the GTFS tables (`trips`, `stop_times`, `routes`, `agency`, `shapes`, etc.), maintaining only the records that meet the defined criteria. If a table or required column is missing from the GTFS data, the function will either attempt to infer it using available data or exclude the table as necessary.
#'
#' @return A filtered `wizardgtfs` object containing only the records that match the specified criteria.
#'
#' @examples
#' # Filter by service pattern
#' filtered_gtfs <- filter_servicepattern(gtfs = for_rail_gtfs, servicepattern = "servicepattern-1")
#'
#' # Filter by a specific date
#' filtered_gtfs <- filter_date(gtfs = for_rail_gtfs, dates = "2021-02-10")
#'
#' # Filter by route ID, keeping only specified routes
#' filtered_gtfs <- filter_route(gtfs = for_rail_gtfs, route = for_rail_gtfs$routes$route_id[1:2])
#'
#' # Filter by trip ID, excluding specified trips
#' filtered_gtfs <- filter_trip(gtfs = for_rail_gtfs,
#'                               trip = for_rail_gtfs$trips$trip_id[1:2],
#'                               keep = FALSE)
#'
#' # Filter by a time range
#' filtered_gtfs <- filter_time(gtfs = for_rail_gtfs, from = "06:30:00", to = "10:00:00")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr filter group_by mutate left_join reframe select
#' @importFrom tidyr unnest
#' @importFrom checkmate assert_logical
#' @importFrom stringr str_split

#' @rdname filter_functions
#' @aliases filter_servicepattern
#' @export
filter_servicepattern <- function(gtfs, servicepattern = NULL){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(is.null(servicepattern)){
    warning('\nNo service pattern(s) provided.\nReturning most frequent pattern.')
    servicepattern <- 'servicepattern-1'
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  if(any(!servicepattern %in% unique(service_pattern$service_pattern))){

    stop(paste0('\nService pattern should be one of ',
                paste(unique(service_pattern$service_pattern), collapse = ', '),
                '. Use ', crayon::cyan('get_servicepattern()'), ' function to check service patterns.'))

  }

  service_patterns <-
    service_pattern[service_pattern$service_pattern %in% servicepattern, ] %>%
    dplyr::group_by(service_pattern) %>%
    dplyr::reframe(service_id = list(service_id))

  services <-
    unlist(service_patterns$service_id) %>%
    unique

  gtfs$trips <-
    gtfs$trips[gtfs$trips$service_id %in% services, ]

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  trips <-
    gtfs$trips$trip_id %>%
    unique

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$trip_id %in% trips, ]

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  shapes <-
    gtfs$trips$shape_id %>%
    unique

  if(!purrr::is_null(gtfs$shapes)){
    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

    fares <-
      gtfs$fare_rules$fare_id %>%
      unique
  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  # if(!purrr::is_null(gtfs$calendar_dates)){
  #   gtfs$calendar_dates <-
  #   gtfs$calendar_dates %>%
  #   dplyr::filter(service_id %in% services)
  # }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

    gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_date
#' @export
filter_date <- function(gtfs, dates = NULL){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(is.null('dates')) {
    warning(crayon::red('No date(s) provided.'), ' Returning furtherst date.')
    date <- gtfs$dates_services$date[length(gtfs$dates_services$date)] %>% as.Date()
  } else {
    date <- as.Date(dates)
    }

  if(any(!date %in% as.Date(gtfs$dates_services$date))){
    stop(crayon::red('Date(s) do not belongs to calendar.'), ' Must be either a ', crayon::cyan('YYYY-MM-DD'), ' character vector or a ', crayon::cyan('POSIXct'), ' object. Please use ', crayon::cyan('get_calendar()'), ' to check available dates.')
  }

  services <-
    gtfs$dates_services[as.Date(gtfs$dates_services$date) %in% date, ] %>%
    tidyr::unnest(cols = 'service_id') %>%
    .$service_id

  gtfs$trips <-
    gtfs$trips[gtfs$trips$service_id %in% services, ]

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  trips <-
    gtfs$trips$trip_id %>%
    unique

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$trip_id %in% trips, ]

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  shapes <-
    gtfs$trips$shape_id %>%
    unique

  if(!purrr::is_null(gtfs$shapes)){
    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

    fares <-
      gtfs$fare_rules$fare_id %>%
      unique
  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  if(!purrr::is_null(gtfs$calendar)){
    new.calendar.dates <-
      tibble(service_id = as.character(services),
             start_date = list(date),
             end_date = list(date)) %>%
      tidyr::unnest(cols = c('start_date', 'end_date'))

    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ] %>%
      .[, 1:8] %>%
      dplyr::left_join(new.calendar.dates, by = dplyr::join_by(service_id))
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates[gtfs$calendar_dates$date %in% date, ]
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

    gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_service
#' @export
filter_service <- function(gtfs, service){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(is.null(service)){
    stop('\nNo service(s) provided.\nUse get_servicepattern() to check available services.')
  }

  if(any(!service %in% gtfs$trips$service_id)){
    stop(paste0('\nService(s) should be one of ',
                paste(unique(gtfs$trips$service_id), collapse = ', '),
                '.'))

  }

  services <- service

  gtfs$trips <-
    gtfs$trips[gtfs$trips$service_id %in% services, ]

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  trips <-
    gtfs$trips$trip_id %>%
    unique

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$trip_id %in% trips, ]

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  shapes <-
    gtfs$trips$shape_id %>%
    unique

  if(!purrr::is_null(gtfs$shapes)){
    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

    fares <-
      gtfs$fare_rules$fare_id %>%
      unique
  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

      gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_route
#' @export
filter_route <- function(gtfs, route, keep = TRUE){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(purrr::is_null(route)){
    stop('\nNo route(s) provided.\nRun gtfs$routes to check available routes.')
  }

  if(any(!route %in% gtfs$routes$route_id)){
    stop('\nThere is no such route(s).\nRun gtfs$routes to check available routes.')

  }

  checkmate::assert_logical(keep)

  if(isTRUE(keep)) {
    routes <- route
  }

  if(!isTRUE(keep)) {
    routes <- gtfs$routes$route_id[!gtfs$routes$route_id %in% route]
  }

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  gtfs$trips <-
    gtfs$trips[gtfs$trips$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  trips <-
    gtfs$trips$trip_id %>%
    unique

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$trip_id %in% trips, ]

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  shapes <-
    gtfs$trips$shape_id %>%
    unique

  if(!purrr::is_null(gtfs$shapes)){
    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

    fares <-
      gtfs$fare_rules$fare_id %>%
      unique
  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  services <-
    gtfs$trips$service_id %>%
    unique

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

    gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_trip
#' @export
filter_trip <- function(gtfs, trip, keep = TRUE){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the ', crayon::cyan('gtfswizard'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }


  if(is.null(trip)){
    stop('\nNo ', crayon::cyan('trip(s)'), ' provided. Run ', crayon::cyan('gtfs$trips'), ' to check available trips.')
  }

  if(any(!trip %in% gtfs$trips$trip_id)){
    stop('\nThere is no such ', crayon::cyan('trip(s)'), '. Run ', crayon::cyan('gtfs$trips'), ' to check available trips.')

  }

  checkmate::assert_logical(keep)

  if(isTRUE(keep)) {
    trips <- trip
  }

  if(!isTRUE(keep)) {
    trips <- gtfs$trips$trip_id[!gtfs$trips$trip_id %in% trip]
  }

  gtfs$trips <-
    gtfs$trips[gtfs$trips$trip_id %in% trips, ]

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$trip_id %in% trips, ]

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  shapes <-
    gtfs$trips$shape_id %>%
    unique

  if(!purrr::is_null(gtfs$shapes)){
    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

    fares <-
      gtfs$fare_rules$fare_id %>%
      unique
  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  services <-
    gtfs$trips$service_id %>%
    unique

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

    gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_stop
#' @export
filter_stop <- function(gtfs, stop){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(is.null(stop)){
    stop('\nNo stop(s) provided.\nRun gtfs$stops to check available stops.')
  }

  if(any(!stop %in% gtfs$stops$stop_id)){
    stop('\nThere is no such stop(s).\nRun gtfs$stops to check available stops.')

  }

  stops <- stop

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  gtfs$stop_times <-
    gtfs$stop_times[gtfs$stop_times$stop_id %in% stops, ]

  trips <-
    gtfs$stop_times$trip_id %>%
    unique

  gtfs$trips <-
    gtfs$trips[gtfs$trips$trip_id %in% trips, ]

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  if(!purrr::is_null(gtfs$shapes)){
    shapes <-
      gtfs$trips$shape_id %>%
      unique

    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    fares <-
      gtfs$fare_rules$fare_id %>%
      unique

    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  services <-
    gtfs$trips$service_id %>%
    unique

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

      gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

#' @rdname filter_functions
#' @aliases filter_time
#' @export
filter_time <- function(gtfs, from = '0:0:0', to = "48:00:00"){

  message(crayon::cyan('filter_time()'), crayon::red(' removes'), ' invalid stop times.') # evitar isso em uma proxima versao

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(suppressWarnings(is.na(stringr::str_split(from, ":") %>%
                            lapply(FUN = as.numeric)))){
    stop('Wrong ', crayon::cyan('from'), ' time format. Please use ', crayon::cyan('HH:MM:SS'), '.')
  }

  if(suppressWarnings(is.na(stringr::str_split(to, ":") %>%
                            lapply(FUN = as.numeric)))){
    stop('Wrong ', crayon::cyan('to'), ' time format. Please use ', crayon::cyan('HH:MM:SS'), '.')
  }

  from <-
    stringr::str_split(from, ":") %>%
    lapply(FUN = as.numeric) %>%
    lapply(FUN = function(x){
      x[1]*60*60+x[2]*60+x[3]
    }) %>%
    unlist %>%
    na.omit()

  to <-
    stringr::str_split(to, ":") %>%
    lapply(FUN = as.numeric) %>%
    lapply(FUN = function(x){
      x[1]*60*60+x[2]*60+x[3]
    }) %>%
    unlist %>%
    na.omit()

  gtfs$stop_times <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::mutate(arrival_filter = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_filter = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit()) %>%
    dplyr::filter(arrival_filter >= from & arrival_filter <= to & departure_filter >= from & departure_filter <= to) %>%
    dplyr::select(-arrival_filter, -departure_filter)

  stops <-
    gtfs$stop_times$stop_id %>%
    unique

  gtfs$stops <-
    gtfs$stops[gtfs$stops$stop_id %in% stops, ]

  trips <-
    gtfs$stop_times$trip_id %>%
    unique

  gtfs$trips <-
    gtfs$trips[gtfs$trips$trip_id %in% trips, ]

  services <-
    gtfs$trips$service_id %>%
    unique

  routes <-
    gtfs$trips$route_id %>%
    unique

  gtfs$routes <-
    gtfs$routes[gtfs$routes$route_id %in% routes, ]

  agencies <-
    gtfs$routes$agency_id %>%
    unique

  gtfs$agency <-
    gtfs$agency[gtfs$agency$agency_id %in% agencies, ]

  if(!purrr::is_null(gtfs$shapes)){
    shapes <-
      gtfs$trips$shape_id %>%
      unique

    gtfs$shapes <-
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }

  if(!purrr::is_null(gtfs$fare_rules)){
    gtfs$fare_rules <-
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]

  }

  if(!purrr::is_null(gtfs$fare_attributes)){
    fares <-
      gtfs$fare_rules$fare_id %>%
      unique

    gtfs$fare_attributes <-
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }

  if(!purrr::is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }

  if(!purrr::is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }

  if(!purrr::is_null(gtfs$frequencies)){
    gtfs$frequencies <-
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }

  if(!purrr::is_null(gtfs$transfers)){
    gtfs$transfers <-
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }

  if(!purrr::is_null(gtfs$dates_services)){

      gtfs$dates_services <- NULL

  }

  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  return(gtfs)

}

# inserir filter_polygon ou filter_space
# stops <- st_filter(as_stops_sf(gtfs$stops) %>% st_make_valid(), shp.de.interesse) %>% .$stop_id
# gtfs <- filter_stop(gtfs, stops)
