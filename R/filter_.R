filter_servicepattern <- function(gtfs, servicepattern = NULL){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is.null(servicepattern)){
    warning('\nNo service pattern(s) provided.\nReturning most frequent pattern.')
    servicepattern <- 'servicepattern-1'
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  if(any(!servicepattern %in% unique(service_pattern$service_pattern))){
    message(paste0('\nService pattern should be one of ', 
                   paste(unique(service_pattern$service_pattern), collapse = ', '),
                   '.',
                   '\nUse get_paservicepattern() function to check service patterns.'))
    stop()
    
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
  
  if(!is_null(gtfs$shapes)){
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
  }
  
  if(!is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <-
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  # if(!is_null(gtfs$calendar_dates)){ 
  #   gtfs$calendar_dates <- 
  #   gtfs$calendar_dates %>% 
  #   dplyr::filter(service_id %in% services)
  # }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <- 
        gtfs$dates_services %>% 
        dplyr::left_join(service_patterns, by = 'service_id') %>% 
        na.omit() %>% 
        .[1:2]
    )
  }
  
  return(gtfs)
  
}

filter_date <- function(gtfs, date = NULL){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as_gtfswizard() is advised.')
  }
  
  if(is.null(date)) {
    warning('\nNo date(s) provided.\nReturning furtherst date.')
    date <- gtfs$dates_services$date[length(gtfs$dates_services$date)] %>% as.Date()
  } else {
    date <- as.Date(date)
    }
  
  if(any(!date %in% as.Date(gtfs$dates_services$date))){
    message('\nDate(s) do not belongs to calendar.\nMust be either a "YYYY-MM-DD" character vector or a POSIXct object.\nPlease use get_calendar() to check available dates.')
    stop()
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
  
  if(!is_null(gtfs$shapes)){
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
  }
  
  if(!is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  if(!is_null(gtfs$calendar)){
    new.calendar.dates <- 
      tibble(service_id = services,
             start_date = list(date),
             end_date = list(date)) %>% 
      tidyr::unnest(cols = c('start_date', 'end_date'))
    
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ] %>% 
      .[, 1:8] %>% 
      dplyr::left_join(new.calendar.dates, by = join_by(service_id))
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates[gtfs$calendar_dates$date %in% date, ]
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <- 
        gtfs$dates_services[as.Date(gtfs$dates_services$date) %in% date, ]
    )
  }
  
  return(gtfs)
  
}

filter_service <- function(gtfs, service = NULL){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is.null(service)){
    message('\nNo service(s) provided.\nUse get_servicepattern() to check available services.')
    stop()
  }
  
  if(any(!service %in% gtfs$trips$service_id)){
    message(paste0('\nService(s) should be one of ', 
                   paste(unique(gtfs$trips$service_id), collapse = ', '),
                   '.'))
    stop()
    
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
  
  if(!is_null(gtfs$shapes)){
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
  }
  
  if(!is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <-
        gtfs$dates_services %>% 
        tidyr::unnest(cols = 'service_id') %>% 
        dplyr::filter(service_id %in% services) %>% 
        dplyr::group_by(date) %>% 
        dplyr::reframe(service_id = list(service_id))
      
    )
  }
  
  return(gtfs)
  
}

filter_route <- function(gtfs, route = NULL, keep = TRUE){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is.null(route)){
    message('\nNo route(s) provided.\nRun gtfs$routes to check available routes.')
    stop()
  }
  
  if(any(!route %in% gtfs$routes$route_id)){
    message('\nThere is no such route(s).\nRun gtfs$routes to check available routes.')
    stop()
    
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
  
  if(!is_null(gtfs$shapes)){
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
  }
  
  if(!is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  services <- 
    gtfs$trips$service_id %>% 
    unique
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <-
        gtfs$dates_services %>%
        tidyr::unnest(cols = 'service_id') %>% 
        dplyr::filter(service_id %in% services) %>% 
        dplyr::group_by(date) %>% 
        dplyr::reframe(service_id = list(service_id))
      
    )
  }
  
  return(gtfs)
  
}

filter_trip <- function(gtfs, trip = NULL, keep = TRUE){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  
  if(is.null(trip)){
    message('\nNo trip(s) provided.\nRun gtfs$trips to check available trips.')
    stop()
  }
  
  if(any(!trip %in% gtfs$trips$trip_id)){
    message('\nThere is no such trip(s).\nRun gtfs$trips to check available trips.')
    stop()
    
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
  
  if(!is_null(gtfs$shapes)){
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
  }
  
  if(!is_null(gtfs$fare_attributes)){
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  services <- 
    gtfs$trips$service_id %>% 
    unique
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <- 
        gtfs$dates_services %>% 
        tidyr::unnest(cols = service_id) %>% 
        dplyr::filter(service_id %in% services) %>% 
        dplyr::group_by(date) %>% 
        dplyr::reframe(service_id = list(service_id))
    )
  }
  
  return(gtfs)
  
}

filter_stop <- function(gtfs, stop = NULL){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is.null(stop)){
    message('\nNo stop(s) provided.\nRun gtfs$stops to check available stops.')
    stop()
  }
  
  if(any(!stop %in% gtfs$stops$stop_id)){
    message('\nThere is no such stop(s).\nRun gtfs$stops to check available stops.')
    stop()
    
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
  
  if(!is_null(gtfs$shapes)){
    shapes <- 
      gtfs$trips$shape_id %>% 
      unique
    
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
  }
  
  if(!is_null(gtfs$fare_attributes)){
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
    
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  services <- 
    gtfs$trips$service_id %>% 
    unique
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <- 
        gtfs$dates_services %>% 
        tidyr::unnest(cols = service_id) %>% 
        dplyr::filter(service_id %in% services) %>% 
        dplyr::group_by(date) %>% 
        dplyr::reframe(service_id = list(service_id))
    )
  }
  
  return(gtfs)
  
}

filter_time <- function(gtfs, from = '0:0:0', to = "48:00:00"){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(suppressWarnings(is.na(stringr::str_split(from, ":") %>% 
                            lapply(FUN = as.numeric)))){
    message('Wrong "from" time format. Please use "HH:MM:SS".')
    stop()
  }
  
  if(suppressWarnings(is.na(stringr::str_split(to, ":") %>% 
                            lapply(FUN = as.numeric)))){
    message('Wrong "to" time format. Please use "HH:MM:SS".')
    stop()
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
  
  if(!is_null(gtfs$shapes)){
    shapes <- 
      gtfs$trips$shape_id %>% 
      unique
    
    gtfs$shapes <- 
      gtfs$shapes[gtfs$shapes$shape_id %in% shapes, ]
  }
  
  if(!is_null(gtfs$fare_rules)){
    gtfs$fare_rules <- 
      gtfs$fare_rules[gtfs$fare_rules$route_id %in% routes, ]
    
  }
  
  if(!is_null(gtfs$fare_attributes)){
    fares <- 
      gtfs$fare_rules$fare_id %>% 
      unique
    
    gtfs$fare_attributes <- 
      gtfs$fare_attributes[gtfs$fare_attributes$fare_id %in% fares, ]
  }
  
  if(!is_null(gtfs$calendar)){
    gtfs$calendar <- 
      gtfs$calendar[gtfs$calendar$service_id %in% services, ]
  }
  
  if(!is_null(gtfs$calendar_dates)){
    gtfs$calendar_dates <-
      gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% services)
  }
  
  if(!is_null(gtfs$frequencies)){
    gtfs$frequencies <- 
      gtfs$frequencies[gtfs$frequencies$trip_id %in% trips, ]
  }
  
  if(!is_null(gtfs$transfers)){
    gtfs$transfers <- 
      gtfs$transfers[gtfs$transfers$stop_id %in% stops, ]
  }
  
  if(!is_null(gtfs$dates_services)){
    suppressWarnings(
      gtfs$dates_services <- 
        gtfs$dates_services %>% 
        tidyr::unnest(cols = 'service_id') %>% 
        dplyr::filter(service_id %in% services) %>% 
        dplyr::group_by(date) %>% 
        dplyr::reframe(service_id = list(service_id))
    )
  }
  
  return(gtfs)
  
  message('filter_time() removes invalid stop times.')
  
}

# inserir filter_polygon ou filter_space
# stops <- st_filter(as_stops_sf(gtfs$stops) %>% st_make_valid(), shp.de.interesse) %>% .$stop_id
# gtfs <- filter_stop(gtfs, stops)
