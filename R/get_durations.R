get_durations <- function(gtfs, method = 'by.route'){
  
  if (method == 'by.route') {
    durations <- get_durations_byroute(gtfs)
  }
  
  if (method == 'by.trip') {
    durations <- get_durations_bytrip(gtfs)
  }
  
  if (method == 'detailed') {
    durations <- get_durations_detailed(gtfs)
  }
  
  if (!method %in% c('by.route', 'detailed', 'by.trip')) {
    durations <- get_durations_byroute(gtfs)
    warning('\n"method" should be one of "by.route", "by.trip" or "detailed".\nReturning "method = by.route"".')
  }
  
  return(durations)
  
}

get_durations_byroute <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  durations <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(starts = arrival_time[1] %>% 
                     stringr::str_split(":") %>% 
                     lapply(FUN = as.numeric) %>% 
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>% 
                     unlist() %>% 
                     na.omit(),
                   ends = arrival_time[n()] %>% 
                     stringr::str_split(":") %>% 
                     lapply(FUN = as.numeric) %>% 
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>% 
                     unlist() %>% 
                     na.omit(),
                   duration = ends - starts) %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(average.duration = mean(duration),
                   trips = n()) %>% 
    dplyr::select(route_id, trips, average.duration, service_pattern, pattern_frequency)
  
  return(durations)
  
}

get_durations_bytrip <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  durations <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(starts = arrival_time[1] %>% 
                     stringr::str_split(":") %>% 
                     lapply(FUN = as.numeric) %>% 
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>% 
                     unlist() %>% 
                     na.omit(),
                   ends = arrival_time[n()] %>% 
                     stringr::str_split(":") %>% 
                     lapply(FUN = as.numeric) %>% 
                     lapply(FUN = function(x){
                       x[1]*60*60+x[2]*60+x[3]
                     }) %>% 
                     unlist() %>% 
                     na.omit(),
                   duration = ends - starts) %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    dplyr::select(route_id, trip_id, duration, service_pattern, pattern_frequency)
  
  return(durations)
  
}

get_durations_detailed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  durations <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>% 
    dplyr::mutate(hour = str_extract(arrival_time, "\\d+"),
                  arrival_time = arrival_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>% 
                    unlist() %>% 
                    na.omit(),
                  departure_time = departure_time %>% 
                    stringr::str_split(":") %>% 
                    lapply(FUN = as.numeric) %>% 
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>% 
                    unlist() %>% 
                    na.omit()
    ) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::mutate(from_stop_id = stop_id,
                  to_stop_id = lead(stop_id),
                  duration = lead(arrival_time)  - departure_time) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    dplyr::select(route_id, trip_id, hour, from_stop_id, to_stop_id, duration, service_pattern, pattern_frequency) %>% 
    stats::na.omit()
  
  return(durations)
  
}

