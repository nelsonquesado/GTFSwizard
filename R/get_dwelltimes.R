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
    warning('\n"method" should be one of "by.hour", "by.route", "by.trip" or "detailed".\nReturning "method = "by.route"".')
  }
  
  return(dwell_time)
  
}

get_dwelltime_byhour <- function(gtfs, max.dwelltime = 90){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
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
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
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
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  dwell_time <- 
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>% 
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
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
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
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
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    dplyr::select(route_id, trip_id, stop_id, hour, dwell_time, service_pattern, pattern_frequency)
  
  return(dwell_time)
  
}

