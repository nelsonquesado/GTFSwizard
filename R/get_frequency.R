get_frequency <- function(gtfs, method = 'by.route'){
  
  if (method == "by.route") {
    freq <- get_frequency_byroute(gtfs)
  }
  
  if (method == "detailed") {
    freq <- get_frequency_detailed(gtfs)
  }
  
  if (!method %in% c("by.route", "detailed")) {
    freq <- get_frequency_byroute(gtfs)
    warning('\n"method" should be one of "by.route" or "detailed".\nReturning "method = "by.route"".')
  }
  
  return(freq)
  
}

get_frequency_byroute <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
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
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, daily.frequency, service_pattern, pattern_frequency)
  
  return(freq)
  
}

get_frequency_detailed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
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
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, hour, frequency, service_pattern, pattern_frequency)
  
  return(freq)
  
}

