get_speeds <- function(gtfs, method = 'by.route'){
  
  if(is_null(gtfs$shapes)){
    
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

