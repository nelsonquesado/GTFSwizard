get_fleet <- function(gtfs, method = 'by.route'){
  
  if (method == 'by.route') {
    durations <- get_fleet_byroute(gtfs)
  }
  
  if (method == 'by.hour') {
    durations <- get_fleet_byhour(gtfs)
  }
  
  if (method == 'peak') {
    durations <- get_fleet_peak(gtfs)
  }
  
  if (method == 'detailed') {
    durations <- get_fleet_detailed(gtfs)
  }
  
  if (!method %in% c('by.route', 'detailed', 'peak', 'by.hour')) {
    durations <- get_durations_byroute(gtfs)
    warning('\n"method" should be one of "by.route", "by.hour", "peak" or "detailed".\nReturning "method = by.route"".')
  }
  
  return(durations)
  
}

get_fleet_byroute <- function(gtfs){
  
  message('\nThis method returns the maximum number of simultaneous trips for a given route.')
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  time_points <- 
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
                     na.omit()) %>% 
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::select(-trip_id) %>% 
    tidyr::pivot_longer(cols = 1:2) %>% 
    dplyr::select(-name) %>% 
    dplyr::arrange(value) %>% 
    unique() %>% 
    dplyr::mutate(name = paste0('timepoint-', 1:nrow(.)) %>% as_factor())
  
  fleet <-
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
                     na.omit()) %>%
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::left_join(time_points %>% dplyr::rename(starts = value, name.starts = name),
                     by = 'starts') %>%
    dplyr::left_join(time_points %>% dplyr::rename(ends = value, name.ends = name),
                     by = 'ends') %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    tidyr::pivot_longer(cols = c('name.starts', 'name.ends')) %>% 
    dplyr::group_by(route_id, name, value, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(n = n()) %>%
    tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::mutate(net.fleet = name.starts - name.ends) %>% 
    dplyr::filter(!net.fleet == 0) %>% 
    dplyr::arrange(service_pattern, value) %>% 
    dplyr::group_by(service_pattern, route_id) %>% 
    dplyr::mutate(fleet = cumsum(net.fleet)) %>% 
    dplyr::left_join(time_points %>% setNames(c('time', 'value')), by = 'value') %>% 
    dplyr::mutate(fleet = fleet - min(fleet)) %>% 
    dplyr::group_by(service_pattern, pattern_frequency, route_id) %>% 
    dplyr::reframe(fleet = max(fleet)) %>% 
    dplyr::select(route_id, fleet, service_pattern, pattern_frequency)
  
  return(fleet)
  
}

get_fleet_byhour <- function(gtfs){
  
  message('\nThis method returns the maximum number of simultaneous trips for a given hour.')
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  time_points <- 
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
                     na.omit()) %>% 
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::select(-trip_id) %>% 
    tidyr::pivot_longer(cols = 1:2) %>% 
    dplyr::select(-name) %>% 
    dplyr::arrange(value) %>% 
    unique() %>% 
    dplyr::mutate(name = paste0('timepoint-', 1:nrow(.)) %>% as_factor())
  
  fleet <-
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
                     na.omit()) %>%
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::left_join(time_points %>% dplyr::rename(starts = value, name.starts = name),
                     by = 'starts') %>%
    dplyr::left_join(time_points %>% dplyr::rename(ends = value, name.ends = name),
                     by = 'ends') %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    tidyr::pivot_longer(cols = c('name.starts', 'name.ends')) %>% 
    dplyr::group_by(route_id, name, value, service_pattern, pattern_frequency, starts) %>% 
    dplyr::reframe(n = n()) %>%
    tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::mutate(net.fleet = name.starts - name.ends) %>% 
    dplyr::filter(!net.fleet == 0) %>% 
    dplyr::arrange(service_pattern, value) %>% 
    dplyr::mutate(fleet = cumsum(net.fleet)) %>% 
    dplyr::left_join(time_points %>% setNames(c('time', 'value')), by = 'value') %>% 
    dplyr::mutate(fleet = fleet - min(fleet),
                  hour = floor(starts/3600)) %>% 
    dplyr::group_by(service_pattern, pattern_frequency, hour) %>% 
    dplyr::reframe(fleet = max(fleet)) %>% 
    dplyr::select(hour, fleet, service_pattern, pattern_frequency)
  
  return(fleet)
  
}

get_fleet_peak <- function(gtfs){
  
  message('\nThis method returns the number of simultaneous trips for the three busiest hours.')
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  time_points <- 
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
                     na.omit()) %>% 
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::select(-trip_id) %>% 
    tidyr::pivot_longer(cols = 1:2) %>% 
    dplyr::select(-name) %>% 
    dplyr::arrange(value) %>% 
    unique() %>% 
    dplyr::mutate(name = paste0('timepoint-', 1:nrow(.)) %>% as_factor())
  
  fleet <-
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
                     na.omit()) %>%
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::left_join(time_points %>% dplyr::rename(starts = value, name.starts = name),
                     by = 'starts') %>%
    dplyr::left_join(time_points %>% dplyr::rename(ends = value, name.ends = name),
                     by = 'ends') %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    tidyr::pivot_longer(cols = c('name.starts', 'name.ends')) %>% 
    dplyr::group_by(route_id, name, value, service_pattern, pattern_frequency, starts) %>% 
    dplyr::reframe(n = n()) %>%
    tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::mutate(net.fleet = name.starts - name.ends) %>% 
    dplyr::filter(!net.fleet == 0) %>% 
    dplyr::arrange(service_pattern, value) %>% 
    dplyr::mutate(fleet = cumsum(net.fleet)) %>% 
    dplyr::left_join(time_points %>% setNames(c('time', 'value')), by = 'value') %>% 
    dplyr::mutate(fleet = fleet - min(fleet),
                  hour = floor(starts/3600)) %>% 
    dplyr::group_by(service_pattern, pattern_frequency, hour) %>%
    dplyr::reframe(fleet = max(fleet)) %>% 
    dplyr::select(hour, fleet, service_pattern, pattern_frequency) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::arrange(., service_pattern, desc(fleet)) %>% 
    dplyr::slice(1:3)
  
  return(fleet)
  
}

get_fleet_detailed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  time_points <- 
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
                     na.omit()) %>% 
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::select(-trip_id) %>% 
    tidyr::pivot_longer(cols = 1:2) %>% 
    dplyr::select(-name) %>% 
    dplyr::arrange(value) %>% 
    unique() %>% 
    dplyr::mutate(name = paste0('timepoint-', 1:nrow(.)) %>% as_factor())
  
  fleet <-
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
                     na.omit()) %>%
    dplyr::mutate(starts = if_else(starts <= 86400, starts, starts - 86400),
                  ends = if_else(ends <= 86400, ends, ends - 86400)) %>% 
    dplyr::left_join(time_points %>% dplyr::rename(starts = value, name.starts = name),
                     by = 'starts') %>%
    dplyr::left_join(time_points %>% dplyr::rename(ends = value, name.ends = name),
                     by = 'ends') %>% 
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>% 
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>% 
    tidyr::pivot_longer(cols = c('name.starts', 'name.ends')) %>% 
    dplyr::group_by(route_id, name, value, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(n = n()) %>%
    tidyr::pivot_wider(names_from = name, values_from = n, values_fill = 0) %>% 
    dplyr::group_by(service_pattern) %>% 
    dplyr::mutate(net.fleet = name.starts - name.ends) %>% 
    dplyr::filter(!net.fleet == 0) %>% 
    dplyr::arrange(service_pattern, value) %>% 
    dplyr::mutate(fleet = cumsum(net.fleet)) %>% 
    dplyr::left_join(time_points %>% setNames(c('time', 'value')), by = 'value') %>% 
    dplyr::select(route_id, net.fleet, fleet, time, service_pattern, pattern_frequency) %>% 
    dplyr::mutate(fleet = fleet - min(fleet))
  
  return(fleet)
  
}

 