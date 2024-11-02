get_headways <- function(gtfs, method = 'by.route'){
  
  if (method == 'by.hour') {
    message('\nThis method assumes constant headways along stops.')
    hw <- get_headway_byhour(gtfs)
  }
  
  if (method == 'by.route') {
    message('\nThis method assumes constant headways along stops.')
    hw <- get_headway_byroute(gtfs)
  }
  
  if (method == 'by.trip') {
    message('\nThis method assumes constant headways along stops.')
    hw <- get_headway_bytrip(gtfs)
  }
  
  if (method == 'detailed') {
    hw <- get_headway_detailed(gtfs)
  }
  
  if (!method %in% c("by.route", 'detailed', 'by.trip', 'by.hour')) {
    hw <- get_headway_byroute(gtfs)
    warning('\n"method" should be one of "by.hour", "by.route", "by.trip" or "detailed".\nReturning "method = "by.route"".')
  }
  
  return(hw)
  
}

get_headway_byhour <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  hw <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    group_by(route_id, trip_id, service_pattern, pattern_frequency) %>%
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
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time)) %>%
    dplyr::filter(headway.minutes >= 0) %>% 
    dplyr::group_by(hour, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(average.headway = mean(headway.minutes, na.rm = T),
                   trips = n()) %>%
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(hour, trips, average.headway, service_pattern, pattern_frequency) %>% 
    na.omit()
  
  return(hw)
  
}

get_headway_byroute <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  hw <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    group_by(route_id, trip_id, service_pattern, pattern_frequency) %>%
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
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::mutate(headway.minutes = (lead(arrival_time) - arrival_time)) %>%
    dplyr::filter(headway.minutes >= 0) %>% 
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(average.headway = mean(headway.minutes, na.rm = T),
                   trips = n()) %>%
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, trips, average.headway, service_pattern, pattern_frequency) %>% 
    na.omit()
  
  return(hw)
  
}

get_headway_bytrip <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    GTFSwizard::get_servicepattern(gtfs)
  
  hw <-
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>%
    dplyr::left_join(gtfs$trips, by = join_by(trip_id)) %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>% 
    group_by(route_id, trip_id, service_pattern, pattern_frequency) %>%
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
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::mutate(headway = (lead(arrival_time) - arrival_time)) %>%
    dplyr::filter(headway >= 0) %>% 
    stats::na.omit() %>% 
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, trip_id, headway, service_pattern, pattern_frequency) %>% 
    dplyr::ungroup()
  
  
  return(hw)
  
}

get_headway_detailed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
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
    dplyr::group_by(route_id, stop_id, service_pattern, pattern_frequency) %>% 
    dplyr::mutate(headway = (lead(arrival_time) - arrival_time)) %>%
    dplyr::filter(headway >= 0) %>% 
    ungroup() %>% 
    # filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, trip_id, stop_id, hour, headway, service_pattern, pattern_frequency) %>% 
    na.omit()
  
  return(hw)
  
}

