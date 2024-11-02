get_distances <- function(gtfs, method = 'by.route'){
  
  if (method == 'by.route') {
    distances <- get_distances_byroute(gtfs)
  }
  
  if (method == 'by.trip') {
    distances <- get_distances_bytrip(gtfs)
  }
  
  if (method == 'detailed') {
    message('This operation may take several minutes to complete.')
    distances <- get_distances_detailed(gtfs)
  }
  
  if (!method %in% c('by.route',
                     'by.trip',
                     'detailed')) {
    distances <- get_distances_byroute(gtfs)
    warning('\n"method" should be one of "by.route", "by.trip" or "detailed".\nReturning "method = by.route"".')
  }
  
  return(distances)
  
}

get_distances_byroute <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is_null(gtfs$shapes)){
    
    gtfs <- GTFSwizard::get_shapes(gtfs)
    
    warning('\nThis gtfs object does not contain a shapes table.\nUsing get_shapes().')
  }
  
  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)
  
  distances <- 
    tidytransit::shapes_as_sf(gtfs$shapes) %>% 
    dplyr::mutate(distance = st_length(geometry))
  
  distances <-
    gtfs$trips %>% 
    dplyr::left_join(distances, by = 'shape_id') %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>% 
    dplyr::reframe(average.distance = mean(distance, na.rm = T),
                   trips = n()) %>% 
    select(route_id, trips, average.distance, service_pattern, pattern_frequency)
  
  return(distances)
  
}

get_distances_bytrip <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is_null(gtfs$shapes)){
    
    gtfs <- GTFSwizard::get_shapes(gtfs)
    
    warning('\nThis gtfs object does not contain a shapes table.\nUsing get_shapes().')
  }
  
  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)
  
  distances <- 
    tidytransit::shapes_as_sf(gtfs$shapes) %>% 
    dplyr::mutate(distance = st_length(geometry))
  
  distances <-
    gtfs$trips %>% 
    dplyr::left_join(distances, by = 'shape_id') %>% 
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    select(route_id, trip_id, distance, service_pattern, pattern_frequency)
  
  return(distances)
  
}

get_distances_detailed <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(is_null(gtfs$shapes)){
    
    gtfs <- GTFSwizard::get_shapes(gtfs)
    
    warning('\nThis gtfs object does not contain a shapes table.\nUsing get_shapes().')
  }
  
  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)
  
  pairs <- 
    gtfs$stop_times %>% 
    dplyr::filter(!arrival_time == '') %>%
    dplyr::arrange(trip_id, stop_sequence) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::reframe(from_stop_id = stop_id,
                   to_stop_id = lead(stop_id)) %>% 
    na.omit%>% 
    dplyr::left_join(gtfs$trips %>% select(trip_id, shape_id), by = 'trip_id') %>% 
    dplyr::group_by(from_stop_id, to_stop_id, shape_id) %>% 
    dplyr::reframe(trips = list(trip_id))
  
  shapes_stops <-
    pairs %>% 
    dplyr::group_by(shape_id, from_stop_id) %>% 
    dplyr::reframe(to_stop_id = list(to_stop_id)) %>% 
    dplyr::group_by(shape_id) %>% 
    dplyr::reframe(from_stop_id = list(from_stop_id),
                   to_stop_id = list(to_stop_id)) 
  
  dist_matrix <- NULL
  
  pb <- txtProgressBar(max = nrow(shapes_stops),
                       style = 3)
  
  for (i in 1:nrow(shapes_stops)) {
    
    max.length <- 25
    
    suppressWarnings({shapes.sf <- 
      gtfs$shapes %>% 
      dplyr::filter(shape_id == shapes_stops$shape_id[i]) %>% 
      tidytransit::shapes_as_sf() %>% 
      stplanr::line_segment(
        segment_length = max.length,
        n_segments = NA,
        use_rsgeo = NULL,
        debug_mode = FALSE
      ) %>% 
      sf::st_cast(., 'POINT')  %>% 
      tidyr::unnest(cols = 'geometry')
    })
    
    network <-
      sfnetworks::as_sfnetwork(shapes.sf, directed = FALSE)
    
    origins <-
      shapes_stops %>% 
      dplyr::filter(shape_id == shapes_stops$shape_id[i]) %>% 
      tidyr::unnest(cols = c('from_stop_id', 'to_stop_id'))
    
    routes <- NULL
    
    for (j in 1:nrow(origins)) {
      
      origin <- 
        gtfs$stops %>% 
        dplyr::filter(stop_id == origins$from_stop_id[j]) %>% 
        tidytransit::stops_as_sf() %>% 
        dplyr::select(stop_id)
      
      destinations_ids <- 
        origins[j, ] %>% 
        tidyr::unnest(cols = 'to_stop_id') %>% 
        .$to_stop_id
      
      destinations <- 
        gtfs$stops %>% 
        dplyr::filter(stop_id %in% destinations_ids) %>% 
        tidytransit::stops_as_sf() %>% 
        dplyr::select(stop_id)
      
      shortest_edges <-
        destinations %>% 
        dplyr::bind_cols(
          sfnetworks::st_network_paths(network, origin, destinations) %>% 
            dplyr::select(edge_paths)
        )
      
      distances <- NULL
      
      for (k in 1:nrow(shortest_edges)) {
        
        distance <- 
          network %>% 
          sfnetworks::activate(edges) %>% 
          dplyr::slice(shortest_edges %>% 
                         tibble() %>% 
                         .[k, 3] %>% 
                         unlist) %>%
          sf::st_as_sf() %>%
          sf::st_union() %>% 
          sf::st_length()
        
        distances <- 
          distances %>% 
          c(., distance)
        
      }
      
      routes <- 
        routes %>% 
        dplyr::bind_rows(
          tibble(
            shape = shapes_stops$shape_id[i],
            origins = origin %>% tibble %>% .[1,1],
            destinations = destinations_ids,
            distance = distances
            
          )
        )
      
      
    }
    
    dist_matrix <- 
      dist_matrix %>% 
      dplyr::bind_rows(., routes)
    
    setTxtProgressBar(pb, i)
    
  }
  
  dist_matrix <-
    dist_matrix %>%
    tidyr::unnest(cols = origins) %>% 
    stats::setNames(c('shape_id', 'from_stop_id', 'to_stop_id', 'distance'))
  
  return(dist_matrix)
  
}


# get_distances_byhour <- function(gtfs){

#   if(!"wizardgtfs" %in% class(gtfs)){
#     gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
#     warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
#   }
#   
#   service_pattern <-
#     GTFSwizard::get_servicepattern(gtfs)
#   
#   distances <- 
#     tidytransit::shapes_as_sf(gtfs$shapes) %>% 
#     dplyr::mutate(distance = st_length(geometry))
#   
#   hour.table <- 
#     tibble(hour = 0:23,
#          starts = hour * 3600,
#          ends = (hour + 1) * 3600 - 1)
#   
#   gtfs$stop_times %>% 
#     dplyr::filter(!arrival_time == '') %>%
#     dplyr::group_by(trip_id) %>% 
#     dplyr::reframe(starts = arrival_time[1] %>% 
#                      stringr::str_split(":") %>% 
#                      lapply(FUN = as.numeric) %>% 
#                      lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>% 
#                      unlist() %>% 
#                      na.omit(),
#                    ends = arrival_time[n()] %>% 
#                      stringr::str_split(":") %>% 
#                      lapply(FUN = as.numeric) %>% 
#                      lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>% 
#                      unlist() %>% 
#                      na.omit())
#     
#     dplyr::left_join(gtfs$trips %>%
#                        select(trip_id, route_id, shape_id, service_id),
#                      by = 'trip_id') %>% 
#     dplyr::left_join(distances, by = 'shape_id') %>% 
#     dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
#     dplyr::group_by(route_id, hour, service_pattern, pattern_frequency) %>% 
#     dplyr::reframe(average.distance = mean(distance, na.rm = T),
#                    trips = n())
#   
#   return(distances)
#   
# }