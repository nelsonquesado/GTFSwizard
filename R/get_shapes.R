get_shapes <- function(gtfs){
  
  message('\nThis algorithm reconstructs the shapes table using euclidean approximation, based on the coordinates and sequence of stops for each trip, and', crayon::cyan(' may not be accurate'), '.')
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('\nThis gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }
  
  if(!is_null(gtfs$shapes)){
    warning('This gtfs object already contains a shapes table.\n', crayon::cyan('get_shapes()'), ' will', crayon::red(" overwrite"), ' it.')
  }
  
  shapes.dic <- 
    gtfs$stop_times %>%
    dplyr::select(trip_id, stop_id, stop_sequence) %>% 
    dplyr::arrange(trip_id, stop_sequence) %>% 
    dplyr::left_join(gtfs$stops %>%
                       tidytransit::stops_as_sf() %>% 
                       dplyr::select(stop_id),
                     by = join_by(stop_id)
                     ) %>% 
    sf::st_as_sf(crs = 4326) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::arrange(stop_sequence) %>% 
    dplyr::summarise(geometry = st_combine(geometry) %>% sf::st_cast('LINESTRING')) %>% #plot 
    dplyr::left_join(gtfs$trips %>%
                       dplyr::select(trip_id, route_id)) %>% 
    dplyr::group_by(geometry) %>% 
    dplyr::reframe(trip_id = list(trip_id)) %>%
    dplyr::mutate(shape_id = paste0('shape-', 1:n()))
  
  gtfs$shapes <-
    shapes.dic[, c(3, 1)] %>% 
    sf::st_as_sf()
  
  gtfs$trips <- 
    gtfs$trips %>% 
    dplyr::select(-shape_id) %>% 
    dplyr::left_join(shapes.dic %>%
                       dplyr::select(-geometry) %>% 
                       tidyr::unnest(cols = 'trip_id'))
  
  gtfs$shapes <-
    GTFSwizard::get_shapes_df(gtfs$shapes)

  
  return(gtfs)
  
}