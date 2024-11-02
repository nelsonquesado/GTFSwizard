get_shapes_df <- function(shape){
  
  if(!'sf' %in% class(shape)){
    warning('shape is not a simple feature object.')
    stop()
  }
  
  if(is_null(shape$shape_id)){
    warning('shape dos not contains the shape_id column.')
    stop()
  }
  
  x <- 0
  units(x) <- 'm'
  
  shapes_df <- 
    shape %>% 
    select(shape_id) %>% 
    dplyr::as_tibble() %>% 
    stats::setNames(c('shape_id', 'geometry')) %>% 
    dplyr::group_by(shape_id) %>% 
    dplyr::mutate(geometry = list(st_coordinates(geometry) %>% .[, -3])) %>%
    dplyr::ungroup() %>% 
    tidyr::unnest(cols = geometry) %>% 
    data.table::data.table() %>% 
    stats::setNames(c('shape_id', 'shape_pt_lon', 'shape_pt_lat')) %>% 
    dplyr::group_by(shape_id) %>% 
    st_as_sf(coords = c('shape_pt_lon', 'shape_pt_lat'), remove = F, crs = 4326) %>% 
    dplyr::mutate(shape_pt_sequence = 1:n(),
                  shape_dist_traveled = abs(sf::st_distance(geometry, lag(geometry), by_element = T)) %>%
                    tidyr::replace_na(x) %>% 
                    cumsum %>% 
                    as.numeric()) %>% 
    tidyr::tibble() %>% 
    dplyr::select(-geometry)
  
  return(shapes_df)
  
  message("'shape_dist_traveled' unit is [m].")
  
}
