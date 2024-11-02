get_shapes_sf <- function(obj){
  UseMethod('get_shapes_sf')
}

get_shapes_sf.wizardgtfs <- function(obj){
  if('shapes' %in% names(obj) == FALSE){
    warning("Gtfs doesn't have a shapes table, using ", crayon::blue("get_shapes"), " to build it")
    obj <- get_shapes(obj)
  }
  obj$shapes <- get_shapes_sf.data.frame(obj$shapes)
  return(obj)
}

get_shapes_sf.list <- function(obj){
  if('shapes' %in% names(obj) == FALSE){
    warning("Gtfs doesn't have a shapes table, using ", crayon::blue("get_shapes"), " to build it")
    obj <- get_shapes(obj)
  }
  obj$shapes <- get_shapes_sf.data.frame(obj$shapes)
  return(obj)
}

get_shapes_sf.gtfs <- function(obj){
  if('shapes' %in% names(obj) == FALSE){
    warning("Gtfs doesn't have a shapes table, using ", crayon::blue("get_shapes"), " to build it")
    obj <- get_shapes(obj)
  }
  obj$shapes <- get_shapes_sf.data.frame(obj$shapes)
  return(obj)
}

get_shapes_sf.data.frame <- function(obj){
  if('sf'%in%class(obj)){
    st_crs(obj) <- 4326
    return(obj)
  }else{
    
    if('shape_pt_sequence' %in% names(obj)){
      obj <- obj %>% 
        dplyr::mutate(shape_pt_sequence = as.numeric(shape_pt_sequence)) %>% 
        dplyr::arrange(shape_id,shape_pt_sequence)
    }else{
      warning("When the ",crayon::blue('"shape_pt_sequence"')," column is not defined, the line will be built considering the points in order.")
    }
    
    if('shape_dist_traveled' %in% names(obj)){
      obj <- obj %>% 
        dplyr::group_by(shape_id) %>% 
        dplyr::mutate(geometry = paste0(shape_pt_lon,' ',shape_pt_lat)) %>% 
        dplyr::group_by(shape_id) %>% 
        dplyr::reframe(
          shape_dist_traveled = sum(as.numeric(shape_dist_traveled),na.rm = T),
          geometry = paste0('LINESTRING(',paste0(geometry,collapse = ', '), ')')
        ) %>%
        sf::st_as_sf(wkt = 'geometry',crs=4326) %>% 
        tibble::as_tibble() %>% 
        sf::st_as_sf()
        
    }else{
      obj <- obj %>% 
        dplyr::group_by(shape_id) %>% 
        dplyr::mutate(geometry = paste0(shape_pt_lon,' ',shape_pt_lat)) %>% 
        dplyr::group_by(shape_id)
        dplyr::reframe(
          geometry = paste0('LINESTRING(',paste0(geometry,collapse = ', '), ')')
        ) %>% 
        sf::st_as_sf(wkt = 'geometry',crs=4326) %>% 
          tibble::as_tibble() %>% 
          sf::st_as_sf()
    }
    
    return(obj)
  }
}
