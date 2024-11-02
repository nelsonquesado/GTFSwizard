#split_trip <- function(gtfs, trip, method = 'equal.size', split = 1){
split_trip <- function(gtfs, trip, split = 1){

  # checa os argumentos --------------------------------------------  -----------------------------
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('The gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }
  
  checkmate::assert_int(split)
  
  checkmate::assert_subset(trip, choices = gtfs$trips$trip_id)
  
  #checkmate::assert_choice(method, choices = c('equal.size'))
  
  # identifica trips -------------------------------------------------------------------------
  groups <- split + 1
  
  split_data <- 
    gtfs$stop_times %>% 
    dplyr::mutate(split = trip_id %in% trip) %>%
    dplyr::group_by(trip_id) %>% 
    dplyr::mutate(subtrip = if_else(split == T, ceiling(1:n()/n() * groups), NA),
           dupe = split == T & !subtrip == lead(subtrip)) %>% 
    dplyr::ungroup() %>% 
    dplyr::bind_rows(slice(., .$dupe %>% which()) %>% mutate(subtrip = subtrip + 1))
  
  trip.dic <- 
    split_data %>% 
    dplyr::select(trip_id, subtrip) %>% 
    na.omit() %>% 
    dplyr::distinct() %>% 
    group_by(trip_id) %>% 
    dplyr::mutate(new.trip_id = 1:n())  %>% 
    dplyr::mutate(new.trip_id = paste0(trip_id, '.', LETTERS[new.trip_id]))
  
  # stop times ----------------------------------------------------------------------------------
  gtfs$stop_times <- 
    split_data %>% 
    left_join(trip.dic, by = c('trip_id', 'subtrip')) %>% 
    mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>% 
    select(-subtrip, -dupe, -new.trip_id)
  
  if (!is_null(gtfs$stop_times$shape_dist_traveled)) {
    
    gtfs$stop_times <-
      gtfs$stop_times %>% 
      dplyr::mutate(shape_dist_traveled = if_else(split, shape_dist_traveled - shape_dist_traveled[1], shape_dist_traveled))
  
      }
  
  gtfs$stop_times <- dplyr::select(gtfs$stop_times, -split)

  # trips --------------------------------------------------------------------------------------
  gtfs$trips <- 
    dplyr::left_join(gtfs$trips, trip.dic, by = 'trip_id') %>% 
    dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id),
                  shape_id = if_else(is.na(new.trip_id), shape_id, paste0('shape-', new.trip_id))) %>% 
    dplyr::select(-new.trip_id, -subtrip)

  # frequencies ---------------------------------------------------------------------------------
  if (!is_null(gtfs$frequencies$trip_id)) {
    
    gtfs$frequencies <- 
      dplyr::left_join(gtfs$frequencies, trip.dic, by = 'trip_id') %>% 
      dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>% 
      dplyr::select(-new.trip_id, -subtrip)
    
  }
  
  # transfers -----------------------------------------------------------------------------------
  if (!is_null(gtfs$transfers$trip_id)) {
    
    gtfs$transfers <- 
      dplyr::left_join(gtfs$transfers, trip.dic, by = 'trip_id') %>% 
      dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>% 
      dplyr::select(-new.trip_id)
    
  }  
  
  # corrigindo shapes ---------------------------------------------------------------------------
  gtfs.x <- 
    GTFSwizard::filter_trip(gtfs, trip.dic$new.trip_id, keep = FALSE)
  
  gtfs.y <- 
    GTFSwizard::filter_trip(gtfs, trip.dic$new.trip_id, keep = TRUE) %>% 
    GTFSwizard::get_shapes()
  
  gtfs <- GTFSwizard::merge_gtfs(gtfs.x, gtfs.y, suffix = FALSE)

  # retornando gtfs -----------------------------------------------------------------------------
  return(gtfs)
  
}
