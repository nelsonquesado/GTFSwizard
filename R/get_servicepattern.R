get_servicepattern <- function(gtfs){
  
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  service_pattern <- 
    gtfs$dates_services %>% 
    dplyr::group_by(service_id) %>% 
    dplyr::reframe(dates = list(as.character(date)),
                   pattern_frequency = n()) %>% 
    dplyr::arrange(-pattern_frequency) %>% 
    dplyr::mutate(service_pattern = paste0('servicepattern-', 1:n()) %>% as_factor()) %>% 
    dplyr::select(service_id, service_pattern, pattern_frequency)
  
  
  while(rlang::is_list(service_pattern$service_id)) {
    service_pattern <- service_pattern %>% unnest(., cols = c(service_id))
  }
  
  return(service_pattern)
  
}
