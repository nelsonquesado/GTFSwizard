delay_trip <- function(gtfs, trip, duration){
  
  # checa os argumentos -------------------------------------------------------------------------
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThe first gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }
  
  if(!lubridate::is.duration(duration)) {
    
    if(!is.numeric(duration)) {
      warning("'duration' muste be of the class duration or numeric (seconds)")
      stop()
    }
    
    dur <- lubridate::duration(duration, units = 'seconds')
    
  } else {dur <- duration}
  
  checkmate::assert_subset(trip, choices = gtfs$trips$trip_id)
  
  # atrasa as trips -----------------------------------------------------------------------------
  temp.arrival <-
    gtfs$stop_times$arrival_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$arrival_time == ""] %>% 
    lubridate::hms() %>% 
    lubridate::as.duration() + dur
  
  gtfs$stop_times$arrival_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$arrival_time == ""] <- 
    temp.arrival %>% 
    as.numeric() %>%
    { sprintf("%02d:%02d:%02d", . %/% 3600, (. %% 3600) %/% 60, round(. %% 60)) }
  
  temp.departure <-
    gtfs$stop_times$departure_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$departure_time == ""] %>% 
    lubridate::hms() %>% 
    lubridate::as.duration() + dur
  
  gtfs$stop_times$departure_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$departure_time == ""] <- 
    temp.departure %>% 
    as.numeric() %>%
    { sprintf("%02d:%02d:%02d", . %/% 3600, (. %% 3600) %/% 60, round(. %% 60)) }
  
  # retornando gtfs -----------------------------------------------------------------------------
  return(gtfs)
  
}
