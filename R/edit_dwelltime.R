
# Lógica ------------------------------------------------------------------
# 
# If the duration doesn't change the headway doesn't change either, then you only need to change the trip.
# 
# If the duration changes and the headway can change, you only need to change the trip as well.
# 
# If the duration changes, but the headway can't change, you need to change it along the route by accumulating the differences in the trips in the same direction.
# 
# If the change is in specific stops keep_duration necessarily needs to be FALSE.
# 
# If the change is to specific trips keep_headway necessarily needs to be FALSE



# Auxiliar functions ------------------------------------------------------------------



do_keep_duration <- function(x){
  if('set'%in%names(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

do_keep_headway <- function(x){
  if('set'%in%names(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

as.character.Period <- function(x){
  x <- lubridate::hms(x,roll = T)
  paste0(ifelse(hour(x)<10,paste0('0',hour(x)),hour(x)),":",minute(x),":",second(x))
}

verify_keepduration <- function(x){
  if('wizardgtfs_selected' %in% class(x)){
    stops <- x$stop_times$stop_id[x$stop_times$trip_id %in% attr(x,'selection')$trips]
    
    if(any(stops %nin% attr(x,'selection')$stops)){
      return(FALSE)
    }else{
      return(TRUE)
    }
    
  }else{
    return(TRUE)
  }
}


verify_keepheadway <- function(x){
  if('wizardgtfs_selected' %in% class(x)){
    trips <- x$trips$trip_id[x$trips$route_id %in% attr(x,'selection')$routes]
    
    if(any(trips %nin% attr(x,'selection')$trips)){
      return(FALSE)
    }else{
      return(TRUE)
    }
    
  }else{
    return(TRUE)
  }
}

# Main functions ----------------------------------------------------------


edit_dwelltime <- function(obj,value){
  if(missing(value)){
    stop('Missing “value” argument with no pattern')
  }
  checkmate::assert_vector(value,len = 1)
  if(!checkmate::test_named(value)){
    cat('The "value" parameter has been assigned as "set"')
    warning('The “value” parameter has been assigned as “set”')
    names(value) <- 'set'
  }
  if(names(value) %in% c('set','to','add','mult') == FALSE){
    stop('The "value" must be one of "set", "to", "add" or "mult"')
  }
  
  UseMethod('edit_dwelltime')
}


edit_dwelltime.list<- function(obj,...){
  obj <- as_wizardgtfs(obj)
  edit_dwelltime.wizardgtfs(obj,...)
}

edit_dwelltime.wizardgtfs_selected <- function(obj,value){
  
  
  if(!checkmate::test_named(value)){
    names(value) <- 'set'
  }
  
  
  keep_duration <- do_keep_duration(value)
  
  if('set'%in%names(value)){
    if(!verify_keepduration(obj)){
      warning(crayon::red("When 'value' is designated as 'set,' the change applies to the entire trip, regardless of specific stop selections. Use 'to,' 'add,' or 'mult' to adjust dwell time at specific stops.\nTo don't see this, select an entire trip or route.\nSee details using help('edit_dwelltime')\n"))
    }
  }
  
  trips <- obj$stop_times %>% 
    group_by(trip_id) %>% 
    arrange(stop_sequence) %>% 
    ungroup() %>%
    dplyr::left_join(
      obj$trips %>% select(trip_id,direction_id),
      by  = 'trip_id'
    )
  
  selection_trips <- attr(obj,'selection')$trips
  
  class(trips) <- c(paste0('trips_',names(value)),class(trips))
  
  trips <- split(trips,trips$trip_id)
  
  if(names(value) == 'set'){
    
    trips[selection_trips] <- lapply(
      trips[selection_trips],
      edit_trips_dwelltime,
      value = value
    )
    
  }else{
    
    selection_stops <- attr(obj,'selection')$stops
    
    trips[selection_trips] <- lapply(
      trips[selection_trips],
      edit_trips_dwelltime,
      value = value,
      stops = selection_stops
    )
    
  }
  
  obj$stop_times <- data.table::rbindlist(trips) 
  
  
  return(obj)
  
}



edit_trip_dwelltime <- function(trip,value,stops){
  UseMethod('edit_trip_dwelltime')
}

edit_trip_dwelltime.trips_set <- function(trip,value,stops){
  
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  trip <- trip %>% 
    mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>% 
    mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>% 
    mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
    mutate(max_dweeltime_ant = as.numeric(arrival_time-lag(departure_time))/1.5) %>% 
    mutate(max_dweeltime_post = as.numeric(lead(arrival_time)-departure_time)/1.5) %>% 
    mutate(max_dweeltime = apply(tibble(max_dweeltime_ant,max_dweeltime_post),1,min,na.rm = T)) %>% 
    select(-max_dweeltime_post,-max_dweeltime_ant) %>% 
    mutate(change_dweeltime = ifelse(value>max_dweeltime,max_dweeltime-actual_dwelltime,value-actual_dweeltime))
  
  if(any(trip$max_dweeltime<value)){
    warning('Some dwelltimes cannot be ',value,' the maximum dwelltime has been allocated')
  }
  
  index <- 2:(nrow(trip)-1)
  trip$arrival_time[index] <- trip$arrival_time[index]-trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[index] <- trip$departure_time[index]+trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[1] <- trip$departure_time[1]+trip$change_dweeltime[1]
  trip$arrival_time[nrow(trip)] <- trip$arrival_time[nrow(trip)]-trip$change_dweeltime[nrow(trip)]
  
  trip %>% 
    select(-max_dweeltime,-actual_dweeltime,-change_dweeltime) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    return()
}

edit_trip_dwelltime.trips_to <- function(trip,value,stops){
  
  trip_na_dwell <- trip %>% 
    filter(arrival_time==""|departure_time=="")
  trip <- trip %>% 
    filter(arrival_time!=""&departure_time!="")
  
  trip <- trip %>% 
    mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>% 
    mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>% 
    mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>% 
    mutate(change_dweeltime = value-actual_dweeltime)
  
  index <- 2:(nrow(trip)-1)
  trip$arrival_time[index] <- trip$arrival_time[index]-trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[index] <- trip$departure_time[index]+trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[1] <- trip$departure_time[1]+trip$change_dweeltime[1]
  trip$arrival_time[nrow(trip)] <- trip$arrival_time[nrow(trip)]-trip$change_dweeltime[nrow(trip)]
  
  trip %>% 
    select(-max_dweeltime,-actual_dweeltime,-change_dweeltime) %>% 
    mutate(arrival_time = as.character.Period(arrival_time)) %>% 
    mutate(departure_time = as.character.Period(departure_time)) %>% 
    dplyr::select(-direction_id) %>% 
    return()
  
}

edit_trip_dwelltime.trips_add <- function(trip,value,stops){
  
}

edit_trip_dwelltime.trips_mult <- function(trip,value,stops){
  
}




