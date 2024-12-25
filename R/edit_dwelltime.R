
#' Edit Dwell Time in GTFS Data
#'
#' Modifies the dwell time of trips in a `wizardgtfs` object based on the specified value parameter.
#'
#' @param gtfs A GTFS object, either as a list, a `wizardgtfs` object or a `wizardgtfs_selected` object, containing GTFS data.
#' @param value A named numeric vector of length 1 specifying how the dwell time should be modified.
#'    The values must be objects of the `lubridate::duration()` class or they will not be interpreted as seconds.
#'    The valid names are "set", "to", "add", and "mult". If `value` is not named, it defaults to "set".
#'   \describe{
#'     \item{\code{`set`}}{Overwrites dwell time for all or selected trips. This method is suitable for adding a dwelltime to a trip where this parameter is zero, it maintains the total duration of the trip.}
#'     \item{\code{`to`}}{Sets dwell time to the specified value for selected trips.}
#'     \item{\code{`add`}}{Increases current dwell time by the specified amount at selected stops.}
#'     \item{\code{`mult`}}{Multiplies current dwell time by the specified factor at selected stops.}
#'   }
#'
#' @details
#' The function modifies the `stop_times` table in the GTFS object by updating dwell times according to the provided `value` parameter.
#' If gtfs is a wizardgtfs_selected object the operation is only performed on the selection.
#' If `value` is specified as `"set"`, the dwell time is updated for the entire trip unless specific stops are selected.
#'
#' A warning is displayed if `"set"` is used without proper selection to inform users that it affects entire trips.
#' Use a selection to modify dwell time only at specific stops or trips.
#'
#' This method modifies the duration of trips, except when the value parameter is set to 'set'.
#'
#' @return A modified wizardgtfs object with updated dwell times.
#'
#' @examples
#' gtfs <- for_bus_gtfs
#'
#' modified_gtfs <- edit_dwelltime(gtfs, value = c(set = 10))
#'
#' modified_gtfs <- gtfs %>%
#'   selection(route_id %in% gtfs$routes$route_id[1:2]) %>%
#'   edit_dwelltime(value = c(add = duration(15)))
#'
#' @seealso
#' [GTFSwizard::selection()], [GTFSwizard::unselection()],
#'
#' @export
edit_dwelltime <- function(gtfs,value){
  if(missing(value)){
    stop('Missing "value" argument with no pattern')
  }
  checkmate::assert_vector(value,len = 1)
  if(!checkmate::test_named(value)){
    #cat('The "value" parameter has been assigned as "set"')
    warning('The "value" parameter has been assigned as "set"')
    names(value) <- 'set'
  }
  if(names(value) %in% c('set','to','add','mult') == FALSE){
    stop('The "value" must be one of "set", "to", "add" or "mult"')
  }

  UseMethod('edit_dwelltime')
}

#' @exportS3Method GTFSwizard::edit_dwelltime list
edit_dwelltime.list<- function(gtfs,...){
  gtfs <- as_wizardgtfs(gtfs)
  edit_dwelltime.wizardgtfs(gtfs,...)
}

#' @exportS3Method GTFSwizard::edit_dwelltime wizardgtfs_selected
edit_dwelltime.wizardgtfs_selected <- function(gtfs,value){


  if(!checkmate::test_named(value)){
    names(value) <- 'set'
  }

  if(lubridate::is.duration(value)){
    value = as.numeric(lubridate::dseconds(value))
  }

  keep_duration <- do_keep_duration(value)

  if('set'%in%names(value)){
    if(!verify_keepduration(gtfs)){
      warning(crayon::red("When 'value' is designated as 'set,' the change applies to the entire trip, regardless of specific stop selections. Use 'to,' 'add,' or 'mult' to adjust dwell time at specific stops.\nTo don't see this, select an entire trip or route.\nSee details using help('edit_dwelltime')\n"))
    }
  }

  trips <- gtfs$stop_times %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(stop_sequence) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      gtfs$trips %>% dplyr::select(trip_id,direction_id),
      by  = 'trip_id'
    )

  selection_trips <- attr(gtfs,'selection')$trips

  trips <- trips %>%
    dplyr::group_by(trip_id) %>%
    dplyr::group_split()

  selection_trips <- sapply(trips, function(x){
    ind <- parent.frame()$i
    if(x$trip_id[1]%in%selection_trips){
      return(ind)
    }else{return(NULL)}
  }) %>% unlist()

  if(names(value) == 'set'){

    trips[selection_trips] <- lapply(
      trips[selection_trips],
      edit_trips_dwelltime.trips_set,
      value = value
    )

  }else{

    selection_stops <- attr(gtfs,'selection')$stops

    if(names(value) == 'to'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_to,
        value = value,
        stops = selection_stops
      )
    }

    if(names(value) == 'add'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_add,
        value = value,
        stops = selection_stops
      )
    }

    if(names(value) == 'mult'){
      trips[selection_trips] <- lapply(
        trips[selection_trips],
        edit_trips_dwelltime.trips_mult,
        value = value,
        stops = selection_stops
      )
    }

  }

  gtfs$stop_times <- data.table::rbindlist(trips) %>%
    dplyr::select(-direction_id)

  if(is.null(attr(gtfs,'editions'))){
    editions <- list(
      `1` = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = attr(gtfs,'selection')
      )
    )
  }else{
    editions <- append(
      attr(gtfs,'editions'),
      list(value = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = attr(gtfs,'selection')
      ))
    )

    names(editions) <- as.character(1:length(editions))

  }

  attr(gtfs,'editions') <- editions


  return(gtfs)

}

#' @exportS3Method GTFSwizard::edit_dwelltime wizardgtfs
edit_dwelltime.wizardgtfs <- function(gtfs,value){

  cat('This operation will change all GTFS trips. Consider using a "selection" before the operation. See help("selection")')

  if(!checkmate::test_named(value)){
    names(value) <- 'set'
  }

  if(lubridate::is.duration(value)){
    value = as.numeric(lubridate::dseconds(value))
  }

  keep_duration <- do_keep_duration(value)

  if('set'%in%names(value)){
    if(!verify_keepduration(gtfs)){
      warning(crayon::red("When 'value' is designated as 'set,' the change applies to the entire trip, regardless of specific stop selections. Use 'to,' 'add,' or 'mult' to adjust dwell time at specific stops.\nTo don't see this, select an entire trip or route.\nSee details using help('edit_dwelltime')\n"))
    }
  }

  trips <- gtfs$stop_times %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(stop_sequence) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      gtfs$trips %>% dplyr::select(trip_id,direction_id),
      by  = 'trip_id'
    )

  trips <- trips %>%
    dplyr::group_by(trip_id) %>%
    dplyr::group_split()


  if(names(value) == 'set'){

    trips <- lapply(
      trips,
      edit_trips_dwelltime.trips_set,
      value = value
    )

  }else{

    if(names(value) == 'to'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_to,
        value = value,
        stops = selection_stops
      )
    }

    if(names(value) == 'add'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_add,
        value = value,
        stops = selection_stops
      )
    }

    if(names(value) == 'mult'){
      trips <- lapply(
        trips,
        edit_trips_dwelltime.trips_mult,
        value = value,
        stops = selection_stops
      )
    }

  }

  gtfs$stop_times <- data.table::rbindlist(trips) %>%
    dplyr::select(-direction_id)

  if(is.null(attr(gtfs,'editions'))){
    editions <- list(
      `1` = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = 'all'
      )
    )
  }else{
    editions <- append(
      attr(gtfs,'editions'),
      list(value = list(
        `function` = 'edit_dwellime',
        params = list(value=value),
        selection = 'all'
      ))
    )

    names(editions) <- as.character(1:length(editions))

  }

  attr(gtfs,'editions') <- editions


  return(gtfs)

}



edit_trips_dwelltime.trips_set <- function(trip,value){

  trip_na_dwell <- trip %>%
    dplyr::filter(arrival_time==""|departure_time=="")
  trip <- trip %>%
    dplyr::filter(arrival_time!=""&departure_time!="")

  trip <- trip %>%
    dplyr::mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>%
    dplyr::mutate(max_dweeltime_ant = as.numeric(arrival_time-lag(departure_time))/1.5) %>%
    dplyr::mutate(max_dweeltime_post = as.numeric(lead(arrival_time)-departure_time)/1.5) %>%
    dplyr::mutate(max_dweeltime = apply(tibble(max_dweeltime_ant,max_dweeltime_post),1,min,na.rm = T)) %>%
    dplyr::select(-max_dweeltime_post,-max_dweeltime_ant) %>%
    dplyr::mutate(change_dweeltime = ifelse(value>max_dweeltime,max_dweeltime-actual_dwelltime,value-actual_dweeltime))

  if(any(trip$max_dweeltime<value)){
    warning('Some dwelltimes cannot be ',value,' the maximum dwelltime has been allocated')
  }

  index <- 2:(nrow(trip)-1)
  trip$arrival_time[index] <- trip$arrival_time[index]-trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[index] <- trip$departure_time[index]+trip$change_dweeltime[2:(nrow(trip)-1)]/2
  trip$departure_time[1] <- trip$departure_time[1]+trip$change_dweeltime[1]
  trip$arrival_time[nrow(trip)] <- trip$arrival_time[nrow(trip)]-trip$change_dweeltime[nrow(trip)]

  trip %>%
    dplyr::select(-max_dweeltime,-actual_dweeltime,-change_dweeltime) %>%
    dplyr::mutate(arrival_time = as.character.Period(arrival_time)) %>%
    dplyr::mutate(departure_time = as.character.Period(departure_time)) %>%
    dplyr::select(-direction_id) %>%
    bind_rows(trip_na_dwell) %>%
    dplyr::arrange(stop_sequence) %>%
    return()
}

edit_trips_dwelltime.trips_to <- function(trip,value,stops){

  trip_na_dwell <- trip %>%
    dplyr::filter(arrival_time==""|departure_time=="")
  trip <- trip %>%
    dplyr::filter(arrival_time!=""&departure_time!="")

  trip <- trip %>%
    dplyr::mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>%
    dplyr::mutate(change_dweeltime = value-actual_dweeltime) %>%
    dplyr::mutate(index = 1:n()) %>%
    dplyr::mutate(arrival_time = (index-1)*change_dweeltime+arrival_time) %>%
    dplyr::mutate(departure_time = index*change_dweeltime+departure_time)

  trip %>%
    dplyr::select(-actual_dweeltime,-change_dweeltime,-index) %>%
    dplyr::mutate(arrival_time = as.character.Period(arrival_time)) %>%
    dplyr::mutate(departure_time = as.character.Period(departure_time)) %>%
    bind_rows(trip_na_dwell) %>%
    dplyr::arrange(stop_sequence) %>%
    return()

}

edit_trips_dwelltime.trips_add <- function(trip,value,stops){
  trip_na_dwell <- trip %>%
    dplyr::filter(arrival_time==""|departure_time=="")
  trip <- trip %>%
    dplyr::filter(arrival_time!=""&departure_time!="")

  if(value<0){
    trip <- trip %>%
      dplyr::mutate(arrival_time = lubridate::hms(arrival_time) %>%
               suppressWarnings()) %>%
      dplyr::mutate(departure_time = lubridate::hms(departure_time) %>%
               suppressWarnings()) %>%
      dplyr::mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>%
      dplyr::mutate(non_negative = actual_dweeltime+value >= 0) %>%
      dplyr::mutate(index = 1:n()) %>%
      dplyr::mutate(change_value = ifelse(non_negative, value, actual_dweeltime )) %>%
      dplyr::mutate(arrival_time = (index-1)*change_value + arrival_time) %>%
      dplyr::mutate(departure_time = index*change_value + departure_time) %>%
      dplyr::select(-change_value,-non_negative)
  }else{
    trip <- trip %>%
      dplyr::mutate(arrival_time = lubridate::hms(arrival_time) %>%
               suppressWarnings()) %>%
      dplyr::mutate(departure_time = lubridate::hms(departure_time) %>%
               suppressWarnings()) %>%
      dplyr::mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>%
      dplyr::mutate(index = 1:n()) %>%
      dplyr::mutate(arrival_time = (index-1)*value + arrival_time) %>%
      dplyr::mutate(departure_time = index*value + departure_time)
  }



  trip %>%
    dplyr::select(-actual_dweeltime,-index) %>%
    dplyr::mutate(arrival_time = as.character.Period(arrival_time)) %>%
    dplyr::mutate(departure_time = as.character.Period(departure_time)) %>%
    bind_rows(trip_na_dwell) %>%
    dplyr::arrange(stop_sequence) %>%
    return()
}

edit_trips_dwelltime.trips_mult <- function(trip,value,stops){
  trip_na_dwell <- trip %>%
    dplyr::filter(arrival_time==""|departure_time=="")
  trip <- trip %>%
    dplyr::filter(arrival_time!=""&departure_time!="")

  trip <- trip %>%
    dplyr::mutate(arrival_time = lubridate::hms(arrival_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(departure_time = lubridate::hms(departure_time) %>%
             suppressWarnings()) %>%
    dplyr::mutate(actual_dweeltime = as.numeric(departure_time-arrival_time)) %>%
    dplyr::mutate(change_dweeltime = (value*actual_dweeltime)-actual_dweeltime) %>%
    dplyr::mutate(arrival_time = (1:n()-1)*change_dweeltime+arrival_time) %>%
    dplyr::mutate(departure_time = (1:n())*change_dweeltime+departure_time)

  trip %>%
    dplyr::select(-actual_dweeltime,-change_dweeltime) %>%
    dplyr::mutate(arrival_time = as.character.Period(arrival_time)) %>%
    dplyr::mutate(departure_time = as.character.Period(departure_time)) %>%
    bind_rows(trip_na_dwell) %>%
    dplyr::arrange(stop_sequence) %>%
    return()
}






# Auxiliar functions ------------------------------------------------------------------
do_keep_duration <- function(x){
  if('set'%in%names(x)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

as.character.Period <- function(x){
  x <- lubridate::hms(x,roll = T)
  paste0(ifelse(lubridate::hour(x)<10,paste0('0',lubridate::hour(x)),lubridate::hour(x)),":",lubridate::minute(x),":",lubridate::second(x))
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



