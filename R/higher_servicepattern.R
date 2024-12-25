



higher_servicepattern <- function(gtfs){
  UseMethod('higher_servicepattern')
}

higher_servicepattern.list <- function(gtfs){
  sp <- get_servicepattern(gtfs)
  sp$service_pattern[1] %>%
    return()
}




