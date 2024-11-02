
get_stops_sf <- function(obj){
  UseMethod('get_stops_sf')
}


get_stops_sf.wizardgtfs <- function(obj){
  obj$stops <- get_stops_sf.data.frame(obj$stops)
  return(obj)
}

get_stops_sf.list <- function(obj){
  obj$stops <- get_stops_sf.data.frame(obj$stops)
  return(obj)
}


get_stops_sf.gtfs <- function(obj){
  obj$stops <- get_stops_sf.data.frame(obj$stops)
  return(obj)
}

get_stops_sf.data.frame <- function(obj){
  if('sf'%in%class(obj)){
    st_crs(obj) <- 4326
    return(obj)
  }else{
    obj %>% 
      sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>% 
      return()
  }
}