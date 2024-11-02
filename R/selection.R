

# Auxiliar functions ------------------------------------------------------

`%intersects%` <- function(x,y){
  x_ <- st_sfc(unique(x),crs = 4326)
  res_match <- st_intersects(x_,y,sparse = F) %>%
    apply( MARGIN = 1, FUN = any)
  return(res_match[match(x,x_)])
}

`%touches%` <- function(x,y){
  x_ <- st_sfc(unique(x),crs = 4326)
  res_match <- st_touches(x_,y,sparse = F) %>%
    apply( MARGIN = 1, FUN = any)
  return(res_match[match(x,x_)])
}

`%crosses%` <- function(x,y){
  st_crosses(x,y,sparse = F) %>% 
    apply( MARGIN = 1, FUN = any)
}

`%within%` <- function(x,y){
  st_within(x,y,sparse = F) %>% 
    apply( MARGIN = 1, FUN = any)
}

`%contains%` <- function(x,y){
  st_contains(x,y,sparse = F) %>% 
    apply( MARGIN = 1, FUN = any)
}

`%overlaps%` <- function(x,y){
  st_overlaps(x,y,sparse = F) %>% 
    apply( MARGIN = 1, FUN = any)
}

`%equals%` <- function(x,y){
  st_equals(x,y,sparse = F) %>% 
    apply( MARGIN = 1, FUN = any)
}

`%nin%` <- function(x,y){
  x %in% y == FALSE
}


# Main Function ----------------------------------------------------------------



selection <- function(obj,...,add = FALSE){
  
  expr <- substitute(...)
  
  tryCatch(
    eval(expr,
         list('stop_id'=character(0),'route_id'=character(0),
           'trip_id'=character(0),'geometry'=st_sfc(st_point(c(0,0)),crs = 4326) )),
    error = function(e){
      e <- as.character(e)
      obj <- regmatches(e, regexpr("(?<=objeto ')[^']+", e, perl = TRUE))
      stop(e,"Variables must be a subset of {'stop_id','route_id','trip_id','geometry'}, but has additional elements {'",obj,"'}")
    }
  )
  
  UseMethod('selection')
}

selection.list <- function(obj,...,add = FALSE){
  obj <- as_wizardgtfs(obj)
  selection.wizardgtfs(obj,...,add)
}

selection.wizardgtfs <- function(obj,...,add = FALSE){
  
  if(add){
    cat('add=TRUE, but there is no selection in the object')
  }
  
  expr <- substitute(...)
  
  variables <- enquos(..., .ignore_empty = 'all') %>%
    sapply(rlang::quo_get_expr) %>% as.character() %>% 
    strsplit('&|\\|') %>% unlist() %>% 
    stringr::str_trim() %>% sapply(function(x) unlist(strsplit(x,' '))[1])
  
  attr(obj,'selection_expr') <- expr
  
  if('geometry' %nin% variables){
    
    stop_times <- left_join(
      obj$stop_times,
      obj$trips[,c('trip_id','route_id')],
      by = 'trip_id'
    )
    
    selection <- eval(expr,stop_times)
    if(sum(selection)==0){
      return(obj)
    }
    
    selection <- list(
      expr = expr,
      variables = variables,
      routes = unique(stop_times$route_id[selection]),
      trips = unique(stop_times$trip_id[selection]),
      stops = unique(stop_times$stop_id[selection])
    )
    
    attr(obj,'selection') <- selection
  }
  
  # if('geometry' %in% variables && 'route_id' %nin% variables){
  #   
  #   stop_times <- left_join(
  #     obj$stop_times,
  #     get_stops_sf(obj$stops)[,'stop_id'],
  #     by = 'stop_id'
  #   ) %>% 
  #     st_as_sf()
  #   
  #   selection <- eval(expr,stop_times)
  #   if(sum(selection)==0){
  #     return(obj)
  #   }
  #   
  #   selection <- list(
  #     expr = expr,
  #     variables = variables,
  #     trips = unique(stop_times$trip_id[selection]),
  #     stops = unique(stop_times$stop_id[selection])
  #   )
  #   
  #   attr(obj,'selection') <- selection
  # }
  # 
  # if('geometry' %nin% variables && 'route_id' %nin% variables){
  #   
  #   selection <- eval(expr,obj$stop_times)
  #   if(sum(selection)==0){
  #     return(obj)
  #   }
  #   
  #   selection <- list(
  #     expr = expr,
  #     variables = variables,
  #     trips = unique(obj$stop_times$trip_id[selection]),
  #     stops = unique(obj$stop_times$stop_id[selection])
  #   )
  #   
  #   attr(obj,'selection') <- selection
  #   
  #   
  # }
  
  if('geometry' %in% variables){
    
    stop_times <- left_join(
      obj$stop_times,
      obj$trips[,c('trip_id','route_id')],
      by = 'trip_id'
    ) %>% 
      left_join(
        get_stops_sf(obj$stops)[,'stop_id'],
        by = 'stop_id'
      ) %>% st_as_sf()
    
    selection <- eval(expr,stop_times)
    if(sum(selection)==0){
      return(obj)
    }
    
    selection <- list(
      expr = expr,
      variables = variables,
      routes = unique(stop_times$route_id[selection]),
      trips = unique(stop_times$trip_id[selection]),
      stops = unique(stop_times$stop_id[selection])
    )
    
    attr(obj,'selection') <- selection
    
  }
  
  class(obj) <- c('wizardgtfs_selected','wizardgtfs','gtfs','list')
  
  return(obj)
}

selection.wizardgtfs_selected <- function(obj,...,add = FALSE){
  
  if(add){
    
    expr <- substitute(...)
    
    variables <- enquos(..., .ignore_empty = 'all') %>%
      sapply(rlang::quo_get_expr) %>% as.character() %>% 
      strsplit('&|\\|') %>% unlist() %>% 
      stringr::str_trim() %>% sapply(function(x) unlist(strsplit(x,' '))[1])
    
    attr(obj,'selection_expr') <- c(attr(obj,'selection_expr'),expr)
    
    if('geometry' %nin% variables){
      
      stop_times <- left_join(
        obj$stop_times,
        obj$trips[,c('trip_id','route_id')],
        by = 'trip_id'
      )
      
      selection <- eval(expr,stop_times)
      if(sum(selection)==0){
        return(obj)
      }
      
      selection <- list(
        expr = attr(obj,'selection_expr'),
        variables = c(attr(obj,'selection')$variables,variables),
        routes = unique(c(attr(obj,'selection')$routes,stop_times$route_id[selection])),
        trips = unique(c(attr(obj,'selection')$trips,stop_times$trip_id[selection])),
        stops = unique(c(attr(obj,'selection')$stops,stop_times$stop_id[selection]))
      )
      
      attr(obj,'selection') <- selection
    }
    
    if('geometry' %in% variables){
      
      stop_times <- left_join(
        obj$stop_times,
        obj$trips[,c('trip_id','route_id')],
        by = 'trip_id'
      ) %>% 
        left_join(
          get_stops_sf(obj$stops)[,'stop_id'],
          by = 'stop_id'
        ) %>% st_as_sf()
      
      selection <- eval(expr,stop_times)
      if(sum(selection)==0){
        return(obj)
      }
      
      selection <- list(
        expr = attr(obj,'selection_expr'),
        variables = c(attr(obj,'selection')$variables,variables),
        routes = unique(c(attr(obj,'selection')$routes,stop_times$route_id[selection])),
        trips = unique(c(attr(obj,'selection')$trips,stop_times$trip_id[selection])),
        stops = unique(c(attr(obj,'selection')$stops,stop_times$stop_id[selection]))
      )
      
      attr(obj,'selection') <- selection
      
    }
    
    return(obj)
    
  }else{
    selection(obj = unselection(obj) , ... ,add = FALSE) %>% 
      return()
  }
  
  
}






