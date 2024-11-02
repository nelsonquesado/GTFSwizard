


print.wizardgtfs <- function(gtfs){
  
  cat(crayon::bold('A wizardgtfs object with: '),'\n')
  cat('\t',length(gtfs$agency$agency_id),' agencys')
  cat('\t',glue::glue_collapse(gtfs$agency$agency_name,sep = ', ',last = ' and '),'\n')
  cat('\t',length(gtfs$routes$route_id),' routes','\n')
  cat('\t',length(gtfs$stops$stop_id),' stops','\n')
  
  for (i in which(names(gtfs)!='dates_services')) {
    cat(crayon::bold(crayon::green(names(gtfs)[i])),'\n')
    if(nrow(gtfs[[i]])>5){
      print(gtfs[[i]],n=5)
    }else{
      print(gtfs[[i]])
    }
    
  }

}

print.summary.wizardgtfs <- function(ls.summ){
  cat(crayon::bold('A wizardgtfs object with: '),'\n\n')
  cat(crayon::cyan(crayon::bold(ls.summ$n)),crayon::silver(' GTFS tables'),'\n')
  cat('With the following names and respective numbers of entries in each:','\n')
  print(ls.summ$tables)
  cat(crayon::silver('Agency: '),crayon::cyan(crayon::bold(ls.summ$agency)),'\n')
  cat(crayon::silver('Period of service:: '),crayon::cyan(crayon::bold('from ',ls.summ$service_days[1],' to ',ls.summ$service_days[2])),'\n\n')
  cat(crayon::cyan(crayon::bold(ls.summ$routes)),crayon::silver(' routes'),'\n')
  cat(crayon::cyan(crayon::bold(ls.summ$stops)),crayon::silver(' stops'),'\n')
  cat(crayon::cyan(crayon::bold(ls.summ$trips)),crayon::silver(' trips'),'\n')
  cat(crayon::cyan(crayon::bold(ls.summ$shapes)),crayon::silver(' shapes'),'\n')
  cat(crayon::cyan(crayon::bold(ls.summ$total_days)),
      crayon::silver(' valid days of service'),'\n')
  cat(crayon::cyan(crayon::bold(ls.summ$stops_dist)),
      crayon::silver(' meters is the average distance between sequencial stops in a given route'),'\n')
}

summary.wizardgtfs <- function(gtfs){
  summ <- list(
    n = length(gtfs)-1,
    tables = lapply(gtfs[names(gtfs)!='dates_services'],nrow ) %>% unlist(),
    agency = str_flatten(gtfs$agency$agency_name,collapse = ', ',last = ' and '),
    service_days = c(min(gtfs$dates_services$date,na.rm = T),max(gtfs$dates_services$date,na.rm = T)),
    routes =  nrow(gtfs$routes),
    stops = nrow(gtfs$stops),
    trips = nrow(gtfs$trips),
    shapes = length(unique(gtfs$shapes$shape_id)),
    total_days = nrow(gtfs$dates_services),
    stops_dist = get_stop_dists(gtfs)
  )
  class(summ) <- 'summary.wizardgtfs'
  print(summ)
}

plot.wizardgtfs <- function(gtfs){
  nm = names(gtfs)
  if (sum(c("stops",'shapes') %in% nm) == 0) {
    stop(crayon::red("Feed doesn't contain a stops table nor a shapes table"),'\n\t',
         crayon::red('Nothing to plot'))
  }else{
    
    if('shapes' %in% nm & 'stops' %in% nm){
      if('sf' %in% class(gtfs$shapes) == FALSE){
        
        tryCatch(
          gtfs$shapes <- get_shapes_sf(gtfs$shapes),
          error = function(e){
            if('sf' %in% class(gtfs$stops) == FALSE){
              gtfs$stops <- get_stops_sf(gtfs$stops)
            }
            return(plot_stops(gtfs))
          }
        )
          
      }
      
      if('sf' %in% class(gtfs$stops) == FALSE){
        
        gtfs$stops <- get_stops_sf(gtfs$stops)
        
      }
      
      plot_shapes.stops(gtfs)
      
      
    }else{
      
      if('stops' %in% nm){
        
        if('sf' %in% class(gtfs$stops) == FALSE){
          
          gtfs$stops <- get_stops_sf(gtfs$stops)
          
        }
        
        plot_stops(gtfs)
        
        
      }else{
        
        if('sf' %in% class(gtfs$shapes) == FALSE){
          
          gtfs$shapes <- get_stops_sf(gtfs$shapes)
          
        }
        
        plot_shapes(gtfs)
        
      }
      
    }
    
    
    
    
  }
  
  
}


plot_shapes.stops <- function(gtfs){
  
  if(nrow(gtfs$agency)==1){
    
    return(
      ggplot()+
        geom_sf(data = gtfs$stops,show.legend = F,color = '#41A5E1',size=1)+
        geom_sf(data = gtfs$shapes,aes(color = shape_id),show.legend = F)+
        theme_linedraw()+
        theme(axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank())+
        labs( title = gtfs$agency$agency_name)
    )
    
  }else{
    
    if(!verify_field(gtfs$trips,'shape_id')|!verify_field(gtfs$routes,'agency_id')){
      
      plot_stops(gtfs)
      
    }else{
      
      if(!verify_field(gtfs$stop_times,'stop_id')|!verify_field(gtfs$routes,'agency_id')){
        
        plot_shapes(gtfs)
        
      }else{
        
        shapes <- gtfs$shapes %>% 
          dplyr::left_join(
            gtfs$trips %>% 
              select(shape_id,route_id) %>% 
              unique(),
            by = 'shape_id'
          ) %>% 
          dplyr::left_join(
            gtfs$routes %>% select(route_id, agency_id),
            by = 'route_id'
          ) %>% 
          dplyr::left_join(
            gtfs$agency %>% select(agency_id,agency_name),
            by = 'agency_id'
          ) %>% st_as_sf()
        
        stops <- gtfs$stop_times %>% 
          select(trip_id,stop_id) %>% 
          dplyr::left_join(
            gtfs$trips %>% 
              select(trip_id,route_id) %>% 
              unique(),
            by = 'trip_id'
          ) %>% select(-trip_id) %>% 
          dplyr::left_join(
            gtfs$routes %>% 
              select(route_id,agency_id),
            by = 'route_id'
          ) %>% 
          dplyr::left_join(
            gtfs$agency %>% 
              select(agency_id,agency_name),
            by = 'agency_id'
          ) %>% 
          select(-route_id) %>% 
          unique() %>% 
          dplyr::left_join(
            gtfs$stops %>% 
              select(stop_id),
            by = 'stop_id'
          ) %>% st_as_sf()
        
        ggplot()+
          geom_sf(data = stops,show.legend = F,color = '#41A5E1',size=1)+
          geom_sf(data = shapes,aes(color = shape_id),show.legend = F)+
          theme_linedraw()+
          theme(axis.title = element_blank(),
                panel.background = element_blank(),
                panel.grid = element_blank())+
          facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))
        
        
      }
      
    }
    
    
  }
  
  
}
plot_shapes <- function(gtfs){
  
  if(!verify_field(gtfs$trips,'shape_id')|!verify_field(gtfs$routes,'agency_id')){
    
    ggplot(shapes)+
      geom_sf(aes(color = shape_id),show.legend = F)+
      theme_light()+
      theme(axis.title = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank())+
      labs(title = paste0(gtfs$agency$agency_name, collapse = ' ,'))
    
  }else{
    shapes <- gtfs$shapes %>% 
      dplyr::left_join(
        gtfs$trips %>% 
          select(shape_id,route_id) %>% 
          unique(),
        by = 'shape_id'
      ) %>% 
      dplyr::left_join(
        gtfs$routes %>% select(route_id, agency_id),
        by = 'route_id'
      ) %>% 
      dplyr::left_join(
        gtfs$agency %>% select(agency_id,agency_name),
        by = 'agency_id'
      )%>% st_as_sf()
    
    ggplot(shapes)+
      geom_sf(aes(color = route_id),show.legend = F)+
      theme_light()+
      theme(axis.title = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank())+
      facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))
  }
  
  
}
plot_stops <- function(gtfs){
  
  if(!verify_field(gtfs$stop_times,'stop_id')|!verify_field(gtfs$routes,'agency_id')){
    
    return(
      ggplot(stops)+
        geom_sf(color = '#41A5E1',size=1)+
        theme_light()+
        theme(axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank())+
        labs(title = paste0(gtfs$agency$agency_name, collapse = ' ,'))
    )
    
  }else{
    
    stops <- gtfs$stop_times %>% 
      select(trip_id,stop_id) %>% 
      dplyr::left_join(
        gtfs$trips %>% 
          select(trip_id,route_id) %>% 
          unique(),
        by = 'trip_id'
      ) %>% select(-trip_id) %>% 
      dplyr::left_join(
        gtfs$routes %>% 
          select(route_id,agency_id),
        by = 'route_id'
      ) %>% 
      dplyr::left_join(
        gtfs$agency %>% 
          select(agency_id,agency_name),
        by = 'agency_id'
      ) %>% 
      select(-route_id) %>% 
      unique() %>% 
      dplyr::left_join(
        gtfs$stops %>% 
          select(stop_id),
        by = 'stop_id'
      ) %>% st_as_sf()
    
    return(ggplot(stops)+
      geom_sf(color = '#41A5E1',size=1)+
      theme_light()+
      theme(axis.title = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank())+
      facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))
    )
    
  }
  
  
  
}


get_fct_plot_cols <- function(x){
  
  if(nrow(x$agency)>3){
    return(3)
  }else{
    return(nrow(x$agency))
  }
  
}

