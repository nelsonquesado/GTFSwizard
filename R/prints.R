#' @exportS3Method base::print wizardgtfs
print.wizardgtfs <- function(x, ...){

  cat(crayon::bold('A wizardx object with: '),'\n')
  cat('\t',length(x$agency$agency_id),' agencys')
  cat('\t',glue::glue_collapse(x$agency$agency_name,sep = ', ',last = ' and '),'\n')
  cat('\t',length(x$routes$route_id),' routes','\n')
  cat('\t',length(x$stops$stop_id),' stops','\n')

  for (i in which(names(x)!='dates_services')) {
    cat(crayon::bold(crayon::green(names(x)[i])),'\n')
    if(nrow(x[[i]])>5){
      print(x[[i]],n=5)
    }else{
      print(x[[i]])
    }

  }

}

#' @exportS3Method base::print summary.wizardgtfs
print.summary.wizardgtfs <- function(x, ...){
  cat(crayon::bold('A wizardgtfs object with: '),'\n\n')
  cat(crayon::cyan(crayon::bold(x$n)),crayon::silver(' GTFS tables'),'\n')
  cat('With the following names and respective numbers of entries in each:','\n')
  print(x$tables)
  cat(crayon::silver('Agency: '),crayon::cyan(crayon::bold(x$agency)),'\n')
  cat(crayon::silver('Period of service:: '),crayon::cyan(crayon::bold('from ',x$service_days[1],' to ',x$service_days[2])),'\n\n')
  cat(crayon::cyan(crayon::bold(x$routes)),crayon::silver(' routes'),'\n')
  cat(crayon::cyan(crayon::bold(x$stops)),crayon::silver(' stops'),'\n')
  cat(crayon::cyan(crayon::bold(x$trips)),crayon::silver(' trips'),'\n')
  cat(crayon::cyan(crayon::bold(x$shapes)),crayon::silver(' shapes'),'\n')
  cat(crayon::cyan(crayon::bold(x$total_days)),
      crayon::silver(' valid days of service'),'\n')
  cat(crayon::cyan(crayon::bold(x$stops_dist)),
      crayon::silver(' meters is the average distance between sequencial stops in a given route'),'\n')
}

#' @exportS3Method base::summary wizardgtfs
summary.wizardgtfs <- function(object, ...){
  x <- object
  summ <- list(
    n = length(x)-1,
    tables = lapply(x[names(x)!='dates_services'],nrow ) %>% unlist(),
    agency = stringr::str_flatten(x$agency$agency_name,collapse = ', ',last = ' and '),
    service_days = c(min(x$dates_services$date,na.rm = TRUE),max(x$dates_services$date,na.rm = TRUE)),
    routes =  nrow(x$routes),
    stops = nrow(x$stops),
    trips = nrow(x$trips),
    shapes = length(unique(x$shapes$shape_id)),
    total_days = nrow(x$dates_services),
    stops_dist = get_stop_dists(x)
  )
  class(summ) <- 'summary.wizardgtfs'
  print(summ)
}

#' @exportS3Method base::plot wizardgtfs
plot.wizardgtfs <- function(x, ...){
  nm = names(x)
  if (sum(c("stops",'shapes') %in% nm) == 0) {
    stop(crayon::red("Feed doesn't contain a stops table nor a shapes table"),'\n\t',
         crayon::red('Nothing to plot'))
  }else{

    if('shapes' %in% nm & 'stops' %in% nm){
      if('sf' %in% class(x$shapes) == FALSE){

        tryCatch(
          x$shapes <- get_shapes_sf(x$shapes),
          error = function(e){
            if('sf' %in% class(x$stops) == FALSE){
              x$stops <- get_stops_sf(x$stops)
            }
            return(plot_stops(x))
          }
        )

      }

      if('sf' %in% class(x$stops) == FALSE){

        x$stops <- get_stops_sf(x$stops)

      }

      plot_shapes.stops(x)


    }else{

      if('stops' %in% nm){

        if('sf' %in% class(x$stops) == FALSE){

          x$stops <- get_stops_sf(x$stops)

        }

        plot_stops(x)


      }else{

        if('sf' %in% class(x$shapes) == FALSE){

          x$shapes <- get_stops_sf(x$shapes)

        }

        plot_shapes(x)

      }

    }




  }


}


plot_shapes.stops <- function(gtfs, ...){

  if(nrow(gtfs$agency)==1){

    return(
      ggplot2::ggplot()+
        ggplot2::geom_sf(data = gtfs$stops,show.legend = FALSE,color = '#41A5E1',size=1)+
        ggplot2::geom_sf(data = gtfs$shapes,ggplot2::aes(color = shape_id),show.legend = FALSE)+
        ggplot2::theme_linedraw()+
        ggplot2::theme(axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank())+
        ggplot2::labs( title = gtfs$agency$agency_name)
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
             dplyr:: select(shape_id,route_id) %>%
              unique(),
            by = 'shape_id'
          ) %>%
          dplyr::left_join(
            gtfs$routes %>% dplyr::select(route_id, agency_id),
            by = 'route_id'
          ) %>%
          dplyr::left_join(
            gtfs$agency %>% dplyr::select(agency_id,agency_name),
            by = 'agency_id'
          ) %>% sf::st_as_sf()

        stops <- gtfs$stop_times %>%
          dplyr::select(trip_id,stop_id) %>%
          dplyr::left_join(
            gtfs$trips %>%
              dplyr::select(trip_id,route_id) %>%
              unique(),
            by = 'trip_id'
          ) %>% dplyr::select(-trip_id) %>%
          dplyr::left_join(
            gtfs$routes %>%
              dplyr::select(route_id,agency_id),
            by = 'route_id'
          ) %>%
          dplyr::left_join(
            gtfs$agency %>%
              dplyr::select(agency_id,agency_name),
            by = 'agency_id'
          ) %>%
          dplyr::select(-route_id) %>%
          unique() %>%
          dplyr::left_join(
            gtfs$stops %>%
              dplyr::select(stop_id),
            by = 'stop_id'
          ) %>% sf::st_as_sf()

        ggplot2::ggplot()+
          ggplot2::geom_sf(data = stops,show.legend = FALSE,color = '#41A5E1',size=1)+
          ggplot2::geom_sf(data = shapes,ggplot2::aes(color = shape_id),show.legend = FALSE)+
          ggplot2::theme_linedraw()+
          ggplot2::theme(axis.title = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank())+
          ggplot2::facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))


      }

    }


  }


}


plot_shapes <- function(gtfs, ...){

  if(!verify_field(gtfs$trips,'shape_id')|!verify_field(gtfs$routes,'agency_id')){

    ggplot2::ggplot(shapes)+
      ggplot2::geom_sf(ggplot2::aes(color = shape_id),show.legend = FALSE)+
      ggplot2::theme_light()+
      ggplot2::theme(axis.title = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank())+
      ggplot2::labs(title = paste0(gtfs$agency$agency_name, collapse = ' ,'))

  }else{
    shapes <- gtfs$shapes %>%
      dplyr::left_join(
        gtfs$trips %>%
          dplyr::select(shape_id,route_id) %>%
          unique(),
        by = 'shape_id'
      ) %>%
      dplyr::left_join(
        gtfs$routes %>% dplyr::select(route_id, agency_id),
        by = 'route_id'
      ) %>%
      dplyr::left_join(
        gtfs$agency %>% dplyr::select(agency_id,agency_name),
        by = 'agency_id'
      )%>% sf::st_as_sf()

    ggplot2::ggplot(shapes)+
      ggplot2::geom_sf(aes(color = route_id),show.legend = FALSE)+
      ggplot2::theme_light()+
      ggplot2::theme(axis.title = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank())+
      ggplot2::facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))
  }


}


plot_stops <- function(gtfs, ...){

  if(!verify_field(gtfs$stop_times,'stop_id')|!verify_field(gtfs$routes,'agency_id')){

    return(
      ggplot2::ggplot(stops) +
        ggplot2::geom_sf(color = '#41A5E1',size=1) +
        ggplot2::theme_light() +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank())+
        ggplot2::labs(title = paste0(gtfs$agency$agency_name, collapse = ' ,'))
    )

  }else{

    stops <- gtfs$stop_times %>%
      dplyr::select(trip_id,stop_id) %>%
      dplyr::left_join(
        gtfs$trips %>%
          dplyr::select(trip_id,route_id) %>%
          unique(),
        by = 'trip_id'
      ) %>% dplyr::select(-trip_id) %>%
      dplyr::left_join(
        gtfs$routes %>%
          dplyr::select(route_id,agency_id),
        by = 'route_id'
      ) %>%
      dplyr::left_join(
        gtfs$agency %>%
          dplyr::select(agency_id,agency_name),
        by = 'agency_id'
      ) %>%
      dplyr::select(-route_id) %>%
      unique() %>%
      dplyr::left_join(
        gtfs$stops %>%
          dplyr::select(stop_id),
        by = 'stop_id'
      ) %>% sf::st_as_sf()

    return(ggplot2::ggplot(stops)+
      ggplot2::geom_sf(color = '#41A5E1',size=1)+
        ggplot2::theme_light()+
        ggplot2::theme(axis.title = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank())+
        ggplot2::facet_wrap(~agency_name,ncol = get_fct_plot_cols(gtfs))
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
