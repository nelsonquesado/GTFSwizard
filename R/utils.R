
seqs_table <- function(intervals){
  tibble(
    period=unique(intervals)
  ) %>%
    dplyr::mutate(date = purrr::map(period,function(x) seq(int_start(x),int_end(x),'1 day')))
}

label_wday <- function(x=1:7){
  c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')[x]
}

without <- function(x,y){
  x[! x %in% y]
}

get_wday_services <- function(x){
  resp <- tibble(
    wday = label_wday()
  )
  resp$service_id <- purrr::map(label_wday(), function(y){
    list(x$service_id[x[,y]==1])
  })
  resp
}

create_dates_services_table <- function(gtfs_list){

  if('calendar'%in%names(gtfs_list)&'calendar_dates'%in%names(gtfs_list)){

    calendar_intervals <- gtfs_list$calendar %>%
      dplyr::mutate(period = lubridate::interval(start_date,end_date))

    week_days_services <- calendar_intervals %>%
      dplyr::group_by(period) %>%
      reframe(get_wday_services(.))

    dates_services_regular <- seqs_table(calendar_intervals$period) %>%
      unnest('date') %>%
      dplyr::mutate(wday = label_wday(lubridate::wday(date,week_start = 1))) %>%
      dplyr::left_join(
        week_days_services,
        by = c('period','wday')
      ) %>%
      dplyr::select(date,service_id)

    if(anyDuplicated(dates_services_regular$date)>0){
      dates_services_regular <- unique(dates_services_regular) %>%
        group_by(date) %>%
        reframe(service_id = list(unique(unlist(service_id))))
    }


    aditional_services <- gtfs_list$calendar_dates %>%
      filter(exception_type==1) %>%
      dplyr::group_by(date) %>%
      reframe(service_id = list(service_id))

    removed_services<- gtfs_list$calendar_dates %>%
      filter(exception_type==2) %>%
      dplyr::group_by(date) %>%
      reframe(service_id = list(service_id))

    if(nrow(aditional_services)>0){
      full_services <- bind_rows(
        dates_services_regular,
        aditional_services
      ) %>%
        dplyr::group_by(date) %>%
        reframe(
          service_id = list(unique(unlist(service_id)))
        )
    }else{
      full_services <- dates_services_regular
    }

    if(nrow(removed_services)>0){
      full_services <- bind_rows(
        full_services %>% dplyr::mutate(type = 1),
        removed_services %>% dplyr::mutate(type = 2)
      ) %>%
        dplyr::group_by(date) %>%
        reframe(service_id = list(without(unlist(service_id[type==1]),unlist(service_id[type==2]))))

    }

    gtfs_list[['dates_services']] <- unique(full_services)

    return(gtfs_list)

  }else{

    if('calendar'%in%names(gtfs_list)){

      calendar_intervals <- gtfs_list$calendar %>%
        dplyr::mutate(period = interval(start_date,end_date))

      week_days_services <- calendar_intervals %>%
        dplyr::group_by(period) %>%
        reframe(get_wday_services(.))

      dates_services_regular <- seqs_table(calendar_intervals$period) %>%
        unnest('date') %>%
        dplyr::mutate(wday = label_wday(lubridate::wday(date,week_start = 1))) %>%
        dplyr::left_join(
          week_days_services,
          by = c('period','wday')
        ) %>%
        dplyr::select(date,service_id)

      if(anyDuplicated(dates_services_regular$date)>0){
        dates_services_regular <- unique(dates_services_regular) %>%
          group_by(date) %>%
          reframe(service_id = list(unique(unlist(service_id))))
      }

      gtfs_list[['dates_services']] <- dates_services_regular
      return(gtfs_list)

    }else{

      aditional_services <- gtfs_list$calendar_dates %>%
        filter(exception_type==1) %>%
        dplyr::group_by(date) %>%
        reframe(service_id = list(service_id))

      removed_services<- gtfs_list$calendar_dates %>%
        filter(exception_type==2) %>%
        dplyr::group_by(date) %>%
        reframe(service_id = list(service_id))

      gtfs_list[['dates_services']] <- bind_rows(
        aditional_services %>% dplyr::mutate(type = 1),
        removed_services %>% dplyr::mutate(type = 2)
      ) %>%
        dplyr::group_by(date) %>%
        reframe(service_id = list(without(unlist(service_id[type==1]),unlist(service_id[type==2]))))

      return(gtfs_list)

    }

  }


}

verify_tables <- function(x,tables){
  ls <- rep(FALSE,length(tables))
  ls <- tables%in%names(x)==FALSE
  names(ls) <- tables
  return(ls)
}

verify_field <- function(tbl,x){
  x %in% names(tbl)
}

field_if_exist <- function(tbl,x){
  if(x %in% names(tbl)){
    return(x)
  }else{
    return(NULL)
  }
}

get_stop_dists <- function(gtfs){
  gtfs$stop_times %>%
    dplyr::left_join(gtfs$trips %>% select('route_id','trip_id', 'direction_id'[verify_field(gtfs$trips,'direction_id')]),by = 'trip_id') %>%
    dplyr::arrange('trip_id','stop_sequence',field_if_exist(.,'direction_id')) %>%
    dplyr::select(route_id,stop_id) %>%
    unique() %>%
    dplyr::left_join(gtfs$stops %>% select(stop_id,stop_lon,stop_lat), by = 'stop_id') %>%
    dplyr::group_by(route_id) %>%
    dplyr::reframe(dists = list(get_trip_stops_dist(stop_lon,stop_lat))) %>%
    .$dists %>% unlist() %>% median(na.rm = TRUE) %>%
    round(1)
}

get_trip_stops_dist <- function(lon,lat){
  coords<-matrix(c(lon,lat),ncol = 2)
  coord_pairs <- cbind(coords[-nrow(coords), ], coords[-1, ])
  # Aplicar distGeo a cada par de coordenadas consecutivas
  distancias <- apply(coord_pairs, 1, function(row) geosphere::distHaversine(row[1:2], row[3:4]))
  return(distancias)
}

check_pkg_all <- function() {
  required_packages <- c("shiny", "plotly", "leaflet", "leaflet.extras", "crayon", "geosphere",
                         "stplanr", "hrbrthemes", "checkmate", "dplyr", "ggplot2", "glue",
                         "gtfsio", "hms", "purrr", "rlang", "sfnetworks", "stringr",
                         "gtfstools", "tidytransit", "lubridate", "sf", "tidyr", "data.table", "tibble")

  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is needed for this function to work. Please install it."))
    }
  })
}

utils::globalVariables(c(
  "agency_id", "agency_name", "arrival_filter", "arrival_time",
  "average.distance", "average.duration", "average.dwelltime",
  "average.headway", "average.speed", "daily.frequency",
  "day_of_month", "departure", "departure_filter", "departure_time",
  "distance", "dupe", "edge_paths", "edges", "end_date", "ends",
  "exception_type", "first_day_of_month", "frequency", "from_stop_id",
  "geometry", "headway", "headway.minutes", "median", "na.omit",
  "name", "name.ends", "name.starts", "net.fleet", "new.trip_id",
  "pattern_frequency", "route_id", "service_id", "service_pattern",
  "setNames", "setTxtProgressBar", "shape_dist_traveled", "shape_id",
  "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "start_date",
  "starts", "stop_id", "stop_lat", "stop_lon", "stop_sequence",
  "subtrip", "time", "to_stop_id", "trip_id", "trips",
  "txtProgressBar", "type", "value", "week_of_month", "weekday",
  "weighted.mean", ".", 'lead_arrival_time',
  "average_headway_minutes", "headway_minutes", "valid_trips",
  'corridor', 'destination', 'group_id', 'origin', 'percent_rank', 'stop_from',
  'stop_to', 'stops', 'stops_sf'
))
