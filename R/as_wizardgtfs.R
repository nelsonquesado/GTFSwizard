#' Convert GTFS Object to wizardgtfs Format
#'
#' `as_wizardgtfs` transforms a GTFS object into the `wizardgtfs` format, providing enhanced functionality and compatibility with the GTFSwizard package. This function supports GTFS objects in various formats, including `tidygtfs` and list-based structures, and can optionally create a shapes table if it is missing.
#'
#'
#' @param gtfs_list A GTFS object in list or `tidygtfs` format.
#' @param build_shapes Logical. If `TRUE`, builds the shapes table if it is missing in the provided GTFS object. Default is `TRUE`.
#'
#' @return An object of class `wizardgtfs`, which includes multiple data frames for transit data analysis.
#'
#' @details
#' - `as_wizardgtfs` is a generic function with S3 methods for different GTFS object formats.
#'
#' - The `wizardgtfs` format includes additional processing and checks, such as validation of unique IDs and structure formatting.
#'
#' @examples
#' # Convert a GTFS object to wizardgtfs format
#' gtfs_wizard <- as_wizardgtfs(for_rail_gtfs, build_shapes = TRUE)
#'
#' @seealso
#' [GTFSwizard::get_shapes()]
#'
#' @export
as_wizardgtfs <- function(gtfs_list,build_shapes = TRUE){
  UseMethod('as_wizardgtfs')
}

#' @exportS3Method GTFSwizard::as_wizardgtfs tidygtfs
as_wizardgtfs.tidygtfs <- function(gtfs_list,build_shapes = TRUE){

  checkmate::assert_logical(build_shapes, len = 1, any.missing = FALSE)

  gtfs_list[['dates_services']] <- gtfs_list$.$dates_services %>%
    dplyr::group_by(date) %>%
    reframe(service_id = list(service_id))
  gtfs_list<-gtfs_list[names(gtfs_list)!='.']
  class(gtfs_list) <- c('wizardgtfs','gtfs','list')
  if(build_shapes){
    if('shapes' %in% names(gtfs_obj) == FALSE){
      gtfs_obj <- get_shapes(gtfs_obj)
    }
  }
  return(gtfs_list)
}

#' @exportS3Method GTFSwizard::as_wizardgtfs list
as_wizardgtfs.list <- function(gtfs_list,build_shapes = TRUE){
  duplicate_ids <- has_duplicate_primary(gtfs_list)

  checkmate::assert_logical(build_shapes, len = 1, any.missing = FALSE)

  if(('calendar'%in%names(gtfs_list)==FALSE)&('calendar_dates'%in%names(gtfs_list)==FALSE)){
    ## Tentar formatar com cor e itálico


    warning(crayon::italic(crayon::red(paste0("Can't find calendar nor calendar_dates tables in GTFS files.\nReturning a gtfs object."))))
    return(gtfs_list)


  }
  if(any(unlist(duplicate_ids))){
    warning("Duplicated ids found in: ", paste0(names(duplicate_ids[duplicate_ids]),
                                                collapse = ", "), "\n", crayon::red("The returned object is not a wizardgtfs object."))
    return(gtfs_list)
  }else{
    gtfs_obj <- convert_to_tibble(gtfs_list) %>%
      convert_times_and_dates() %>%
      create_dates_services_table()
    #gtfs_obj <- gtfsio::new_gtfs(gtfs_obj)
    class(gtfs_obj) <- c('wizardgtfs','gtfs','list')
    if(build_shapes){
      if('shapes' %in% names(gtfs_obj) == FALSE){
        gtfs_obj <- get_shapes(gtfs_obj)
      }
    }
    return(gtfs_obj)
  }
}




has_duplicate_primary <- function(gtfs_list){
  duplicated_ids <- as.list(rep(FALSE,length(gtfs_list)))
  names(duplicated_ids) <- names(gtfs_list)
  for (table_name in names(primary_ids())) {
    if(table_name %in% names(duplicated_ids)){
      primary_vec <- gtfs_list[[table_name]] %>% dplyr::select(primary_ids()[table_name])
      if(anyDuplicated(primary_vec)>0){
        duplicated_ids[[table_name]] <- TRUE
      }
    }

  }
  return(unlist(duplicated_ids))
}

primary_ids <- function(){
  c(trips='trip_id',calendar='service_id',routes='route_id',stops='stop_id')
}

convert_to_tibble <- function(x){
  purrr::map(x, as_tibble)
}

convert_times_and_dates <- function(gtfs_list){

  if('calendar'%in%names(gtfs_list)){
    gtfs_list$calendar$start_date <-
      date_to_posixct(gtfs_list$calendar$start_date)
    gtfs_list$calendar$end_date <-
      date_to_posixct(gtfs_list$calendar$end_date)
  }
  if('calendar_dates'%in%names(gtfs_list)){
    gtfs_list$calendar_dates$date <-
      date_to_posixct(gtfs_list$calendar_dates$date)
  }
  if('stop_times'%in%names(gtfs_list)){
    # gtfs_list$stop_times$arrival_time <-
    #   chr_to_hms(gtfs_list$stop_times$arrival_time)
    # gtfs_list$stop_times$departure_time <-
    #   chr_to_hms(gtfs_list$stop_times$departure_time)
  }
  return(gtfs_list)
}

date_to_posixct <- function(x) {
  as.character(x) %>%
    as.POSIXct(tryFormats = c(
      "%Y%m%d",
      "%Y-%m-%d",
      "%Y/%m/%d"
    ))
}

chr_to_segs <- function(x){
  x <- as.numeric(x)
  return(x[1]*60*60+x[2]*60+x[3])
}

chr_to_hms <- function(x){
  x[nchar(x)==0] <- NA
  x <- purrr::map_vec(str_split(x,':'),function(y){
    ifelse(
      sum(is.na(y))==0,
      NA,
      chr_to_segs(y)
    )
  })
  hms::hms(x)
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
