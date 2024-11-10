#' Merge Two GTFS Datasets
#'
#' `merge_gtfs` combines two GTFS datasets into a single `wizardgtfs` object, with an option to append suffixes to ensure unique identifiers across tables.
#'
#' @param gtfs.x The first GTFS dataset, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param gtfs.y The second GTFS dataset, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param suffix A logical value. If `TRUE`, appends `.x` and `.y` suffixes to identifier columns in `gtfs.x` and `gtfs.y`, respectively, to prevent conflicts.
#'
#' @return A merged `wizardgtfs` object containing all records from `gtfs.x` and `gtfs.y` across GTFS tables.
#'
#' @details
#' - When `suffix = TRUE`, unique suffixes are appended to key identifiers in `gtfs.x` and `gtfs.y` (e.g., `agency_id`, `route_id`, `trip_id`).
#'
#' - After suffix handling, the function merges individual tables, ensuring no duplicated entries.
#'
#' - Finally, the resulting list is converted into a `wizardgtfs` object.
#'
#' @note
#' This function assumes that both input datasets follow GTFS structure. Non-standard tables or columns may be ignored or cause warnings.
#'
#' @examples
#' # Merge two GTFS datasets with suffix handling
#' merged_gtfs <- merge_gtfs(for_rail_gtfs, for_bus_gtfs, suffix = TRUE)
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr bind_rows distinct mutate left_join
#' @importFrom checkmate assert_logical
#' @importFrom crayon blue
#' @export

merge_gtfs <- function(gtfs.x, gtfs.y, suffix = TRUE){

  # checa a classe ----
  checkmate::assert_logical(suffix)

  if(!"wizardgtfs" %in% class(gtfs.x)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs.x)
    message('The first gtfs (gtfs.x) object is not of the wizardgtfs class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(!"wizardgtfs" %in% class(gtfs.y)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs.y)
    message('The second gtfs (gtfs.y) object is not of the wizardgtfs class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if (isTRUE(suffix)) {

    gtfs.x$agency$agency_id <- paste0(gtfs.x$agency$agency_id, '.x')
    gtfs.y$agency$agency_id <- paste0(gtfs.y$agency$agency_id, '.y')

    gtfs.x$routes$agency_id <- paste0(gtfs.x$routes$agency_id, '.x')
    gtfs.y$routes$agency_id <- paste0(gtfs.y$routes$agency_id, '.y')

    gtfs.x$routes$route_id <- paste0(gtfs.x$routes$route_id, '.x')
    gtfs.y$routes$route_id <- paste0(gtfs.y$routes$route_id, '.y')

    gtfs.x$trips$route_id <- paste0(gtfs.x$trips$route_id, '.x')
    gtfs.y$trips$route_id <- paste0(gtfs.y$trips$route_id, '.y')

    gtfs.x$trips$trip_id <- paste0(gtfs.x$trips$trip_id, '.x')
    gtfs.y$trips$trip_id <- paste0(gtfs.y$trips$trip_id, '.y')

    gtfs.x$trips$shape_id <- paste0(gtfs.x$trips$shape_id, '.x')
    gtfs.y$trips$shape_id <- paste0(gtfs.y$trips$shape_id, '.y')

    gtfs.x$trips$service_id <- paste0(gtfs.x$trips$service_id, '.x')
    gtfs.y$trips$service_id <- paste0(gtfs.y$trips$service_id, '.y')

    gtfs.x$stop_times$trip_id <- paste0(gtfs.x$stop_times$trip_id, '.x')
    gtfs.y$stop_times$trip_id <- paste0(gtfs.y$stop_times$trip_id, '.y')

    gtfs.x$stop_times$stop_id <- paste0(gtfs.x$stop_times$stop_id, '.x')
    gtfs.y$stop_times$stop_id <- paste0(gtfs.y$stop_times$stop_id, '.y')

    gtfs.x$stops$stop_id <- paste0(gtfs.x$stops$stop_id, '.x')
    gtfs.y$stops$stop_id <- paste0(gtfs.y$stops$stop_id, '.y')

    if(!purrr::is_null(gtfs.x$fare_rules$route_id)){
      gtfs.x$fare_rules$route_id <- paste0(gtfs.x$fare_rules$route_id, '.x')
    }

    if(!purrr::is_null(gtfs.x$fare_rules$fare_id)){
      gtfs.x$fare_rules$fare_id <- paste0(gtfs.x$fare_rules$fare_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$fare_rules$route_id)){
      gtfs.y$fare_rules$route_id <- paste0(gtfs.y$fare_rules$route_id, '.y')
    }

    if(!purrr::is_null(gtfs.y$fare_rules$fare_id)){
      gtfs.y$fare_rules$fare_id <- paste0(gtfs.y$fare_rules$fare_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$fare_attributes$fare_id)){
      gtfs.x$fare_attributes$fare_id <- paste0(gtfs.x$fare_attributes$fare_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$fare_attributes$fare_id)){
      gtfs.y$fare_attributes$fare_id <- paste0(gtfs.y$fare_attributes$fare_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$shapes$shape_id)){
      gtfs.x$shapes$shape_id <- paste0(gtfs.x$shapes$shape_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$shapes$shape_id)){
      gtfs.y$shapes$shape_id <- paste0(gtfs.y$shapes$shape_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$calendar$service_id)){
      gtfs.x$calendar$service_id <- paste0(gtfs.x$calendar$service_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$calendar$service_id)){
      gtfs.y$calendar$service_id <- paste0(gtfs.y$calendar$service_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$calendar_dates$service_id)){
      gtfs.x$calendar_dates$service_id <- paste0(gtfs.x$calendar_dates$service_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$calendar_dates$service_id)){
      gtfs.y$calendar_dates$service_id <- paste0(gtfs.y$calendar_dates$service_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$frequencies$trip_id)){
      gtfs.x$frequencies$trip_id <- paste0(gtfs.x$frequencies$trip_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$frequencies$trip_id)){
      gtfs.y$frequencies$trip_id <- paste0(gtfs.y$frequencies$trip_id, '.y')
    }

    if(!purrr::is_null(gtfs.x$transfers$trip_id)){
      gtfs.x$transfers$trip_id <- paste0(gtfs.x$transfers$trip_id, '.x')
    }

    if(!purrr::is_null(gtfs.x$transfers$stop_id)){
      gtfs.x$transfers$stop_id <- paste0(gtfs.x$transfers$stop_id, '.x')
    }

    if(!purrr::is_null(gtfs.y$transfers$trip_id)){
      gtfs.y$transfers$trip_id <- paste0(gtfs.y$transfers$trip_id, '.y')
    }

    if(!purrr::is_null(gtfs.y$transfers$stop_id)){
      gtfs.y$transfers$stop_id <- paste0(gtfs.y$transfers$stop_id, '.y')
    }

  }

  # bind rows ----
  gtfs <- list()

  if(any(!is.null(gtfs.x$agency), !is.null(gtfs.y$agency))){
    gtfs$agency <- dplyr::bind_rows(gtfs.x$agency, gtfs.y$agency) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$routes), !is.null(gtfs.y$routes))){
    gtfs$routes <- dplyr::bind_rows(gtfs.x$routes, gtfs.y$routes) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$trips), !is.null(gtfs.y$trips))){
    gtfs$trips <- dplyr::bind_rows(gtfs.x$trips, gtfs.y$trips) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$stop_times), !is.null(gtfs.y$stop_times))){
    gtfs$stop_times <- dplyr::bind_rows(gtfs.x$stop_times, gtfs.y$stop_times) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$stops), !is.null(gtfs.y$stops))){
    gtfs$stops <- dplyr::bind_rows(gtfs.x$stops, gtfs.y$stops) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$fare_attributes), !is.null(gtfs.y$fare_attributes))){
    gtfs$fare_attributes <- dplyr::bind_rows(gtfs.x$fare_attributes, gtfs.y$fare_attributes) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$fare_rules), !is.null(gtfs.y$fare_rules))){
    gtfs$fare_rules <- dplyr::bind_rows(gtfs.x$fare_rules, gtfs.y$fare_rules) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$shapes), !is.null(gtfs.y$shapes))){
    gtfs$shapes <- dplyr::bind_rows(gtfs.x$shapes, gtfs.y$shapes) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$calendar), !is.null(gtfs.y$calendar))){
    gtfs$calendar <- dplyr::bind_rows(gtfs.x$calendar, gtfs.y$calendar) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$calendar_dates), !is.null(gtfs.y$calendar_dates))){
    gtfs$calendar_dates <- dplyr::bind_rows(gtfs.x$calendar_dates, gtfs.y$calendar_dates) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$dates_services), !is.null(gtfs.y$dates_services))){
    gtfs$dates_services <- dplyr::bind_rows(gtfs.x$dates_services, gtfs.y$dates_services) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$frequencies), !is.null(gtfs.y$frequencies))){
    gtfs$frequencies <- dplyr::bind_rows(gtfs.x$frequencies, gtfs.y$frequencies) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$transfers), !is.null(gtfs.y$transfers))){
    gtfs$transfers <- dplyr::bind_rows(gtfs.x$transfers, gtfs.y$transfers) %>% dplyr::distinct()
  }

  if(any(!is.null(gtfs.x$feed_info), !is.null(gtfs.y$feed_info))){
    gtfs$feed_info <- dplyr::bind_rows(gtfs.x$feed_info, gtfs.y$feed_info) %>% dplyr::distinct()
  }

  # convertendo para 'wizardgts' ----
  gtfs <- GTFSwizard::as_wizardgtfs(gtfs)

  # retornando gtfs ----
  return(gtfs)

}
