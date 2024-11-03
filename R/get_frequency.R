#' Calculate Route Frequency in GTFS Data
#'
#' The `get_frequency` function calculates route frequency within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide daily frequencies by route or detailed hourly frequencies.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by.route"}{Calculates the total daily frequency for each route.}
#'     \item{"detailed"}{Calculates the hourly frequency for each route.}
#'   }
#'
#' @return A data frame containing route frequencies based on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{Returns a data frame with columns: `route_id`, `daily.frequency`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `hour`, `frequency`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by.route": Calculates the total daily frequency for each route, summing up the number of trips for each route on a daily basis.
#'
#' - "detailed": Provides an hourly breakdown of frequency, showing the number of departures per hour for each route.
#'
#' If an invalid `method` is specified, the function defaults to `"by.route"` and provides a warning.
#'
#' @examples
#' \dontrun{
#' # Calculate daily route frequency
#' frequency_by_route <- get_frequency(gtfs = for_gtfs, method = "by.route")
#'
#' # Calculate detailed hourly frequency
#' detailed_frequency <- get_frequency(gtfs = for_gtfs, method = "detailed")
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter
#' @importFrom stringr str_extract
#' @export
get_frequency <- function(gtfs, method = 'by.route'){

  if (method == "by.route") {
    freq <- get_frequency_byroute(gtfs)
  }

  if (method == "detailed") {
    freq <- get_frequency_detailed(gtfs)
  }

  if (!method %in% c("by.route", "detailed")) {
    freq <- get_frequency_byroute(gtfs)
    warning('\n"method" should be one of "by.route" or "detailed".\nReturning "method = "by.route"".')
  }

  return(freq)

}

get_frequency_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  freq <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(departure = arrival_time[1]) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = "many-to-many") %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(daily.frequency = n()) %>%
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, daily.frequency, service_pattern, pattern_frequency)

  return(freq)

}

get_frequency_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  freq <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(departure = arrival_time[1]) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = "many-to-many") %>%
    dplyr::mutate(hour = str_extract(as.character(departure), '\\d+')) %>%
    dplyr::group_by(route_id, hour, service_pattern, pattern_frequency) %>%
    dplyr::reframe(frequency = n()) %>%
    #filter(route_id %in% c() & service_id %in% c()) # filtrar por dia e por rota
    dplyr::select(route_id, hour, frequency, service_pattern, pattern_frequency)

  return(freq)

}

