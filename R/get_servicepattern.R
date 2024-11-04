#' Identify Service Patterns in GTFS Data
#'
#' The `get_servicepattern` function identifies and organizes unique service patterns within a `wizardgtfs` object. It groups services by common dates of operation and assigns each a frequency-based pattern identifier.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#'
#' @return A data frame containing unique service patterns with the following columns:
#'   \describe{
#'     \item{`service_id`}{Unique identifier(s) for each service.}
#'     \item{`service_pattern`}{An identifier for each distinct service pattern based on operational dates, in the format "servicepattern-N".}
#'     \item{`pattern_frequency`}{The frequency of each service pattern, indicating the number of dates associated with that pattern.}
#'   }
#'
#' @details
#' The function first checks if the input `gtfs` object is of class `wizardgtfs`. If not, it converts it using `as_wizardgtfs()`. It then groups services by common dates of operation, assigns a frequency to each unique pattern, and organizes these into service pattern identifiers, ordered by their frequency.
#'
#' @examples
#' \dontrun{
#' # Generate service patterns for a GTFS object
#' service_patterns <- get_servicepattern(gtfs = for_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr group_by reframe arrange mutate select
#' @importFrom rlang is_list
#' @export
get_servicepattern <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    warning('\nThis gtfs object is not of the wizardgtfs class.\nComputation may take longer.\nUsing as.gtfswizard() is advised.')
  }

  service_pattern <-
    gtfs$dates_services %>%
    dplyr::group_by(service_id) %>%
    dplyr::reframe(dates = list(as.character(date)),
                   pattern_frequency = n()) %>%
    dplyr::arrange(-pattern_frequency) %>%
    dplyr::mutate(service_pattern = paste0('servicepattern-', 1:n()) %>% as_factor()) %>%
    dplyr::select(service_id, service_pattern, pattern_frequency)


  while(rlang::is_list(service_pattern$service_id)) {
    service_pattern <- service_pattern %>% unnest(., cols = c(service_id))
  }

  return(service_pattern)

}
