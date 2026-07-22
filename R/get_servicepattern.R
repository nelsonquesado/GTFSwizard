#' Identify Recurring Daily Service Patterns
#'
#' Identifies each distinct set of `service_id` values active on a calendar
#' date. The same service may belong to more than one pattern when it operates
#' alongside different services on different dates.
#'
#' @param gtfs A GTFS object.
#'
#' @return A tibble with one row per service-to-pattern association and columns
#'   `service_id`, `service_pattern`, and `pattern_frequency`. The frequency is
#'   the number of dates having that exact set of active services. Pattern
#'   numbers are ordered from most to least frequent. Calendar dates without
#'   active services are represented by `service_pattern = "No service"` and
#'   `service_id = NA`.
#'
#' @examples
#' get_servicepattern(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::filter_servicepattern()]
#' @export
get_servicepattern <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  service_patterns_for_services(gtfs, include_no_service = TRUE)
}
