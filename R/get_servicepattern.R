#' Identify Services with the Same Operating Dates
#'
#' Groups `service_id` values that operate on exactly the same set of dates.
#'
#' @param gtfs A GTFS object.
#'
#' @return A tibble with one row per `service_id`, its `service_pattern`, and
#'   `pattern_frequency`, the number of active dates in that pattern. Pattern
#'   numbers are ordered from most to least frequent. Date ranges that include
#'   no active services are represented by `service_pattern = "No service"` and
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
