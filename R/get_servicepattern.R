#' Identify Services with the Same Operating Dates
#'
#' Groups `service_id` values that operate on exactly the same set of dates.
#'
#' @param gtfs A GTFS object.
#'
#' @return A tibble with one row per `service_id`, its `service_pattern`, and
#'   `pattern_frequency`, the number of active dates in that pattern. Pattern
#'   numbers are ordered from most to least frequent.
#'
#' @examples
#' get_servicepattern(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::filter_servicepattern()]
#' @export
get_servicepattern <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  service_dates <- tidyr::unnest(gtfs$dates_services, cols = "service_id") |>
    dplyr::arrange(service_id, date) |>
    dplyr::group_by(service_id) |>
    dplyr::summarise(
      .signature = paste(as.character(date), collapse = "|"),
      pattern_frequency = dplyr::n(),
      .groups = "drop"
    )
  patterns <- service_dates |>
    dplyr::distinct(.signature, pattern_frequency) |>
    dplyr::arrange(dplyr::desc(pattern_frequency), .signature) |>
    dplyr::mutate(
      service_pattern = paste0("servicepattern-", dplyr::row_number())
    )
  service_dates |>
    dplyr::left_join(patterns, by = c(".signature", "pattern_frequency")) |>
    dplyr::arrange(service_pattern, service_id) |>
    dplyr::select(service_id, service_pattern, pattern_frequency)
}
