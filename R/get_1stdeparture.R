#' Get the First Departure of Each Trip
#'
#' Returns the departure at the lowest `stop_sequence` for every trip.
#'
#' @param gtfs A GTFS object.
#'
#' @return A tibble with `route_id`, `trip_id`, `departure_time`, and
#'   `stop_id`.
#'
#' @examples
#' head(get_1stdeparture(for_rail_gtfs))
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/#stop_timestxt)
#' @export
get_1stdeparture <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  first_calls <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::group_by(trip_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(trip_id, departure_time, stop_id)
  gtfs$trips |>
    dplyr::select(route_id, trip_id) |>
    dplyr::left_join(first_calls, by = "trip_id") |>
    dplyr::select(route_id, trip_id, departure_time, stop_id)
}
